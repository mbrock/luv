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

(defparameter +luvcraft-luft-stock-names+
  #(:turf :granite :sand :terminal :tree :snow :crystal)
  "Luvcraft's fixed LUFT palette, whose vector offset is the scene stock slot.")

(defgeneric block-luft-stock (block)
  (:documentation
   "Return BLOCK's Luvcraft LUFT stock name, or NIL for non-solid air.

This is the open semantic boundary between authored block identity and the
fixed resident-rendering palette.  Dense chunk scans dispatch here once per
world-vocabulary revision and thereafter read a u8 lookup table."))

(defmethod block-luft-stock ((block null))
  nil)

(defmethod block-luft-stock ((block block-kind))
  "Map the current broad block vocabulary through explicit transition aliases."
  (case (block-kind-name block)
    ((:grass :dirt :moss :flowers) :turf)
    ((:stone :gravel :clay :mud :cobblestone :stone-bricks :bricks
      :slate :fountain :lava-spring)
     :granite)
    ((:sand :sandstone) :sand)
    ((:terminal :urbit :tape :film) :terminal)
    ((:wood :leaves :planks :cactus) :tree)
    (:snow :snow)
    ((:crystal :orb-mote) :crystal)
    (otherwise
     (error "Block kind ~S has no Luvcraft LUFT stock mapping."
            (block-kind-name block)))))

(defun make-luvcraft-luft-scene-stocks ()
  "Return the fixed palette after resolving every name through LUFT."
  (let ((stocks (make-array (length +luvcraft-luft-stock-names+))))
    (dotimes (slot (length stocks) stocks)
      (setf (aref stocks slot)
            (luft.render:material-name
             (luft.render:find-material
              (aref +luvcraft-luft-stock-names+ slot)))))))

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
   (stock-lut
    :initform (make-array 0 :element-type '(unsigned-byte 8))
    :accessor luft-world-materialization-stock-lut
    :documentation
    "BLOCK-LUFT-STOCK materialized once per world-vocabulary revision.")
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
   (resident-chunk-stocks
    :initform (make-hash-table :test #'eq)
    :accessor luft-world-materialization-resident-chunk-stocks
    :documentation
    "One dense LUFT stock-slot u8 vector for each resident authored chunk.")
   (resident-solid-counts
    :initform (make-hash-table :test #'eql)
    :accessor luft-world-materialization-resident-solid-counts
    :documentation
    "The number of resident authored solids at each canonical LUFT cell.")
   (resident-stock-counts
    :initform (make-hash-table :test #'eql)
    :accessor luft-world-materialization-resident-stock-counts
    :documentation
    "Exact per-slot resident authored counts at each canonical LUFT cell.")
   (pending-solid-cells
    :initform (make-hash-table :test #'eql)
    :reader luft-world-materialization-pending-solid-cells
    :documentation
    "Canonical LUFT cells dirtied by resident occupancy or stock changes.")
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
  (let* ((domain (luft:make-world-domain :horizontal-bits horizontal-bits))
         (slots
           (make-array (luft:chain-cell-bit-count domain)
                       :element-type '(unsigned-byte 8)
                       :initial-element 0)))
    (make-instance 'luft-world-materialization
                   :world world
                   :domain domain
                   :scene
                   (luft.render:make-scene
                    domain :slots slots :stocks (make-luvcraft-luft-scene-stocks))
                   :horizontal-bits horizontal-bits
                   :vertical-origin vertical-origin)))

(defun invalidate-luft-world-solidity (materialization)
  "Make the next reconciliation reclassify all resident occupancy and stocks.

Call this after changing BLOCK-SOLID-P or BLOCK-LUFT-STOCK semantics.  The
current scene and dense resident products remain readable until a complete
replacement is built and published by RECONCILE-LUFT-WORLD-MATERIALIZATION."
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

(defun make-luft-world-stock-counts ()
  "Return zeroed exact counts for every fixed Luvcraft LUFT stock slot."
  (make-array (length +luvcraft-luft-stock-names+)
              :element-type 'fixnum :initial-element 0))

(defun change-luft-world-count-tables
    (solid-counts stock-counts geometry stock-slot delta)
  "Apply one exact resident DELTA to the parallel canonical count tables."
  (check-type stock-slot (integer 0 6))
  (check-type delta integer)
  (let* ((old-total (gethash geometry solid-counts 0))
         (slot-counts
           (or (gethash geometry stock-counts)
               (make-luft-world-stock-counts)))
         (old-slot-count (aref slot-counts stock-slot))
         (new-total (+ old-total delta))
         (new-slot-count (+ old-slot-count delta)))
    (unless (= old-total (loop for count across slot-counts sum count))
      (error "LUFT resident stock counts for ~S do not sum to ~D."
             geometry old-total))
    (when (or (minusp new-total) (minusp new-slot-count))
      (error "LUFT resident count for cell ~S slot ~D cannot change by ~D."
             geometry stock-slot delta))
    (setf (aref slot-counts stock-slot) new-slot-count)
    (if (zerop new-total)
        (progn
          (remhash geometry solid-counts)
          (remhash geometry stock-counts))
        (setf (gethash geometry solid-counts) new-total
              (gethash geometry stock-counts) slot-counts))))

(defun note-luft-world-cell-count-change
    (materialization x y z stock-slot delta)
  "Apply one authored solid's stock-specific DELTA when its row is valid."
  (multiple-value-bind (geometry valid-p)
      (luft-world-cell-geometry-if-valid materialization x y z)
    (when valid-p
      (change-luft-world-count-tables
       (luft-world-materialization-resident-solid-counts materialization)
       (luft-world-materialization-resident-stock-counts materialization)
       geometry stock-slot delta)
      (setf (gethash geometry
                     (luft-world-materialization-pending-solid-cells
                      materialization))
            t))))

(defun make-luft-world-lut-candidate (materialization)
  "Build complete solidity and stock tables at one vocabulary revision."
  (let* ((vocabulary
           (block-world-vocabulary
            (luft-world-materialization-world materialization)))
         (revision (block-vocabulary-revision vocabulary))
         (members (block-vocabulary-members vocabulary))
         (solid (make-array (length members) :element-type 'bit))
         (stocks (make-array (length members)
                             :element-type '(unsigned-byte 8))))
    (dotimes (offset (length members))
      (let* ((block (aref members offset))
             (solid-p (block-solid-p block))
             (stock (block-luft-stock block)))
        (setf (aref solid offset) (if solid-p 1 0)
              (aref stocks offset)
              (cond ((null stock)
                     (when solid-p
                       (error "Solid block ~S has no Luvcraft LUFT stock."
                              block))
                     0)
                    (t
                     (or
                      (position stock +luvcraft-luft-stock-names+ :test #'eq)
                      (error
                       "Block ~S maps to non-palette Luvcraft LUFT stock ~S."
                       block stock)))))))
    (unless (= revision (block-vocabulary-revision vocabulary))
      (error "Block vocabulary changed while its LUFT lookup tables were built."))
    (values solid stocks revision)))

(defun luft-world-luts (materialization)
  "Return cached BLOCK-SOLID-P and BLOCK-LUFT-STOCK tables at one revision."
  (let* ((vocabulary
           (block-world-vocabulary
            (luft-world-materialization-world materialization)))
         (revision (block-vocabulary-revision vocabulary)))
    (unless (eql revision
                 (luft-world-materialization-solid-lut-revision
                  materialization))
      (multiple-value-bind (solid stocks candidate-revision)
          (make-luft-world-lut-candidate materialization)
        (setf (luft-world-materialization-solid-lut materialization) solid
              (luft-world-materialization-stock-lut materialization) stocks
              (luft-world-materialization-solid-lut-revision materialization)
              candidate-revision)))
    (values (luft-world-materialization-solid-lut materialization)
            (luft-world-materialization-stock-lut materialization))))

(defun map-chunk-luft-cells (function chunk solid stocks)
  "Call FUNCTION with world X,Y,Z, OFFSET, and stock slot for each solid.

SOLID and STOCKS are the paired world-vocabulary lookup tables.  The hot scan
reads specialized indices and these tables directly; it does not perform CLOS
dispatch or construct a coordinate object per cell."
  (check-type function function)
  (check-type chunk block-chunk)
  (with-block-content-storage (domain palette indices) chunk
    (unless (= (length palette) (length solid) (length stocks))
      (error "Chunk palette and LUFT lookup tables disagree: ~D, ~D, and ~D."
             (length palette) (length solid) (length stocks)))
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
            (let ((block-offset (aref indices offset)))
              (when (= 1 (aref solid block-offset))
                (funcall function
                         (+ origin-x local-x)
                         (+ origin-y local-y)
                         (+ origin-z local-z)
                         offset
                         (aref stocks block-offset))))
            (incf offset))))))
  chunk)

(defstruct (luft-world-solidity-candidate
             (:constructor %make-luft-world-solidity-candidate
                 (lut-revision solid-lut stock-lut
                  resident-chunk-solids resident-chunk-stocks
                  resident-solid-counts resident-stock-counts)))
  "A complete unpublished replacement for resident geometry and stocks."
  lut-revision
  solid-lut
  stock-lut
  resident-chunk-solids
  resident-chunk-stocks
  resident-solid-counts
  resident-stock-counts)

(defun add-luft-world-candidate-solid
    (materialization solid-counts stock-counts x y z stock-slot)
  "Add one resident solid to unpublished canonical count products."
  (multiple-value-bind (geometry valid-p)
      (luft-world-cell-geometry-if-valid materialization x y z)
    (when valid-p
      (change-luft-world-count-tables
       solid-counts stock-counts geometry stock-slot 1))))

(defun build-luft-world-solidity-candidate (materialization)
  "Build complete resident occupancy and stock state off-side."
  (let* ((world (luft-world-materialization-world materialization))
         (world-revision (block-world-revision world))
         (residency-revision (block-world-residency-revision world)))
    (multiple-value-bind (solid stocks revision)
        (make-luft-world-lut-candidate materialization)
      (let ((resident-solids (make-hash-table :test #'eq))
            (resident-stocks (make-hash-table :test #'eq))
            (solid-counts (make-hash-table :test #'eql))
            (stock-counts (make-hash-table :test #'eql)))
        (dolist (chunk
                 (resident-world-chunks world))
          (let* ((cardinality
                   (chunk-domain-cardinality (block-chunk-domain chunk)))
                 (bits
                   (make-array cardinality :element-type 'bit
                                           :initial-element 0))
                 (chunk-stocks
                   (make-array cardinality :element-type '(unsigned-byte 8)
                                           :initial-element 0)))
            (map-chunk-luft-cells
             (lambda (x y z offset stock-slot)
               (setf (aref bits offset) 1
                     (aref chunk-stocks offset) stock-slot)
               (add-luft-world-candidate-solid
                materialization solid-counts stock-counts
                x y z stock-slot))
             chunk solid stocks)
            (setf (gethash chunk resident-solids) bits
                  (gethash chunk resident-stocks) chunk-stocks)))
        (unless (= residency-revision (block-world-residency-revision world))
          (error "Block-world residency changed during LUFT solidity rebuild."))
        (unless (= world-revision (block-world-revision world))
          (error "Block-world content changed during LUFT solidity rebuild."))
        (%make-luft-world-solidity-candidate
         revision solid stocks resident-solids resident-stocks
         solid-counts stock-counts)))))

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
        (luft-world-solidity-candidate-solid-lut candidate)
        (luft-world-materialization-stock-lut materialization)
        (luft-world-solidity-candidate-stock-lut candidate)
        (luft-world-materialization-resident-chunk-solids materialization)
        (luft-world-solidity-candidate-resident-chunk-solids candidate)
        (luft-world-materialization-resident-chunk-stocks materialization)
        (luft-world-solidity-candidate-resident-chunk-stocks candidate)
        (luft-world-materialization-resident-solid-counts materialization)
        (luft-world-solidity-candidate-resident-solid-counts candidate)
        (luft-world-materialization-resident-stock-counts materialization)
        (luft-world-solidity-candidate-resident-stock-counts candidate))
  materialization)

(defun queue-luft-world-chunk (materialization chunk resident-p)
  "Add or remove CHUNK's dense solidity and stock products."
  (let ((resident-solids
          (luft-world-materialization-resident-chunk-solids materialization))
        (resident-stocks
          (luft-world-materialization-resident-chunk-stocks materialization)))
    (if resident-p
        (progn
          (when (or (nth-value 1 (gethash chunk resident-solids))
                    (nth-value 1 (gethash chunk resident-stocks)))
            (error "LUFT materialization already contains resident chunk ~S."
                   chunk))
          (multiple-value-bind (solid stocks)
              (luft-world-luts materialization)
            (let* ((cardinality
                     (chunk-domain-cardinality (block-chunk-domain chunk)))
                   (bits
                     (make-array cardinality :element-type 'bit
                                             :initial-element 0))
                   (chunk-stocks
                     (make-array cardinality
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
              (map-chunk-luft-cells
               (lambda (x y z offset stock-slot)
                 (setf (aref bits offset) 1
                       (aref chunk-stocks offset) stock-slot)
                 (note-luft-world-cell-count-change
                  materialization x y z stock-slot 1))
               chunk solid stocks)
              (setf (gethash chunk resident-solids) bits
                    (gethash chunk resident-stocks) chunk-stocks))))
        (multiple-value-bind (bits solids-present-p)
            (gethash chunk resident-solids)
          (multiple-value-bind (chunk-stocks stocks-present-p)
              (gethash chunk resident-stocks)
            (unless (and solids-present-p stocks-present-p)
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
                     (aref chunk-stocks offset)
                     -1)))))
            (remhash chunk resident-solids)
            (remhash chunk resident-stocks)))))
  chunk)

(defun queue-luft-world-cell
    (materialization chunk x y z solid-p stock-slot)
  "Update one resident chunk cell's cached solidity and LUFT stock."
  (multiple-value-bind (bits present-p)
      (gethash chunk
               (luft-world-materialization-resident-chunk-solids
                materialization))
    (unless present-p
      (error "LUFT materialization does not contain edited chunk ~S." chunk))
    (multiple-value-bind (chunk-stocks stocks-present-p)
        (gethash chunk
                 (luft-world-materialization-resident-chunk-stocks
                  materialization))
      (unless stocks-present-p
        (error "LUFT materialization has no stock vector for edited chunk ~S."
               chunk))
      (let* ((origin (chunk-domain-origin (block-chunk-domain chunk)))
             (local-x (- x (world-coordinate-x origin)))
             (local-y (- y (world-coordinate-y origin)))
             (local-z (- z (world-coordinate-z origin)))
             (offset
               (chunk-domain-offset-components
                (block-chunk-domain chunk) local-x local-y local-z))
             (desired-solid (if solid-p 1 0))
             (desired-stock (if solid-p stock-slot 0))
             (current-solid (aref bits offset))
             (current-stock (aref chunk-stocks offset)))
        (unless (and (= desired-solid current-solid)
                     (= desired-stock current-stock))
          (when (= 1 current-solid)
            (note-luft-world-cell-count-change
             materialization x y z current-stock -1))
          (setf (aref bits offset) desired-solid
                (aref chunk-stocks offset) desired-stock)
          (when (= 1 desired-solid)
            (note-luft-world-cell-count-change
             materialization x y z desired-stock 1))
          ;; Count changes already dirtied solid transitions.  Keep the direct
          ;; mark as the proof that solid-to-solid stock changes queue too.
          (multiple-value-bind (geometry valid-p)
              (luft-world-cell-geometry-if-valid materialization x y z)
            (when valid-p
              (setf (gethash
                     geometry
                     (luft-world-materialization-pending-solid-cells
                      materialization))
                    t)))))))
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
    (let* ((origin (chunk-domain-origin (block-chunk-domain chunk)))
           (block
             (chunk-block-at chunk
                             (- x (world-coordinate-x origin))
                             (- y (world-coordinate-y origin))
                             (- z (world-coordinate-z origin))))
           (vocabulary (block-world-vocabulary world))
           (block-offset (block-vocabulary-offset vocabulary block nil)))
      (unless block-offset
        (error "Edited block ~S is absent from its world's vocabulary." block))
      (multiple-value-bind (solid stocks)
          (luft-world-luts materialization)
        (queue-luft-world-cell
         materialization chunk x y z
         (= 1 (aref solid block-offset))
         (aref stocks block-offset)))))
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
       (queue-luft-world-chunk materialization chunk t))
      (:departed
       ;; A departed chunk is deliberately still readable to observers.
       (queue-luft-world-chunk materialization chunk nil))))
  materialization)

(defun attach-luft-world-materialization
    (world &key (horizontal-bits 8) (vertical-origin 0))
  "Subscribe a fresh LUFT materialization and queue resident cells and stocks."
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
             (queue-luft-world-chunk materialization chunk t))
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

(defun resident-luft-world-stock-slot
    (solid-counts stock-counts geometry)
  "Return GEOMETRY's unique resident stock slot, or zero for resident air."
  (let ((total (gethash geometry solid-counts 0)))
    (when (zerop total)
      (return-from resident-luft-world-stock-slot 0))
    (let ((counts
            (or (gethash geometry stock-counts)
                (error "Resident solid cell ~S has no LUFT stock counts."
                       geometry)))
          (sum 0)
          (slot 0)
          (found-p nil))
      (dotimes (candidate (length counts))
        (let ((count (aref counts candidate)))
          (incf sum count)
          (when (plusp count)
            (when found-p
              (error "Resident aliases give LUFT cell ~S several stocks."
                     geometry))
            (setf slot candidate
                  found-p t))))
      (unless (and found-p (= sum total))
        (error "Resident LUFT stock counts for ~S do not total ~D."
               geometry total))
      slot)))

(defun scene-luft-world-stock-slot (scene geometry)
  "Return SCENE's dense stock slot at canonical cell GEOMETRY."
  (aref (luft.render:scene-slots scene)
        (luft:cell-bit-index
         (luft.render:scene-domain scene)
         (luft:site-x geometry)
         (luft:site-y geometry)
         (luft:site-z geometry))))

(defun make-luft-world-stock-edit-vectors (edits)
  "Return parallel simple u64 cell and u8 slot vectors for EDITS."
  (let* ((count (length edits))
         (cells (make-array count :element-type '(unsigned-byte 64)))
         (slots (make-array count :element-type '(unsigned-byte 8))))
    (loop for (geometry . slot) in edits
          for index from 0
          do (setf (aref cells index) geometry
                   (aref slots index) slot))
    (values cells slots)))

(defun reconcile-luft-world-materialization (materialization)
  "Publish all pending authored occupancy and stocks as one exact LUFT edit.

Observer events maintain exact resident-solid and per-stock counts at canonical
LUFT cells; PENDING-SOLID-CELLS only says which fields must be compared with
the scene.  Thus temporary aliases and differing-stock replacements cannot
erase or repaint an unchanged resident solid.  Invalidated classification is
rebuilt as a complete candidate and swapped in only after its one exact scene
edit succeeds.  Returns the scene and the changed canonical-cell union count."
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
           (resident-stock-counts
             (if candidate
                 (luft-world-solidity-candidate-resident-stock-counts candidate)
                 (luft-world-materialization-resident-stock-counts
                  materialization)))
           (dirty
             (if candidate
                 (luft-world-rebuild-dirty-cells scene candidate)
                 pending)))
      (let ((edit (luft:make-chain domain))
            (stock-edits nil)
            (changed 0))
        (maphash
         (lambda (geometry dirty-p)
           (declare (ignore dirty-p))
           (let* ((desired-solid-p
                    (plusp (gethash geometry resident-counts 0)))
                  (desired-stock
                    (if desired-solid-p
                        (resident-luft-world-stock-slot
                         resident-counts resident-stock-counts geometry)
                        0))
                  (current-solid-p
                    (luft.render:scene-cell-p
                     scene
                     (luft:site-x geometry)
                     (luft:site-y geometry)
                     (luft:site-z geometry)))
                  (current-stock
                    (scene-luft-world-stock-slot scene geometry))
                  (solid-changed-p
                    (not (eql (not (null desired-solid-p))
                              (not (null current-solid-p)))))
                  (stock-changed-p (/= desired-stock current-stock)))
             (when solid-changed-p
               (luft:add-chain-site
                edit
                (luft:site-with-polarity
                 geometry (if desired-solid-p 1 -1))))
             (when stock-changed-p
               (push (cons geometry desired-stock) stock-edits))
             (when (or solid-changed-p stock-changed-p)
               (incf changed))))
         dirty)
        (when (plusp changed)
          ;; One reconciliation is one exact revision, regardless of how many
          ;; authored edits, stock changes, or transitions it coalesced.
          (multiple-value-bind (stock-cells stock-slots)
              (make-luft-world-stock-edit-vectors stock-edits)
            (luft.render:apply-scene-edit
             scene edit :stock-cells stock-cells :stock-slots stock-slots)))
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
