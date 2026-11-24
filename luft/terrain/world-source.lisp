(in-package #:luft.terrain)

;;; A world source is a deterministic description of a large world plus its
;;; sparse semantic edits.  A game says which cells a chunk generates; the
;;; terrain materializes immutable resident chunk values from that answer on
;;; a streaming worker, replays the edits, and assembles occupancy fibers.

(defconstant +authored-world-gameplay-radius+ 1
  "Chunks around the focus whose occupancy stays resident for collision.")

(defclass world-source ()
  ((domain :initarg :domain :reader world-source-domain)
   (material-vocabulary :initarg :material-vocabulary
                        :reader world-source-material-vocabulary)
   (journal :initform (make-world-journal) :reader world-source-journal))
  (:documentation
   "A deterministic world in one placement vocabulary, with a journal of edits."))

(defgeneric world-source-baseline-value (source placement)
  (:documentation
   "The journal value of a cell SOURCE generated as PLACEMENT, NIL for air.

A game whose journal names blocks by its own identities (#FBU3TF) answers
those here, so a generated cell and an authored one compare alike.")
  (:method ((source world-source) placement)
    placement))

(defun world-source-edits (source)
  "SOURCE's sparse edits: the latest value its journal gives each authored site.

This is a view of the journal (#3F0SCZ); record events rather than change it."
  (world-journal-edits (world-source-journal source)))

(defgeneric generate-world-source-chunk (source key place)
  (:documentation
   "Call PLACE with X, Y, Z, and a placement for every solid cell SOURCE
generates in chunk KEY.  A later call for the same cell wins.  Runs on a
worker, so it must read only SOURCE's immutable description."))

(defstruct (resident-cell-chunk
             (:constructor %make-resident-cell-chunk
                 (key incarnation fibers material-cells &optional attachments))
             (:copier nil))
  "One immutable, evictable materialization of an authored source chunk.

ATTACHMENTS are the surface attachments its fixture cells became."
  (key 0 :type luft:chunk-key :read-only t)
  (incarnation 0 :type (integer 1 *) :read-only t)
  (fibers nil :type luft:chunk-fibers :read-only t)
  (material-cells nil :type (or chunk-materials hash-table) :read-only t)
  (attachments nil :type list :read-only t))

(defgeneric world-source-spawn (source)
  (:documentation
   "Where a player arriving in SOURCE's world stands: cell X and Y, the height
Z of the ground under its feet, and the yaw it faces, as four values.")
  (:method ((source world-source))
    (let ((domain (world-source-domain source)))
      (values (floor (luft:world-domain-x-limit domain) 2)
              (floor (luft:world-domain-y-limit domain) 2)
              0 0.0))))

(defun make-world-source-streaming-scene
    (source &key (frames-per-load 1) (residency-radius 1) mesh-preparer)
  "Make a demand-streamed scene over SOURCE without materializing any chunk.

The scene paints in SOURCE's own vocabulary, so the offsets a worker
materializes and the descriptors the scene compiles name the same palette."
  (let* ((domain (world-source-domain source))
         (vocabulary (world-source-material-vocabulary source))
         (builder (make-scene-builder
                   :horizontal-bits (luft:world-domain-x-bits domain)
                   :material-vocabulary vocabulary))
         ;; Its light is regional (#BQMYNU): no whole-world field exists.
         (empty (finish-scene-builder
                 builder :voxel-light-propagation-p nil)))
    (unless (= (luft:world-domain-x-bits domain) (luft:world-domain-y-bits domain))
      (error "A streamed world source needs a square domain, not ~S." domain))
    (make-instance
     'streaming-scene
     :source source
     :journal (world-source-journal source)
     :solid (luft:make-fiber-store domain)
     :material-vocabulary vocabulary
     :material-cells (make-material-store)
     :authored-light-sources #()
     :authored-light-opacity-table
     (scene-authored-light-opacity-table empty)
     :authored-light-revision 0
     :authored-light-provenance (scene-authored-light-provenance empty)
     :authored-light-generation (scene-authored-light-generation empty)
     :content-revision 0
     :voxel-light-propagation-p t
     :light-generation (scene-authored-light-generation empty)
     :light-tiles (make-hash-table :test #'eql)
     :attachments #()
     :frames-per-load frames-per-load
     :residency-radius residency-radius
     :mesh-preparer mesh-preparer)))

(defun world-source-edit-at (source cell)
  "Return a sparse edited placement and whether SOURCE owns an edit at CELL."
  (gethash cell (world-source-edits source)))

(defun capture-world-source-chunk-edits (source key)
  "Copy the sparse edits belonging to KEY, as placements, for immutable worker use."
  (let ((edits nil))
    (maphash
     (lambda (cell value)
       (when (= key (luft:site-chunk-key cell))
         (push (cons cell (world-value-placement value)) edits)))
     (world-source-edits source))
    edits))

;;; A fixture is a cell whose material has an attachment form: a lantern is a
;;; block in Luvcraft's world but a small drawn body here.  It stands on the
;;; solid cell below it, else hangs from the one above; a fixture is no
;;; support, and one with no support in its column stays a cube.  Its own cell
;;; becomes the attachment's clearance.

(defun realize-fixture-cells (domain vocabulary materials)
  "Turn MATERIALS' supported fixture cells into attachments, in face order."
  (let ((members (domains:identity-vocabulary-members vocabulary))
        (fixtures (make-hash-table :test #'eql))
        (attachments nil))
    (flet ((kind-at (cell)
             (material-placement-kind (aref members (gethash cell materials)))))
      (maphash (lambda (cell offset)
                 (declare (ignore offset))
                 (when (material-kind-attachment-form (kind-at cell))
                   (setf (gethash cell fixtures) (kind-at cell))))
               materials)
      (flet ((support-p (site)
               (and site (nth-value 1 (gethash site materials))
                    (not (gethash site fixtures)))))
        (maphash
         (lambda (cell kind)
           (let ((below (luft:step-site domain cell :z -1))
                 (above (luft:step-site domain cell :z 1)))
             (multiple-value-bind (support face)
                 (cond ((support-p below)
                        (values below (luft:site-boundary-high domain below :z)))
                       ((support-p above)
                        (values above (luft:site-boundary-low domain above :z))))
               (when support
                 (push (make-instance 'surface-attachment
                                      :support-cell support :face face
                                      :clearance-cell cell :material kind)
                       attachments)))))
         fixtures))
      (dolist (attachment attachments)
        (remhash (surface-attachment-clearance-cell attachment) materials))
      (sort attachments #'< :key #'surface-attachment-face))))

(defun realize-grid-fixture-cells
    (domain key vocabulary fixture-kinds grid fixture-columns max-z)
  "Realize supported fixture cells directly from GRID, clearing their cells."
  (declare (type luft:chunk-key key)
           (type simple-vector fixture-kinds)
           (type (simple-array (unsigned-byte 16) (*)) grid)
           (type simple-bit-vector fixture-columns)
           (type fixnum max-z))
  (declare (ignore vocabulary))
  (let ((attachments nil)
        (cleared nil)
        (z-limit (min luft:+top-z+ (1+ max-z))))
    (flet ((support-p (base z)
             (and (<= 0 z) (< z luft:+top-z+)
                  (let ((mat (aref grid (+ base z))))
                    (and (/= mat +material-air-sentinel+)
                         (null (aref fixture-kinds mat)))))))
      (dotimes (col +material-chunk-column-count+)
        (when (= 1 (sbit fixture-columns col))
          (let ((base (ash col 8)))
            (dotimes (z z-limit)
              (let* ((index (+ base z))
                     (mat (aref grid index))
                     (kind (and (/= mat +material-air-sentinel+)
                                (aref fixture-kinds mat))))
                (when kind
                  (multiple-value-bind (support face)
                      (cond ((support-p base (1- z))
                             (let ((below (material-column-site key col (1- z))))
                               (values below (luft:site-boundary-high domain below :z))))
                            ((support-p base (1+ z))
                             (let ((above (material-column-site key col (1+ z))))
                               (values above (luft:site-boundary-low domain above :z)))))
                    (when support
                      (push (make-instance 'surface-attachment
                                           :support-cell support :face face
                                           :clearance-cell (material-column-site key col z)
                                           :material kind)
                            attachments)
                      (push index cleared))))))))))
    (dolist (index cleared)
      (setf (aref grid index) +material-air-sentinel+))
    (sort attachments #'< :key #'surface-attachment-face)))

(defun %set-fiber-run-bits (fiber-words fiber-base z0 z1)
  (declare (type (simple-array (unsigned-byte 64) (*)) fiber-words)
           (type fixnum fiber-base z0 z1)
           (optimize (speed 3) (safety 1)))
  (dotimes (word 4)
    (let* ((word-start (ash word 6))
           (lo (max z0 word-start))
           (hi (min z1 (+ word-start 64))))
      (when (< lo hi)
        (let* ((bit-lo (- lo word-start))
               (width (- hi lo))
               (mask (if (= width 64)
                         #xffffffffffffffff
                         (ash (1- (ash 1 width)) bit-lo))))
          (setf (aref fiber-words (+ fiber-base word))
                (logior (aref fiber-words (+ fiber-base word)) mask)))))))

(defun %compress-chunk-grid (domain key grid max-z)
  "Build immutable CHUNK-FIBERS and CHUNK-MATERIALS from GRID in one linear pass."
  (declare (type luft:chunk-key key)
           (type (simple-array (unsigned-byte 16) (*)) grid)
           (type fixnum max-z)
           (optimize (speed 3) (safety 1)))
  (let* ((fibers (luft:make-chunk-fibers domain key))
         (fiber-words (luft:chunk-fibers-words fibers))
         (starts (make-array (1+ +material-chunk-column-count+)
                             :element-type '(unsigned-byte 32)
                             :initial-element 0))
         (runs (make-array 4096 :element-type '(unsigned-byte 32)
                                :adjustable t :fill-pointer 0))
         (cell-count 0)
         (z-limit (min luft:+top-z+ (1+ max-z))))
    (declare (type fixnum cell-count z-limit))
    (dotimes (col +material-chunk-column-count+)
      (setf (aref starts col) (fill-pointer runs))
      (when (plusp z-limit)
        (let* ((lx (logand col 63))
               (ly (ash col -6))
               (fiber-base (ash (+ ly (ash lx 6)) 2))
               (base (ash col 8))
               (z 0))
          (declare (type fixnum lx ly fiber-base base z))
          (loop while (< z z-limit)
                for mat of-type (unsigned-byte 16) = (aref grid (+ base z))
                do (if (= mat +material-air-sentinel+)
                       (incf z)
                       (let ((z0 z))
                         (incf z)
                         (loop while (and (< z z-limit)
                                          (= (aref grid (+ base z)) mat))
                               do (incf z))
                         (incf cell-count (- z z0))
                         (%set-fiber-run-bits fiber-words fiber-base z0 z)
                         (vector-push-extend (pack-material-run z0 z mat) runs)))))))
    (setf (aref starts +material-chunk-column-count+) (fill-pointer runs))
    (let ((packed (make-array (fill-pointer runs) :element-type '(unsigned-byte 32))))
      (replace packed runs)
      (values fibers (%make-chunk-materials key cell-count starts packed)))))

(zdefun (materialize-world-source-chunk :zone :luft/materialize-source-chunk
                                        :value key)
    (source key incarnation &key edits)
  "Build KEY bit-identically from SOURCE and an immutable sparse edit capture."
  (check-type source world-source)
  (let* ((domain (world-source-domain source))
         (vocabulary (world-source-material-vocabulary source))
         (members (domains:identity-vocabulary-members vocabulary))
         (fixture-kinds
           (map 'simple-vector
                (lambda (placement)
                  (let ((kind (material-placement-kind placement)))
                    (and (material-kind-attachment-form kind) kind)))
                members))
         (x0 (luft:chunk-origin-x key))
         (y0 (luft:chunk-origin-y key))
         (x-end (min (+ x0 luft:+chunk-size+) (luft:world-domain-x-limit domain)))
         (y-end (min (+ y0 luft:+chunk-size+) (luft:world-domain-y-limit domain)))
         (grid (make-array (* +material-chunk-column-count+ 256)
                           :element-type '(unsigned-byte 16)
                           :initial-element +material-air-sentinel+))
         (fixture-columns (make-array +material-chunk-column-count+
                                      :element-type 'bit :initial-element 0))
         (any-fixture-p nil)
         (max-z -1)
         (offsets (make-hash-table :test #'eq)))
    (declare (type fixnum x0 y0 x-end y-end max-z))
    (flet ((place (x y z placement)
             (declare (type fixnum x y z))
             (unless (and (<= x0 x) (< x x-end)
                          (<= y0 y) (< y y-end)
                          (<= 0 z) (< z luft:+top-z+))
               (error "Cell (~D ~D ~D) lies outside chunk ~D in ~S." x y z key domain))
             (let* ((offset (or (gethash placement offsets)
                                (setf (gethash placement offsets)
                                      (domains:identity-vocabulary-offset
                                       vocabulary placement))))
                    (col (material-column-index (- x x0) (- y y0))))
               (declare (type (unsigned-byte 16) offset)
                        (type (integer 0 4095) col))
               (setf (aref grid (+ (ash col 8) z)) offset)
               (when (> z max-z) (setf max-z z))
               (when (aref fixture-kinds offset)
                 (setf (sbit fixture-columns col) 1
                       any-fixture-p t)))))
      (zone :luft/generate-source-materials
        (generate-world-source-chunk source key #'place)))
    (zone :luft/replay-source-edits
      (dolist (edit edits)
        (let* ((cell (car edit))
               (col (ldb (byte (* 2 luft:+chunk-bits+) luft:+x-local-shift+) cell))
               (z (ldb (byte luft:+vertical-coordinate-bits+ luft:+z-shift+) cell))
               (index (+ (ash col 8) z)))
          (if (cdr edit)
              (let ((offset (domains:identity-vocabulary-offset vocabulary (cdr edit))))
                (setf (aref grid index) offset)
                (when (> z max-z) (setf max-z z))
                (when (aref fixture-kinds offset)
                  (setf (sbit fixture-columns col) 1
                        any-fixture-p t)))
              (setf (aref grid index) +material-air-sentinel+)))))
    (let ((attachments
            (when any-fixture-p
              (realize-grid-fixture-cells
               domain key vocabulary fixture-kinds grid fixture-columns max-z))))
      (multiple-value-bind (fibers materials)
          (zone :luft/assemble-source-fibers
            (%compress-chunk-grid domain key grid max-z))
        ;; Find the chunk's emitters here, on the worker, before the owner
        ;; thread asks which tiles its arrival may light.
        (chunk-emitter-sources materials vocabulary)
        ;; Direct sky and interior interfaces depend only on this chunk; prepare
        ;; them with materialization rather than on the mesh cohort's path.
        (chunk-sky-interfaces materials (compile-material-light-opacity-table vocabulary) domain)
        (%make-resident-cell-chunk key incarnation fibers materials attachments)))))

(defclass authored-chunk-load-request (production:production-request)
  ((scene :initarg :scene :reader authored-chunk-load-request-scene)
   (source :initarg :source :reader authored-chunk-load-request-source)
   (chunk-key :initarg :chunk-key :reader authored-chunk-load-request-chunk-key)
   (demand-token :initarg :demand-token
                 :reader authored-chunk-load-request-demand-token)
   (incarnation :initarg :incarnation
                :reader authored-chunk-load-request-incarnation)
   (edits :initarg :edits :reader authored-chunk-load-request-edits)))

(zdefmethod (production:perform-production-request
             :zone :luft/produce-source-chunk)
    ((request authored-chunk-load-request))
  (materialize-world-source-chunk
   (authored-chunk-load-request-source request)
   (authored-chunk-load-request-chunk-key request)
   (authored-chunk-load-request-incarnation request)
   :edits (authored-chunk-load-request-edits request)))
