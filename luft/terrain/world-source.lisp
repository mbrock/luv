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
   ;; Presence is meaningful: NIL is an authored removal, while absence means
   ;; that the deterministic source still owns the cell.
   (edits :initform (make-hash-table :test #'eql)
          :reader world-source-edits))
  (:documentation
   "A deterministic world in one placement vocabulary, with sparse edits."))

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
  (material-cells nil :type hash-table :read-only t)
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
  "Copy the sparse edits belonging to KEY for immutable worker use."
  (let ((edits nil))
    (maphash
     (lambda (cell placement)
       (when (= key (luft:site-chunk-key cell))
         (push (cons cell placement) edits)))
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

(zdefun (materialize-world-source-chunk :zone :luft/materialize-source-chunk
                                        :value key)
    (source key incarnation &key edits)
  "Build KEY bit-identically from SOURCE and an immutable sparse edit capture."
  (check-type source world-source)
  (let* ((domain (world-source-domain source))
         (vocabulary (world-source-material-vocabulary source))
         (materials (make-hash-table :test #'eql :size 100000))
         (offsets (make-hash-table :test #'eq)))
    (flet ((place (x y z placement)
             (setf (gethash (luft:make-site domain x y z luft:+cell-extent+ 1)
                            materials)
                   (or (gethash placement offsets)
                       (setf (gethash placement offsets)
                             (domains:identity-vocabulary-offset
                              vocabulary placement))))))
      (zone :luft/generate-source-materials
        (generate-world-source-chunk source key #'place)))
    (zone :luft/replay-source-edits
      (dolist (edit edits)
        (if (cdr edit)
            (setf (gethash (car edit) materials)
                  (domains:identity-vocabulary-offset vocabulary (cdr edit)))
            (remhash (car edit) materials))))
    (let ((fibers (luft:make-chunk-fibers domain key))
          (attachments (realize-fixture-cells domain vocabulary materials)))
      (zone :luft/assemble-source-fibers
        (maphash (lambda (cell offset)
                   (declare (ignore offset))
                   (setf (luft:fibers-cell-bit
                          fibers (luft:site-x cell) (luft:site-y cell)
                          (luft:site-z cell))
                         1))
                 materials))
      ;; Find the chunk's emitters here, on the worker, before the owner
      ;; thread asks which tiles its arrival may light.
      (chunk-emitter-sources materials vocabulary)
      (%make-resident-cell-chunk key incarnation fibers materials attachments))))

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
