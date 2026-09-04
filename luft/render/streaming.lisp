(in-package #:luft.render)

;;; Streaming chunk scenes
;;;
;;; A finite fixture may still split an ordinary authored scene into chunks.
;;; Ordinary play instead retains a deterministic source and a sparse overlay;
;;; immutable resident values are produced on the worker and may be discarded.
;;; A bounded square window follows the camera. Each focus change installs the
;;; final desired residency first, then remeshes exactly the chunks whose 3 by 3
;;; dependency neighborhoods changed. MESH-STAR-CHUNK's probes into non-resident
;;; neighbors signal MISSING-CHUNK. Demand worlds capture a complete guard and
;;; reject an unknown probe rather than silently turning nonresidency into air.
;;; The canvas owner publishes replacements and departures as one complete
;;; cohort, so no frame observes a mixed seam generation.


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
   (mesh-cache :initform nil :accessor streaming-scene-mesh-cache)
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
                  realize-torch-light-p reusable-light-generation
                  &optional mesh-cache)))
  "Immutable CPU input for one dependency-closed regional mesh request."
  (scene nil :read-only t)
  ;; The owning streaming scene remains mutable on the canvas thread.  Workers
  ;; borrow this frozen scene value so a later edit cannot mix new materials or
  ;; light with the snapshot's old occupancy fibers.
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
                             :read-only t)
  (mesh-cache nil :type list :read-only t))

(defstruct (streaming-star-product
             (:constructor make-streaming-star-product
                 (key signature descriptors prepared))
             (:copier nil))
  "Immutable CPU product; GPU residency and light publication are independent.
#WQCMA3"
  (key nil :read-only t)
  (signature nil :read-only t)
  (descriptors nil :read-only t)
  (prepared nil :read-only t))

(defclass streaming-mesh-request (production:production-request)
  ((snapshot :initarg :snapshot :reader streaming-mesh-request-snapshot)))

(defstruct (streaming-mesh-result
             (:constructor %make-streaming-mesh-result
                 (meshes generation &optional mesh-cache))
             (:copier nil))
  "One worker-complete prepared owner cohort and its exact light generation."
  (meshes nil :type list :read-only t)
  (generation nil :type scene-mesh-generation :read-only t)
  (mesh-cache nil :type list :read-only t))

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
    (luft:map-fiber-store
     (lambda (key fibers)
       (setf (gethash key (streaming-scene-store streaming)) fibers))
     (scene-solid scene))
    streaming))

(defun make-authored-world-streaming-scene
    (&key (horizontal-bits +large-world-horizontal-bits+)
      (seed +large-world-seed+) (frames-per-load 1) (residency-radius 1))
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
     :solid (luft:make-fiber-store domain)
     :material-vocabulary (scene-material-vocabulary empty)
     :material-cells (make-material-store)
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

(defun streaming-store-fibers (scene key &optional default)
  "Return KEY's fibers from either a finite fixture or resident source value."
  (multiple-value-bind (value present-p)
      (gethash key (streaming-scene-store scene))
    (values
     (if present-p
         (etypecase value
           (luft:chunk-fibers value)
           (resident-cell-chunk (resident-cell-chunk-fibers value)))
         default)
     present-p)))

(defun streaming-store-incarnation (scene key)
  (let ((value (gethash key (streaming-scene-store scene))))
    (and (resident-cell-chunk-p value)
         (resident-cell-chunk-incarnation value))))

(defun streaming-scene-cell-state (scene x y z)
  "Classify a cell without conflating sky, finite boundary, and nonresidency."
  (let ((domain (scene-domain scene)))
    (if (or (< x 0) (>= x (luft:world-domain-x-limit domain))
            (< y 0) (>= y (luft:world-domain-y-limit domain))
            (< z 0) (> z 254))
        :closed-boundary
        (let ((key (luft:chunk-key-at x y)))
          (multiple-value-bind (fibers resident-p)
              (streaming-store-fibers scene key)
            (cond
              ((not resident-p) :unknown-nonresident)
              ((= 1 (luft:fibers-cell-bit fibers x y z)) :solid)
              (t :open-sky)))))))

(zdefun (snapshot-streaming-scene-input :zone :luft/capture-semantic-scene)
    (scene)
  "Freeze SCENE's replace-only authored values for a worker request."
  (let ((material-cells
          (if (streaming-scene-source scene)
              (let ((store (make-material-store)))
                ;; Appearance at an owner boundary also reads the resident
                ;; guard. Capture every chunk table without flattening cells.
                (maphash
                 (lambda (key resident)
                   (setf (gethash key (material-store-chunks store))
                         (resident-cell-chunk-material-cells resident)))
                 (streaming-scene-store scene))
                store)
              (scene-material-cells scene))))
    (make-instance
     'scene
     :solid (scene-solid scene)
     :material-vocabulary (scene-material-vocabulary scene)
     :material-cells material-cells
     :authored-light-sources (scene-authored-light-sources scene)
     :authored-light-opacity-table (scene-authored-light-opacity-table scene)
     :authored-light-revision (scene-authored-light-revision scene)
     :authored-light-provenance (scene-authored-light-provenance scene)
     :authored-light-generation (scene-authored-light-generation scene)
     :content-revision (scene-content-revision scene)
     :torch-light-emission (scene-torch-light-emission scene)
     :voxel-light-propagation-p (scene-voxel-light-propagation-p scene)
     :torches (scene-torches scene)
     :player-p (scene-player-p scene))))

(defstruct (scene-edit
             (:constructor %make-scene-edit
                 (cell old-placement new-placement content-revision))
             (:copier nil))
  "One reversible authored cell transition already published to a scene."
  (cell 0 :type luft:site :read-only t)
  (old-placement nil :type (or null material-placement) :read-only t)
  (new-placement nil :type (or null material-placement) :read-only t)
  (content-revision 0 :type (integer 0 *) :read-only t))

(defun scene-edit-torch-conflict-p (scene cell)
  "Whether changing CELL would invalidate a retained torch attachment."
  (loop for attachment across (scene-torches scene)
        thereis (or (= cell (torch-attachment-support-cell attachment))
                    (= cell (torch-attachment-clearance-cell attachment)))))

(zdefun (edit-streaming-scene-cell :zone :luft/edit-cell)
    (scene cell new-placement)
  "Publish one complete authored cell edit and return EDIT, status, and chunk.

NEW-PLACEMENT fills an empty cell with an existing scene vocabulary member;
NIL removes an occupied cell.  The successor store, chunk fibers, material
state, and light are constructed before the canvas-owned scene is changed;
the old values stay intact for any worker snapshot that captured them.  Active production
is deliberately rejected; the caller may retry after its current cohort has
published."
  (check-type scene streaming-scene)
  (check-type cell luft:site)
  (when new-placement (check-type new-placement material-placement))
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (return-from edit-streaming-scene-cell (values nil :busy nil)))
  (let* ((solid (scene-solid scene))
         (domain (luft:fiber-store-domain solid)))
    (luft:checked-site domain cell)
    (unless (and (= (luft:site-extent cell) luft:+cell-extent+)
                 (luft:site-positive-p cell))
      (error "A scene edit needs one positive cell in the scene domain, not ~S."
             cell))
    (when (scene-edit-torch-conflict-p scene cell)
      (return-from edit-streaming-scene-cell (values nil :attachment nil)))
    (multiple-value-bind (old-offset occupied-p)
        (material-cell-at (scene-material-cells scene) cell)
      (unless (eql occupied-p (= 1 (scene-cell-bit scene
                                                 (luft:site-x cell)
                                                 (luft:site-y cell)
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
        (let* ((unchanged-products
                 (streaming-products-outside-cell scene cell))
               (material-cells (material-store-with-cell
                                (scene-material-cells scene) cell new-offset))
               (bit (if new-placement 1 0))
               (key (luft:site-chunk-key cell))
               (old-chunk (streaming-store-fibers
                           scene key (luft:make-chunk-fibers domain key)))
               (new-chunk (luft:fibers-with-cell
                           old-chunk (luft:site-x cell) (luft:site-y cell)
                           (luft:site-z cell) bit))
               (new-solid (luft:copy-fiber-store solid))
               (light-revision (1+ (scene-authored-light-revision scene))))
          (setf (luft:fiber-store-chunk new-solid key) new-chunk)
          (let* ((sources
                   (let ((sources
                           (remove cell (copy-seq (scene-authored-light-sources scene))
                                   :key #'luft::%voxel-light-source-cell)))
                     (when new-placement
                       (let ((emission (material-kind-packed-light-emission
                                        (material-placement-kind new-placement))))
                         (unless (zerop emission)
                           (setf sources
                                 (concatenate 'vector sources
                                              (vector (luft:make-voxel-light-source
                                                       cell emission)))))))
                     (coerce (sort sources #'<)
                             '(simple-array (unsigned-byte 64) (*)))))
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
                    new-placement content-revision))
                 (mesh-cache
                   (rebase-streaming-star-products
                    unchanged-products key new-chunk
                    (gethash key (material-store-chunks material-cells)))))
            ;; These values are replace-only.  Existing worker snapshots retain
            ;; the old store, fibers, hash table, and light generation.
            (setf (scene-solid scene) new-solid
                  (scene-material-cells scene) material-cells
                  (scene-authored-light-sources scene) sources
                  (scene-authored-light-revision scene) light-revision
                  (scene-authored-light-generation scene) base-generation
                  (scene-content-revision scene) content-revision
                  (streaming-scene-light-generation scene) base-generation)
            (if (streaming-scene-source scene)
                (let ((local-materials
                        (gethash key (material-store-chunks material-cells))))
                  (setf (gethash cell
                                 (authored-world-source-edits
                                  (streaming-scene-source scene)))
                        new-placement
                        (gethash key (streaming-scene-store scene))
                        (%make-resident-cell-chunk
                         key
                         (incf (streaming-scene-next-incarnation scene))
                         new-chunk local-materials)))
                (if (luft:fibers-empty-p new-chunk)
                    (remhash key (streaming-scene-store scene))
                    (setf (gethash key (streaming-scene-store scene))
                          new-chunk)))
            (setf (streaming-scene-mesh-cache scene)
                  mesh-cache)
            (values edit :edited key)))))))

(defun reset-streaming-scene-publication (scene)
  "Forget renderer publication while retaining source-owned resident values."
  (check-type scene streaming-scene)
  (setf (streaming-scene-mesh-cache scene) nil)
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

(zdefun (%retarget-resident-streaming-scene
         :zone :luft/retarget-mesh-residency)
    (scene production-system bevel-width world-x world-y)
  "Batch SCENE's desired window around a camera position and mesh it once.

The camera may stand outside the finite authored world while looking back at
its boundary.  Clamp that position to the nearest domain cell before forming
an unsigned chunk key, so a low-side coordinate cannot wrap to chunk 4095 and
silently empty the desired residency window."
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (return-from %retarget-resident-streaming-scene nil))
  (let* ((domain (scene-domain scene))
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
  (let ((domain (scene-domain scene)))
    (luft:chunk-key-at
     (max 0 (min (1- (luft:world-domain-x-limit domain)) (floor world-x)))
     (max 0 (min (1- (luft:world-domain-y-limit domain)) (floor world-y))))))

(defun streaming-domain-keys-near (scene focus radius)
  "Return every in-domain key in the square RADIUS around FOCUS."
  (let* ((domain (scene-domain scene))
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

(zdefun (rebuild-authored-world-resident-values
         :zone :luft/rebuild-active-scene
         :value (length collision-keys))
    (scene source-keys &optional (collision-keys source-keys))
  "Publish visible materials and a possibly wider resident collision store."
  (let* ((source (streaming-scene-source scene))
         (domain (authored-world-source-domain source))
         (collision (luft:make-fiber-store domain))
         (materials (make-material-store)))
    (dolist (key collision-keys)
      (setf (luft:fiber-store-chunk collision key)
            (resident-cell-chunk-fibers
             (gethash key (streaming-scene-store scene)))))
    (zone :luft/assemble-visible-materials
      (dolist (key source-keys)
        (let ((resident (gethash key (streaming-scene-store scene))))
          (setf (gethash key (material-store-chunks materials))
                (resident-cell-chunk-material-cells resident)))))
    (setf (scene-solid scene) collision
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

(defun streaming-scene-loaded-source-keys (scene)
  "Return the sorted gameplay source keys in SCENE's published mesh cohort."
  (sort (loop for key being the hash-keys of (streaming-scene-loaded scene)
              collect key)
        #'<))

(zdefun (evict-undesired-authored-world-residents
         :zone :luft/evict-source-chunks)
    (scene)
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

(zdefun (retarget-authored-world :zone :luft/retarget-authored-world)
    (scene production-system bevel-width world-x world-y)
  "Demand, asynchronously materialize, and activate one bounded source window."
  (let* ((focus-key (streaming-scene-focus-key scene world-x world-y))
         (focus (cons (luft:chunk-key-x focus-key)
                      (luft:chunk-key-y focus-key)))
         (visible-keys
           (streaming-domain-keys-near
            scene focus-key (streaming-scene-residency-radius scene)))
         (gameplay-keys
           (streaming-domain-keys-near
            scene focus-key +authored-world-gameplay-radius+))
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
               (authored-world-residency-ready-p scene)
               (not (equal visible-keys
                           (streaming-scene-loaded-source-keys scene))))
      ;; Let the established cohort path observe the old published LOADED set;
      ;; it computes exact owner removals before replacing it with this focus.
      (setf (streaming-scene-focus scene) nil)
      (rebuild-authored-world-resident-values
       scene visible-keys gameplay-keys)
      (%retarget-resident-streaming-scene
       scene production-system bevel-width world-x world-y))))

(zdefun (retarget-streaming-scene :zone :luft/retarget-streaming)
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
  (let* ((domain (scene-domain scene))
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
  (let* ((domain (scene-domain scene))
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

(zdefun (make-streaming-region-snapshot :zone :luft/capture-mesh-region
                                        :value (length output-keys))
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
         (domain (scene-domain scene)))
    (unless (every (lambda (key) (member key witness-keys :test #'eql))
                   output-keys)
      (error "Streaming outputs ~S are not all resident in witness set ~S."
             output-keys witness-keys))
    (dolist (key captured-keys)
      ;; Presence records residency.  Empty fibers are still a captured answer
      ;; and must not be confused with an unknown/out-of-window chunk.
      (multiple-value-bind (fibers present-p)
          (streaming-store-fibers scene key)
        (cond
          (present-p
           (setf (gethash key union-neighborhood) fibers))
          ((streaming-scene-source scene)
           (error "Demand snapshot crossed unknown nonresident chunk ~D." key))
          (t
           (setf (gethash key union-neighborhood)
                 (luft:make-chunk-fibers domain key))))))
    (%make-streaming-mesh-snapshot
     scene (snapshot-streaming-scene-input scene)
     output-keys witness-keys resident-source-keys
     bevel-width union-neighborhood
     (streaming-scene-mesh-stamp
      scene output-keys bevel-width)
     realize-torch-light-p
     (streaming-scene-light-generation scene)
     (streaming-scene-mesh-cache scene))))

(defun streaming-owner-input-signature (snapshot key)
  "Identity of an owner's immutable geometry/material inputs and compiler.

The conservative 3x3 guard includes both sides of every chunk seam. Mutable
legacy material tables and callable fields deliberately bypass reuse."
  (let* ((scene (streaming-mesh-snapshot-input-scene snapshot))
         (materials (scene-material-cells scene)))
    (when (typep materials 'material-store)
      (list (scene-domain scene)
            (streaming-mesh-snapshot-bevel-width snapshot)
            (symbol-function 'luft:mesh-star-chunk)
            (symbol-function 'compile-surface-mesh-appearance)
            (symbol-function 'make-render-population)
            (symbol-function 'pack-terrain-appearance-codes)
            (loop for neighbor in
                  (streaming-scene-dependency-guard-keys scene (list key))
                  collect
                  (list neighbor
                        (gethash neighbor
                                 (streaming-mesh-snapshot-union-neighborhood
                                  snapshot))
                        (gethash neighbor (material-store-chunks materials))))))))

(defun streaming-products-outside-cell (scene cell)
  "Retain valid products whose sampled cell box excludes this exact edit.

Chunk identity changes on every edit, but only stars touching CELL can change
geometry or appearance. Validate the old inputs before rebasing unaffected
products; arbitrary source or compiler changes must still miss the cache."
  (let* ((products (streaming-scene-mesh-cache scene))
         (snapshot
           (and products
                (make-streaming-region-snapshot
                 scene (mapcar #'streaming-star-product-key products)
                 luft:+mesh-bevel-width+))))
    (remove-if-not
     (lambda (product)
       (let* ((key (streaming-star-product-key product))
              (x (* luft:+chunk-size+ (luft:chunk-key-x key)))
              (y (* luft:+chunk-size+ (luft:chunk-key-y key))))
         (and (not (and (<= (1- x) (luft:site-x cell) (+ x luft:+chunk-size+))
                        (<= (1- y) (luft:site-y cell) (+ y luft:+chunk-size+))))
              (equal (streaming-star-product-signature product)
                     (streaming-owner-input-signature snapshot key)))))
     products)))

(defun rebase-streaming-star-products (products changed-key fibers materials)
  "Advance only proven-unaffected products to the successor chunk identities."
  (mapcar
   (lambda (product)
     (let ((signature (streaming-star-product-signature product)))
       (make-streaming-star-product
        (streaming-star-product-key product)
        (append
         (butlast signature)
         (list (loop for input in (car (last signature))
                     collect (if (eql changed-key (first input))
                                 (list changed-key fibers materials)
                                 input))))
        (streaming-star-product-descriptors product)
        (streaming-star-product-prepared product))))
   products))

(defun make-streaming-mesh-snapshot
    (scene key bevel-width &key (realize-torch-light-p t))
  "Capture the one-owner form of MAKE-STREAMING-REGION-SNAPSHOT."
  (make-streaming-region-snapshot
   scene (list key) bevel-width
   :realize-torch-light-p realize-torch-light-p))

(zdefun (mesh-streaming-snapshot :zone :luft/mesh-region
                                 :value
                                 (length
                                  (streaming-mesh-snapshot-output-keys snapshot)))
    (snapshot &key prepare-products-p)
  "Mesh one worker-owned regional snapshot without reading owner state.

The first value is an alist of output owner to final mesh. Every output uses
the same width-one star selector against the captured guard fibers. Star
appearance reads authored cells directly and needs no meshed context owners."
  (let* ((owner-scene (streaming-mesh-snapshot-scene snapshot))
         (scene (streaming-mesh-snapshot-input-scene snapshot))
         (neighborhood (streaming-mesh-snapshot-union-neighborhood snapshot))
         (descriptors
           (compile-terrain-material-descriptors (scene-material-vocabulary scene)))
         (products nil))
    (labels ((mesh-owner (key)
               (let* ((fibers (gethash key neighborhood))
                      (signature (streaming-owner-input-signature snapshot key))
                      (cached
                        (find key (streaming-mesh-snapshot-mesh-cache snapshot)
                              :key #'streaming-star-product-key :test #'eql))
                      (reusable
                        (and signature cached
                             (equal signature (streaming-star-product-signature cached))
                             (equalp descriptors (streaming-star-product-descriptors cached))))
                      (prepared (and reusable (streaming-star-product-prepared cached)))
                      (old-mesh (and prepared (prepared-render-mesh-mesh prepared))))
                 (unless fibers
                   (error "Chunk ~D was not captured by this regional snapshot."
                          key))
                 (let ((mesh
                         (if old-mesh
                             (luft:make-surface-mesh
                              (scene-domain scene)
                              (luft:surface-mesh-star-site-words old-mesh))
                             (zone :luft/mesh-owner
                               (luft:mesh-star-chunk
                                fibers key :outside-domain-policy :air)))))
                   ;; New metadata shell: never change an installed mesh's light
                   ;; or attachment witnesses when borrowing immutable arrays.
                   (if old-mesh
                       (setf (luft:surface-mesh-appearance-codes mesh)
                             (luft:surface-mesh-appearance-codes old-mesh)
                             (luft:surface-mesh-appearance-descriptor-words mesh)
                             descriptors)
                       (zone :luft/compile-appearance
                         (compile-surface-mesh-appearance
                          mesh (scene-material-cells scene) descriptors)))
                   (push (list key signature prepared) products)
                   mesh)))
             (decorate-owners (owners)
               (decorate-scene-meshes
                owners scene :appearance-prepared-p t
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
               (multiple-value-bind (fibers present-p)
                   (gethash (luft:missing-chunk-key condition) neighborhood)
                 (if present-p
                     (invoke-restart 'luft:use-chunk fibers)
                     (invoke-restart 'luft:treat-as-air)))))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let* ((output-keys
                 (streaming-mesh-snapshot-output-keys snapshot))
               (owners
                 (mapcar (lambda (key) (cons key (mesh-owner key)))
                         output-keys)))
          (multiple-value-bind (decorated generation)
              (decorate-owners owners)
            (values
             decorated nil nil generation
             (when prepare-products-p
               (loop for (key signature previous) in (nreverse products)
                     for mesh = (cdr (assoc key decorated :test #'eql))
                     collect
                     (make-streaming-star-product
                      key signature descriptors
                      (if previous
                          (%make-prepared-render-mesh
                           mesh (prepared-render-mesh-population previous))
                          (prepare-render-mesh mesh))))))))))))

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

(zdefmethod (production:perform-production-request
             :zone :luft/produce-mesh-region)
    ((request streaming-mesh-request))
  (multiple-value-bind (meshes census diagnostics generation products)
      (mesh-streaming-snapshot (streaming-mesh-request-snapshot request)
                               :prepare-products-p t)
    (declare (ignore meshes census diagnostics))
    (%make-streaming-mesh-result
     (mapcar (lambda (product)
               (cons (streaming-star-product-key product)
                     (streaming-star-product-prepared product)))
             products)
     generation products)))

(defconstant +streaming-cohort-production-key+ :luft-streaming-cohort)

(defvar *streaming-mesh-snapshot-observer* nil
  "Optional test instrumentation called with each snapshot before scheduling.")

(zdefun (schedule-streaming-scene-cohort :zone :luft/schedule-mesh-region
                                         :value (length output-keys))
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
      (setf (streaming-scene-mesh-cache scene)
            (streaming-mesh-result-mesh-cache result))
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

(zdefun (publish-ready-streaming-scene :zone :luft/publish-ready-region)
    (scene renderer)
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

(zdefun (drain-streaming-scene-production :zone :luft/drain-production)
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
