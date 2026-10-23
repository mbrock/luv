(in-package #:luft.terrain)

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


(defun make-streaming-scene
    (scene &key (frames-per-load 15) (residency-radius 1) mesh-preparer
             (regional-light-p t))
  "Wrap SCENE in bounded camera-driven chunk residency.

Every resident owner uses the same uniform width, or participates in one
cohort-compiled material site policy. Distance-varying geometry was removed
because independently selected chunk widths open their shared lattice sites;
a future LoD must bring an explicit transition representation.

REGIONAL-LIGHT-P publishes streamed light tiles (#BQMYNU) when SCENE
propagates light; otherwise every cohort shares SCENE's one field."
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
                    :voxel-light-propagation-p
                    (scene-voxel-light-propagation-p scene)
                    :light-generation (scene-authored-light-generation scene)
                    :light-tiles
                    (and regional-light-p
                         (scene-voxel-light-propagation-p scene)
                         (make-hash-table :test #'eql))
                    :attachments (scene-attachments scene)
                    :frames-per-load frames-per-load
                    :residency-radius residency-radius
                    :mesh-preparer mesh-preparer)))
    (luft:map-fiber-store
     (lambda (key fibers)
       (setf (gethash key (streaming-scene-store streaming)) fibers))
     (scene-solid scene))
    streaming))

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
     :voxel-light-propagation-p (scene-voxel-light-propagation-p scene)
     :attachments (scene-attachments scene))))

(defstruct (scene-edit
             (:constructor %make-scene-edit
                 (cell old-placement new-placement content-revision))
             (:copier nil))
  "One reversible authored cell transition already published to a scene."
  (cell 0 :type luft:site :read-only t)
  (old-placement nil :type (or null material-placement) :read-only t)
  (new-placement nil :type (or null material-placement) :read-only t)
  (content-revision 0 :type (integer 0 *) :read-only t))

(defun scene-edit-attachment-conflict-p (scene cell)
  "Whether changing CELL would invalidate a retained surface attachment."
  (loop for attachment across (scene-attachments scene)
        thereis (or (= cell (surface-attachment-support-cell attachment))
                    (= cell (surface-attachment-clearance-cell attachment)))))

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
  (when (streaming-scene-replacement scene)
    (return-from edit-streaming-scene-cell (values nil :busy nil)))
  (let* ((solid (scene-solid scene))
         (domain (luft:fiber-store-domain solid)))
    (luft:checked-site domain cell)
    (unless (and (= (luft:site-extent cell) luft:+cell-extent+)
                 (luft:site-positive-p cell))
      (error "A scene edit needs one positive cell in the scene domain, not ~S."
             cell))
    (when (scene-edit-attachment-conflict-p scene cell)
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
                 ;; Regional light re-solves only the tiles around CELL when
                 ;; the edit's cohort is meshed; one whole field is solved now.
                 (base-generation
                   (if (scene-regional-light-p scene)
                       (scene-authored-light-generation scene)
                       (solve-realized-light-generation
                        domain material-cells
                        (scene-authored-light-opacity-table scene)
                        (if (scene-voxel-light-propagation-p scene) sources #())
                        (scene-authored-light-provenance scene) light-revision
                        (make-realized-light-seeds #() #())
                        :field-revision light-revision)))
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
                  (scene-content-revision scene) content-revision)
            (unless (scene-regional-light-p scene)
              (setf (streaming-scene-light-generation scene) base-generation))
            (if (streaming-scene-source scene)
                (let ((local-materials
                        (gethash key (material-store-chunks material-cells))))
                  (setf (gethash cell
                                 (world-source-edits
                                  (streaming-scene-source scene)))
                        new-placement
                        (gethash key (streaming-scene-store scene))
                        (%make-resident-cell-chunk
                         key
                         (incf (streaming-scene-next-incarnation scene))
                         new-chunk local-materials
                         (let ((old (gethash key (streaming-scene-store scene))))
                           (and old (resident-cell-chunk-attachments old))))))
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
  (clrhash (streaming-scene-loaded scene))
  (when (streaming-scene-light-tiles scene)
    (clrhash (streaming-scene-light-tiles scene)))
  (setf (streaming-scene-replacement scene) nil
        (streaming-scene-production-errors scene) nil
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

(defun streaming-scene-resident-attachment-support-keys (scene source-keys)
  "Return sorted support owners for attachments resident in SOURCE-KEYS."
  (sort
   (remove-duplicates
    (loop for attachment across (scene-attachments scene)
          for key = (luft:site-chunk-key
                     (surface-attachment-support-cell attachment))
          when (member key source-keys :test #'eql)
            collect key)
    :test #'eql)
   #'<))

(defun streaming-residency-changes-affect-attachments-p
    (scene changes old-source-keys desired-source-keys)
  "Whether CHANGES can add, remove, or move a resident realized attachment frame."
  (let ((support-keys
          (streaming-scene-resident-attachment-support-keys
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
  (when (streaming-scene-replacement scene)
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
      (let* ((attachment-residency-change-p
               (streaming-residency-changes-affect-attachments-p
                scene residency-changes old-source-keys desired))
             (desired-attachment-p
               (plusp
                (length
                 (streaming-scene-resident-attachment-support-keys
                  scene desired))))
             ;; Regional light carries attachment light in its tiles, which
             ;; STREAMING-LIGHT-DIRTY-OWNERS follows; one whole field must
             ;; instead be re-realized for every owner.
             (regional-p (scene-regional-light-p scene))
             (full-closure-p
               (or width-changes
                   (and attachment-residency-change-p (not regional-p))))
             (realize-attachment-light-p
               (not
                (null
                 (or attachment-residency-change-p
                     (and desired-attachment-p width-changes))))))
        (setf (streaming-scene-focus scene) focus)
        (when changes
          (clrhash loaded)
          (dolist (key desired)
            (setf (gethash key loaded) (gethash key desired-widths)))
          (let ((affected
                  (if full-closure-p
                      desired-owner-keys
                      (union
                       (remove-if-not
                        (lambda (key)
                          (some (lambda (changed)
                                  (chunk-keys-neighbor-p key changed))
                                residency-changes))
                        desired-owner-keys)
                       (and regional-p
                            (set-difference
                             (streaming-light-dirty-owners
                              scene desired-owner-keys)
                             ;; Leaving owners are removed, not relit.
                             owner-removals))))))
            (setf affected (sort (copy-list affected) #'<))
            (cond
              (affected
               (schedule-streaming-scene-cohort
                scene production-system affected bevel-width
                (reduce #'min affected
                        :key (lambda (key)
                               (streaming-scene-key-distance key focus)))
                :realize-attachment-light-p realize-attachment-light-p
                :removals owner-removals))
              (owner-removals
               ;; A removal can leave no affected output owner to send to a
               ;; worker. Preserve the installed realized field when resident
               ;; attachment semantics are unchanged; only an exact empty resident
               ;; attachment set returns to the material-only base generation.
               (setf (streaming-scene-replacement scene)
                     (make-streaming-removal
                      owner-removals
                      (make-scene-mesh-generation-value
                       scene
                       (streaming-scene-mesh-stamp
                        scene nil bevel-width)
                       (if desired-attachment-p
                           (streaming-scene-light-generation scene)
                           (scene-authored-light-generation scene)))))))
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
         (domain (world-source-domain source))
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
    ;; The visible chunks' fixtures are the scene's attachments, in face order.
    (setf (scene-attachments scene)
          (coerce
           (sort (loop for key in source-keys
                       append (copy-list
                               (resident-cell-chunk-attachments
                                (gethash key (streaming-scene-store scene)))))
                 #'< :key #'surface-attachment-face)
           'vector))
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
            :edits (capture-world-source-chunk-edits source key)))
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
    (when (and (null (streaming-scene-replacement scene))
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
