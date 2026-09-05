(in-package #:luft.render)

;;; Scene decoration: realizing torch frames on a final bevel surface,
;;; solving the light generation those frames imply, and stamping the
;;; result onto meshes as an immutable SCENE-MESH-GENERATION.

(defun face-solid-cell (solid face)
  "Return the occupied cell incident to boundary FACE and which side it is on."
  (let* ((domain (luft:fiber-store-domain solid))
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
    (if (= 1 (handler-bind ((luft:missing-chunk
                              (lambda (condition)
                                (declare (ignore condition))
                                (invoke-restart 'luft:treat-as-air))))
               (luft:fiber-store-cell-bit solid x y z)))
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
  (nth-value 1 (material-cell-at (scene-material-cells scene) cell)))

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
   (scene-domain scene)
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
        (scene-domain scene)
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

(zdefun (decorate-scene-meshes :zone :luft/decorate-meshes
                                :value (length owners))
    (owners scene
     &key surface-context appearance-prepared-p
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
mesh light sidecars and packed body/flame frames finalized.

APPEARANCE-PREPARED-P is the regional compiler's promise that every surface
already has appearance for this exact material snapshot. Light metadata is
always initialized anew, including when immutable appearance arrays are reused."
  (check-type owners list)
  (check-type surface-context list)
  (let* ((frames
           (zone :luft/resolve-attachments
             (unless
                 (every (lambda (entry)
                          (not (null (luft:surface-mesh-star-site-words
                                      (cdr entry)))))
                        owners)
               (resolve-unlit-scene-torch-frames
                owners scene surface-context attachment-source-owners
                attachment-source-owners-p))))
         (light-generation
           (zone :luft/realize-light
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
                       (error "A non-realizing mesh request needs a reusable light generation."))))))
         (field (realized-light-generation-field light-generation)))
    (zone :luft/compile-appearance
      (let ((descriptors
              (unless appearance-prepared-p
                (compile-terrain-material-descriptors
                 (scene-material-vocabulary scene)))))
        (labels ((initialize (surface)
                   (unless appearance-prepared-p
                     (compile-surface-mesh-appearance
                      surface (scene-material-cells scene) descriptors))
                   (setf (luft:surface-mesh-voxel-light surface) field
                         (luft:surface-mesh-attachments surface) nil)
                   (dolist (companion (luft:surface-mesh-companions surface))
                     (initialize companion))))
          (dolist (entry owners) (initialize (cdr entry))))))
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

(defun compile-static-scene-mesh
    (scene bevel-width &key reusable-light-generation)
  "Combine regional output into a single static mesh with its publication proof."
  (declare (ignore bevel-width))
  (multiple-value-bind (owners census diagnostics generation)
      (make-scene-regional-meshes
       scene luft:+mesh-bevel-width+
       :reusable-light-generation reusable-light-generation)
    (let* ((light (scene-mesh-generation-light-generation generation))
           (root (combine-scene-owner-meshes scene owners light)))
      (values
       root census diagnostics
       (make-scene-mesh-generation-value
        scene (scene-mesh-generation-request-stamp generation) light
        :mesh-entries (list (cons +unkeyed-scene-mesh-output+ root))
        :unkeyed-mesh-p t)))))

(defun combine-scene-owner-meshes (scene owners light-generation)
  "Join star/appearance streams while retaining their light and attachment data.

A static root changes the grouping, not the meaning of the regional products.
All owners must share the claimed field and palette. Even an empty scene needs
its exact empty light field so normal publication validation still applies."
  (let* ((meshes (mapcar #'cdr owners))
         (field (realized-light-generation-field light-generation))
         (descriptors (if meshes
                          (luft:surface-mesh-appearance-descriptor-words (first meshes))
                          (compile-terrain-material-descriptors
                           (scene-material-vocabulary scene)))))
    (dolist (mesh meshes)
      (unless (and (eq field (luft:surface-mesh-voxel-light mesh))
                   (equalp descriptors
                           (luft:surface-mesh-appearance-descriptor-words mesh)))
        (error "Static mesh owners do not share the claimed light field and palette.")))
    (let ((root
            (luft:make-surface-mesh
             (scene-domain scene)
             (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                    (mapcar #'luft:surface-mesh-star-site-words meshes)))))
      (setf (luft:surface-mesh-appearance-codes root)
            (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                   (mapcar #'luft:surface-mesh-appearance-codes meshes))
            (luft:surface-mesh-appearance-descriptor-words root) descriptors
            (luft:surface-mesh-voxel-light root) field
            (luft:surface-mesh-attachments root)
            (mapcan (lambda (mesh) (copy-list (luft:surface-mesh-attachments mesh))) meshes)
            (luft:surface-mesh-companions root)
            (mapcan (lambda (mesh) (copy-list (luft:surface-mesh-companions mesh))) meshes))
      root)))

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
    (check-type solid luft:fiber-store)
    (when custom-stock-policy-p
      (error "Scene stock callbacks bypass the production regional compiler; use MAKE-WHOLE-DOMAIN-DIAGNOSTIC-MESH explicitly."))
    (when reusable-light-generation
      (check-type reusable-light-generation scene-mesh-generation)
      (unless
          (eq scene (scene-mesh-generation-scene reusable-light-generation))
        (error "A reusable generation belongs to a different authored scene input.")))
    (zone (:luft/rematerialize :value (luft:fiber-store-count solid))
      (multiple-value-bind (mesh census diagnostics generation)
          (compile-static-scene-mesh
           scene bevel-width
           :reusable-light-generation reusable-light-generation)
        (declare (ignore census diagnostics))
        (values mesh generation)))))
