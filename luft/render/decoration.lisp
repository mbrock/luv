(in-package #:luft.render)

;;; Finalize star-mesh appearance and light metadata, then record the exact
;;; outputs as a SCENE-MESH-GENERATION. Static compilation also combines the
;;; region into one root. Authored torch placement is not yet realized here.

(defun default-face-stock (face)
  (mod (+ (luft:site-x face) (* 2 (luft:site-y face))
          (* 3 (luft:site-z face)) (luft:site-extent face))
       4))

(defun scene-mesh-light-generation (scene &optional reusable-light-generation)
  "Retain the exact material-source light generation for the current star mesh."
  (when (and (typep reusable-light-generation 'scene-mesh-generation)
             (not
              (eq (scene-authored-light-provenance scene)
                  (scene-authored-light-provenance
                   (scene-mesh-generation-scene
                    reusable-light-generation)))))
    (error "A reusable scene generation belongs to a different authored light input."))
  (let* ((seeds (make-realized-light-seeds #() #()))
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
     &key appearance-prepared-p
       request-stamp reusable-light-generation
       (realize-torch-light-p t)
       (generation-scene scene))
  "Finalize appearance and exact light metadata for a region of star meshes.

OWNERS is an alist of canonical chunk owner to finished mesh. The returned
SCENE-MESH-GENERATION snapshots those exact outputs after metadata is final.
APPEARANCE-PREPARED-P means their material arrays are already compiled.

Authored torches are retained by SCENE, but the star representation has no
surface-attachment resolver yet. This path therefore produces no torch frames.
The independent torch drawing consumes frames supplied by diagnostic callers;
its presence does not imply that world attachments have been realized."
  (check-type owners list)
  (let* ((light-generation
           (zone :luft/realize-light
             (if realize-torch-light-p
                 (scene-mesh-light-generation scene reusable-light-generation)
                 (or (etypecase reusable-light-generation
                       (null nil)
                       (realized-light-generation reusable-light-generation)
                       (scene-mesh-generation
                        (scene-mesh-generation-light-generation reusable-light-generation)))
                     (error "A non-realizing mesh request needs a reusable light generation.")))))
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
    ;; Snapshot only after appearance and light metadata are final.
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
