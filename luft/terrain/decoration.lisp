(in-package #:luft.terrain)

;;; Finalize star-mesh appearance and light metadata, then record the exact
;;; outputs as a SCENE-MESH-GENERATION. Static compilation also combines the
;;; region into one root. Each owner carries the frames of the attachments its
;;; chunk supports; attachment light is not yet seeded into the voxel field.

(defun default-face-stock (face)
  (mod (+ (luft:site-x face) (* 2 (luft:site-y face))
          (* 3 (luft:site-z face)) (luft:site-extent face))
       4))

(defun surface-attachment-form-code (attachment normal-z)
  "The drawn-body code of ATTACHMENT: 0 a torch, 1 a standing lantern, 2 a
hanging one."
  (ecase (or (material-kind-attachment-form (surface-attachment-material attachment))
             :torch)
    (:torch 0)
    (:lantern (if (minusp normal-z) 2 1))))

(defun %attachment-seed (x y z)
  (let ((value (* (sin (+ (* x 0.1731) (* y 0.1173) (* z 0.2357))) 43758.5453)))
    (- value (floor value))))

(defun surface-attachment-frame-data (attachment)
  "ATTACHMENT's realized frame: twelve floats in the renderer's torch frame
order -- origin and seed, outward normal and flags, tangent and scale.

The origin is the face's centre moved by the chart point U and V, half a cell
each.  FLAGS holds the material's packed RGB4 emission in bits 0-11 and the
drawn-body code (SURFACE-ATTACHMENT-FORM-CODE) from bit 12."
  (let* ((support (surface-attachment-support-cell attachment))
         (clearance (surface-attachment-clearance-cell attachment))
         (nx (- (luft:site-x clearance) (luft:site-x support)))
         (ny (- (luft:site-y clearance) (luft:site-y support)))
         (nz (- (luft:site-z clearance) (luft:site-z support)))
         ;; The face's centre is halfway between the two cell centres.
         (cx (+ (luft:site-x support) 0.5 (* 0.5 nx)))
         (cy (+ (luft:site-y support) 0.5 (* 0.5 ny)))
         (cz (+ (luft:site-z support) 0.5 (* 0.5 nz)))
         ;; Tangent U is the first other axis in X, Y, Z order; V the second.
         (tangent (if (zerop nx) '(1 0 0) '(0 1 0)))
         (bitangent (cond ((not (zerop nz)) '(0 1 0))
                          (t '(0 0 1))))
         (u (* 0.5 (surface-attachment-chart-u attachment)))
         (v (* 0.5 (surface-attachment-chart-v attachment)))
         (flags (logior (surface-attachment-light-emission attachment)
                        (ash (surface-attachment-form-code attachment nz) 12))))
    (map '(simple-array single-float (12))
         (lambda (value) (coerce value 'single-float))
         (list (+ cx (* u (first tangent)) (* v (first bitangent)))
               (+ cy (* u (second tangent)) (* v (second bitangent)))
               (+ cz (* u (third tangent)) (* v (third bitangent)))
               (%attachment-seed cx cy cz)
               nx ny nz flags
               (first tangent) (second tangent) (third tangent)
               1.0))))

(defun scene-owner-attachment-frames (scene key)
  "Frames of SCENE's attachments supported in chunk KEY, or all for NIL."
  (loop for attachment across (scene-attachments scene)
        when (or (null key)
                 (not (integerp key))
                 (= key (luft:site-chunk-key
                         (surface-attachment-support-cell attachment))))
          collect (surface-attachment-frame-data attachment)))

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
       request-stamp reusable-light-generation light-generation
       (realize-attachment-light-p t)
       (generation-scene scene))
  "Finalize appearance and exact light metadata for a region of star meshes.

OWNERS is an alist of canonical chunk owner to finished mesh. The returned
SCENE-MESH-GENERATION snapshots those exact outputs after metadata is final.
APPEARANCE-PREPARED-P means their material arrays are already compiled.
LIGHT-GENERATION, when given, is an already solved regional generation
(#BQMYNU) and replaces the scene's whole-field realization.

Each owner's mesh carries the frames of the attachments whose support cell
lies in its chunk (SURFACE-ATTACHMENT-FRAME-DATA); an independent drawing,
such as the renderer's torches and lanterns, consumes them.  The frames do not
yet seed the realized voxel light."
  (check-type owners list)
  (let* ((light-generation
           (zone :luft/realize-light
             (cond
               (light-generation light-generation)
               (realize-attachment-light-p
                 (scene-mesh-light-generation scene reusable-light-generation))
               (t
                 (or (etypecase reusable-light-generation
                       (null nil)
                       (realized-light-generation reusable-light-generation)
                       (scene-mesh-generation
                        (scene-mesh-generation-light-generation reusable-light-generation)))
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
          (dolist (entry owners)
            (initialize (cdr entry))
            (setf (luft:surface-mesh-attachments (cdr entry))
                  (scene-owner-attachment-frames scene (car entry)))))))
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
            (luft:surface-mesh-bodies root)
            (apply #'concatenate '(simple-array single-float (*))
                   (mapcar #'luft:surface-mesh-bodies meshes))
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
