(in-package #:luft.render)

;;; Semantic material definitions live here, above scene storage and mesh
;;; realization.  The objects are the inspectable vocabulary; cells and GPU
;;; instances carry only dense offsets closed by an owning vocabulary.

(defclass material-kind ()
  ((name :initarg :name :reader material-kind-name)
   (base-tone :initarg :base-tone :reader material-kind-base-tone)
   (roughness :initarg :roughness :reader material-kind-roughness)
   (metalness :initarg :metalness :initform 0.0
              :reader material-kind-metalness)
   (relief :initarg :relief :reader material-kind-relief)
   ;; Propagation, visible radiance, and transmission are deliberately
   ;; independent.  A source illuminates its own cell before LIGHT-OPACITY is
   ;; applied to light entering that cell.
   (light-opacity :initarg :light-opacity :initform 15
                  :reader material-kind-light-opacity)
   (light-emission :initarg :light-emission :initform '(0 0 0)
                   :reader material-kind-light-emission)
   (surface-emission :initarg :surface-emission :initform 0.0
                     :reader material-kind-surface-emission)
   (opacity :initarg :opacity :initform 1.0
            :reader material-kind-opacity))
  (:documentation "A reusable substance and its renderer-facing response."))

(defclass earth-material-kind (material-kind)
  ((top-tone :initarg :top-tone :initform nil
             :reader %earth-material-kind-top-tone)
   (side-tone :initarg :side-tone :initform nil
              :reader %earth-material-kind-side-tone)
   (underside-tone :initarg :underside-tone :initform nil
                   :reader %earth-material-kind-underside-tone))
  (:documentation
   "Earth may author distinct exposed-top, side, and underside tones.

Each optional tone deliberately inherits BASE-TONE when omitted.  This keeps
custom earth honest about its own lineage instead of acquiring the built-in
terrain palette as an accidental renderer default."))
(defclass stone-material-kind (material-kind) ())
(defclass metal-material-kind (material-kind)
  ((metalness :initform 1.0))
  (:documentation
   "A material whose optical response is metallic.

METALNESS remains ordinary inspectable material data compiled into the
physical GPU descriptor row.  Metal does not inherit any topology family;
renderer attachments are explicitly non-meshed in the bevel policy."))
(defclass luminous-material-kind (material-kind) ())
(defclass crystal-material-kind (luminous-material-kind)
  ((index-of-refraction :initarg :index-of-refraction :initform 1.5
                        :reader crystal-material-index-of-refraction)
   (dispersion :initarg :dispersion :initform 0.0
               :reader crystal-material-dispersion)
   (internal-scatter :initarg :internal-scatter :initform 0.0
                     :reader crystal-material-internal-scatter)
   (chatoyancy :initarg :chatoyancy :initform 0.0
               :reader crystal-material-chatoyancy)
   (anisotropic-sharpness :initarg :anisotropic-sharpness :initform 1.0
                          :reader crystal-material-anisotropic-sharpness)
   (adularescence :initarg :adularescence :initform 0.0
                  :reader crystal-material-adularescence)
   (arris-lustre :initarg :arris-lustre :initform 0.0
                 :reader crystal-material-arris-lustre))
  (:documentation
   "A dielectric crystal with compiled, raster-friendly gemstone optics."))

(defmethod shared-initialize :after
    ((kind material-kind) slot-names &key &allow-other-keys)
  "Validate bounded physical properties at the semantic object boundary."
  (declare (ignore slot-names))
  (let ((metalness (material-kind-metalness kind)))
    (unless (and (realp metalness) (<= 0.0 metalness 1.0))
      (error "Material ~S has metalness ~S outside [0, 1]."
             (material-kind-name kind) metalness))))

(defun earth-material-kind-top-tone (kind)
  "Return KIND's authored top tone, or its base tone when omitted."
  (or (%earth-material-kind-top-tone kind)
      (material-kind-base-tone kind)))

(defun earth-material-kind-side-tone (kind)
  "Return KIND's authored side tone, or its base tone when omitted."
  (or (%earth-material-kind-side-tone kind)
      (material-kind-base-tone kind)))

(defun earth-material-kind-underside-tone (kind)
  "Return KIND's authored underside tone, or its base tone when omitted."
  (or (%earth-material-kind-underside-tone kind)
      (material-kind-base-tone kind)))

(defclass material-frame ()
  ((name :initarg :name :reader material-frame-name)
   (origin :initarg :origin :reader material-frame-origin)
   (axes :initarg :axes :reader material-frame-axes))
  (:documentation
   "A coherent coordinate frame for natural fields or authored construction."))

(defclass material-placement ()
  ((name :initarg :name :reader material-placement-name)
   (kind :initarg :kind :reader material-placement-kind)
   (finish :initarg :finish :reader material-placement-finish)
   (frame :initarg :frame :reader material-placement-frame)
   (role :initarg :role :reader material-placement-role)
   (readings :initform (make-hash-table :test #'eq)
             :reader material-placement-readings))
  (:documentation
   "Authored cell meaning: substance, deliberate finish, frame, and role."))

(defclass surface-reading ()
  ((name :initarg :name :reader surface-reading-name)
   (kind :initarg :kind :reader surface-reading-kind)
   (tone :initarg :tone :reader surface-reading-tone)
   (finish :initarg :finish :reader surface-reading-finish)
   (frame :initarg :frame :reader surface-reading-frame)
   (role :initarg :role :reader surface-reading-role))
  (:documentation
   "One exposed face's derived interpretation of an authored placement."))

(defclass surface-closure-summary ()
  ((readings :initarg :readings :reader surface-closure-summary-readings))
  (:documentation
   "The canonical set of actual face readings incident to one derived stock.

This is material provenance for recursive band and fan closure.  It is
deliberately separate from an assembly's three renderer descriptor readings:
those lanes describe an appearance, and may include shader context which was
not an incident surface."))

(defclass surface-assembly ()
  ((name :initarg :name :reader surface-assembly-name)
   (relation :initarg :relation :reader surface-assembly-relation)
   (primary :initarg :primary :reader surface-assembly-primary)
   (secondary :initarg :secondary :initform nil
              :reader surface-assembly-secondary)
   (tertiary :initarg :tertiary :initform nil
             :reader surface-assembly-tertiary)
   (kernel :initarg :kernel :reader surface-assembly-kernel)
   (closure-summary :initarg :closure-summary
                    :reader surface-assembly-closure-summary))
  (:documentation
   "An interned face, band, or fan material relation compiled for rendering.

CLOSURE-SUMMARY is part of the assembly's semantic identity even when two
assemblies compile to identical GPU descriptor rows.  A later fan must be able
to recover every real incident reading from the stocks produced for its
surrounding bands."))

(defun ensure-semantic-instance (current class &rest initargs)
  "Reinitialize CURRENT when it still has CLASS, preserving live identity."
  (if (and current (eq (class-of current) (find-class class)))
      (progn (apply #'reinitialize-instance current initargs) current)
      (apply #'make-instance class initargs)))

(defvar *earth-material* nil)
(defvar *limestone-material* nil)
(defvar *highland-rock-material* nil)
(defvar *crystal-material* nil)
(defvar *torch-body-material* nil)
(defvar *torch-flame-material* nil)
(defvar *world-material-frame* nil)
(defvar *sanctuary-material-frame* nil)
(defvar *beacon-material-frame* nil)
(defvar *terrain-material-placement* nil)
(defvar *highland-rock-material-placement* nil)
(defvar *sanctuary-material-placement* nil)
(defvar *beacon-material-placement* nil)
(defvar *crystal-material-placement* nil)

(setf *earth-material*
      (ensure-semantic-instance
       *earth-material* 'earth-material-kind
       :name :earth :base-tone '(0.42 0.32 0.21)
       :top-tone '(0.18 0.31 0.105)
       :side-tone '(0.42 0.32 0.21)
       :underside-tone '(0.24 0.18 0.13)
       :roughness 0.92 :relief :granular)
      *limestone-material*
      (ensure-semantic-instance
       *limestone-material* 'stone-material-kind
       :name :limestone :base-tone '(0.53 0.49 0.39)
       :roughness 0.78 :relief :weathered-stone)
      *highland-rock-material*
      (ensure-semantic-instance
       *highland-rock-material* 'stone-material-kind
       :name :highland-rock :base-tone '(0.29 0.30 0.27)
       :roughness 0.94 :relief :weathered-stone)
      *crystal-material*
      (ensure-semantic-instance
       *crystal-material* 'crystal-material-kind
       :name :aether-crystal :base-tone '(0.16 0.68 0.94)
       :roughness 0.14 :relief :crystal
       :light-opacity 1 :light-emission '(3 11 15)
       :surface-emission 0.30 :opacity 0.48
       :index-of-refraction 1.62 :dispersion 0.48
       :internal-scatter 0.58 :chatoyancy 0.42
       :anisotropic-sharpness 18.0 :adularescence 0.30
       :arris-lustre 0.88)
      *torch-body-material*
      (ensure-semantic-instance
       *torch-body-material* 'metal-material-kind
       :name :torch-bronze :base-tone '(0.47 0.17 0.04)
       :roughness 0.52 :metalness 0.88 :relief :forged-metal)
      *torch-flame-material*
      (ensure-semantic-instance
       *torch-flame-material* 'luminous-material-kind
       :name :torch-flame :base-tone '(1.0 0.36 0.055)
       :roughness 0.12 :relief :crystal
       :light-emission '(15 9 3) :surface-emission 1.8 :opacity 1.0)
      *world-material-frame*
      (ensure-semantic-instance
       *world-material-frame* 'material-frame
       :name :world :origin '(0 0 0)
       :axes '((1 0 0) (0 1 0) (0 0 1)))
      *sanctuary-material-frame*
      (ensure-semantic-instance
       *sanctuary-material-frame* 'material-frame
       :name :sanctuary :origin '(32 24 0)
       :axes '((1 0 0) (0 1 0) (0 0 1)))
      *beacon-material-frame*
      (ensure-semantic-instance
       *beacon-material-frame* 'material-frame
       :name :ridge-beacon :origin '(90 78 0)
       :axes '((0.70710677 0.70710677 0)
               (-0.70710677 0.70710677 0)
               (0 0 1)))
      *terrain-material-placement*
      (ensure-semantic-instance
       *terrain-material-placement* 'material-placement
       :name :terrain :kind *earth-material* :finish :living
       :frame *world-material-frame* :role :terrain)
      *highland-rock-material-placement*
      (ensure-semantic-instance
       *highland-rock-material-placement* 'material-placement
       :name :highland-rock :kind *highland-rock-material* :finish :weathered
       :frame *world-material-frame* :role :terrain-rock)
      *sanctuary-material-placement*
      (ensure-semantic-instance
       *sanctuary-material-placement* 'material-placement
       :name :sanctuary-limestone :kind *limestone-material* :finish :dressed
       :frame *sanctuary-material-frame* :role :architecture)
      *beacon-material-placement*
      (ensure-semantic-instance
       *beacon-material-placement* 'material-placement
       :name :ridge-beacon-limestone :kind *limestone-material* :finish :dressed
       :frame *beacon-material-frame* :role :architecture)
      *crystal-material-placement*
      (ensure-semantic-instance
       *crystal-material-placement* 'material-placement
       :name :aether-crystal :kind *crystal-material* :finish :faceted
       :frame *sanctuary-material-frame* :role :crystal))

(defvar *grass-reading* nil)
(defvar *soil-reading* nil)
(defvar *subsoil-reading* nil)
(defvar *stone-reading* nil)
(defvar *foundation-stone-reading* nil)
(defvar *crystal-reading* nil)
(defvar *torch-body-reading* nil)

(setf *grass-reading*
      (ensure-semantic-instance
       *grass-reading* 'surface-reading :name :grass
       :kind *earth-material* :tone '(0.18 0.31 0.105) :finish :living
       :frame *world-material-frame* :role :exposed-top)
      *soil-reading*
      (ensure-semantic-instance
       *soil-reading* 'surface-reading :name :soil
       :kind *earth-material* :tone '(0.42 0.32 0.21) :finish :cut
       :frame *world-material-frame* :role :exposed-side)
      *subsoil-reading*
      (ensure-semantic-instance
       *subsoil-reading* 'surface-reading :name :subsoil
       :kind *earth-material* :tone '(0.24 0.18 0.13) :finish :broken
       :frame *world-material-frame* :role :underside)
      *stone-reading*
      (ensure-semantic-instance
       *stone-reading* 'surface-reading :name :dressed-limestone
       :kind *limestone-material* :tone '(0.53 0.49 0.39) :finish :dressed
       :frame *sanctuary-material-frame* :role :architecture)
      *foundation-stone-reading*
      (ensure-semantic-instance
       *foundation-stone-reading* 'surface-reading :name :foundation-limestone
       :kind *limestone-material* :tone '(0.53 0.49 0.39)
       :finish :earth-weathered :frame *sanctuary-material-frame*
       :role :foundation)
      *crystal-reading*
      (ensure-semantic-instance
       *crystal-reading* 'surface-reading :name :aether-crystal
       :kind *crystal-material* :tone '(0.16 0.68 0.94) :finish :faceted
       :frame *sanctuary-material-frame* :role :crystal)
      *torch-body-reading*
      (ensure-semantic-instance
       *torch-body-reading* 'surface-reading :name :torch-bronze
       :kind *torch-body-material* :tone '(0.47 0.17 0.04) :finish :forged
       :frame *sanctuary-material-frame* :role :torch-body))

(flet ((seed (placement &rest readings)
         (clrhash (material-placement-readings placement))
         (dolist (reading readings)
           (setf (gethash (surface-reading-role reading)
                          (material-placement-readings placement))
                 reading))))
  ;; Built-in placements retain their inspectable readings across live rebuilds;
  ;; later placements derive equivalent readings in their own authored frames.
  (seed *terrain-material-placement*
        *grass-reading* *soil-reading* *subsoil-reading*)
  (seed *highland-rock-material-placement*)
  (seed *sanctuary-material-placement*
        *stone-reading* *foundation-stone-reading*)
  (seed *beacon-material-placement*)
  (seed *crystal-material-placement* *crystal-reading*))

(defun placement-surface-reading
    (placement role name tone finish)
  "Intern one face interpretation in PLACEMENT's authored coordinate frame."
  (or (gethash role (material-placement-readings placement))
      (setf (gethash role (material-placement-readings placement))
            (make-instance 'surface-reading
                           :name (list (material-placement-name placement) name)
                           :kind (material-placement-kind placement)
                           :tone tone :finish finish
                           :frame (material-placement-frame placement)
                           :role role))))

(defun authored-placement-role-reading (kind placement role tone)
  "Intern ROLE from KIND using only PLACEMENT-authored appearance data."
  (unless (eq kind (material-placement-kind placement))
    (error "Material kind ~S does not own placement ~S."
           (material-kind-name kind) (material-placement-name placement)))
  (placement-surface-reading
   placement role role tone (material-placement-finish placement)))

(defgeneric material-role-surface-reading (kind placement role)
  (:documentation
   "Read one supported semantic ROLE from an authored material placement.

This is the single cold derivation protocol shared by per-face interpretation
and dense placement compilation.  It intentionally has no catchall method: a
new material role must state its tone law instead of inheriting a built-in
substance's literals."))

(defmethod material-role-surface-reading
    ((kind earth-material-kind) placement (role (eql :exposed-top)))
  (authored-placement-role-reading
   kind placement role (earth-material-kind-top-tone kind)))

(defmethod material-role-surface-reading
    ((kind earth-material-kind) placement (role (eql :exposed-side)))
  (authored-placement-role-reading
   kind placement role (earth-material-kind-side-tone kind)))

(defmethod material-role-surface-reading
    ((kind earth-material-kind) placement (role (eql :underside)))
  (authored-placement-role-reading
   kind placement role (earth-material-kind-underside-tone kind)))

(defmethod material-role-surface-reading
    ((kind stone-material-kind) placement (role (eql :natural-rock)))
  (authored-placement-role-reading
   kind placement role (material-kind-base-tone kind)))

(defmethod material-role-surface-reading
    ((kind stone-material-kind) placement (role (eql :architecture)))
  (authored-placement-role-reading
   kind placement role (material-kind-base-tone kind)))

(defmethod material-role-surface-reading
    ((kind stone-material-kind) placement (role (eql :foundation)))
  (authored-placement-role-reading
   kind placement role (material-kind-base-tone kind)))

(defmethod material-role-surface-reading
    ((kind crystal-material-kind) placement (role (eql :crystal)))
  (authored-placement-role-reading
   kind placement role (material-kind-base-tone kind)))

(defvar *surface-reading-intern-table* (make-hash-table :test #'equalp))

(defun surface-reading-semantic-key (reading)
  "Return READING's complete stable cold-path identity."
  (let* ((kind (surface-reading-kind reading))
         (frame (surface-reading-frame reading)))
    (list
     (class-name (class-of kind))
     (material-kind-name kind)
     (material-kind-base-tone kind)
     (material-kind-roughness kind)
     (material-kind-metalness kind)
     (material-kind-relief kind)
     (material-kind-light-opacity kind)
     (material-kind-light-emission kind)
     (material-kind-surface-emission kind)
     (material-kind-opacity kind)
     (when (typep kind 'crystal-material-kind)
       (list (crystal-material-index-of-refraction kind)
             (crystal-material-dispersion kind)
             (crystal-material-internal-scatter kind)
             (crystal-material-chatoyancy kind)
             (crystal-material-anisotropic-sharpness kind)
             (crystal-material-adularescence kind)
             (crystal-material-arris-lustre kind)))
     (surface-reading-role reading)
     (surface-reading-name reading)
     (surface-reading-tone reading)
     (surface-reading-finish reading)
     (material-frame-name frame)
     (material-frame-origin frame)
     (material-frame-axes frame))))

(defun canonical-surface-reading (reading)
  "Intern semantically identical readings across scene rebuilds."
  (check-type reading surface-reading)
  (let ((key (surface-reading-semantic-key reading)))
    (multiple-value-bind (canonical present-p)
        (gethash key *surface-reading-intern-table*)
      (if present-p
          canonical
          (setf (gethash key *surface-reading-intern-table*) reading)))))

(defun canonical-surface-reading-or-nil (reading)
  (and reading (canonical-surface-reading reading)))

(defvar *surface-closure-summary-intern-table*
  (make-hash-table :test #'equalp))

(defun surface-closure-reading-order-key (reading)
  "Return the deterministic ordering key used by canonical summary sets."
  (prin1-to-string (surface-reading-semantic-key reading)))

(defun canonical-surface-closure-readings (readings)
  "Return READINGS as a sorted duplicate-free list of canonical readings."
  (sort
   (remove-duplicates
    (mapcar #'canonical-surface-reading readings) :test #'eq)
   #'string< :key #'surface-closure-reading-order-key))

(defun intern-surface-closure-summary (readings)
  "Intern the canonical set of actual incident READINGS."
  (let* ((canonical (canonical-surface-closure-readings readings))
         (key (mapcar #'surface-reading-semantic-key canonical)))
    (unless canonical
      (error "A surface closure summary cannot be empty."))
    (multiple-value-bind (summary present-p)
        (gethash key *surface-closure-summary-intern-table*)
      (if present-p
          summary
          (setf (gethash key *surface-closure-summary-intern-table*)
                (make-instance
                 'surface-closure-summary
                 :readings (coerce canonical 'simple-vector)))))))

(defun join-surface-closure-summaries (left right)
  "Return the canonical set union of closure summaries LEFT and RIGHT."
  (intern-surface-closure-summary
   (append (coerce (surface-closure-summary-readings left) 'list)
           (coerce (surface-closure-summary-readings right) 'list))))

(defun ensure-surface-assembly
    (current name relation primary
     &key secondary tertiary kernel closure-readings)
  (let ((primary (canonical-surface-reading primary)))
    (ensure-semantic-instance
     current 'surface-assembly :name name :relation relation
     :primary primary
     :secondary (canonical-surface-reading-or-nil secondary)
     :tertiary (canonical-surface-reading-or-nil tertiary) :kernel kernel
     :closure-summary
     (intern-surface-closure-summary (or closure-readings (list primary))))))

(defvar *grass-surface* nil)
(defvar *soil-surface* nil)
(defvar *subsoil-surface* nil)
(defvar *stone-surface* nil)
(defvar *turf-set-stone-surface* nil)
(defvar *soil-set-stone-surface* nil)
(defvar *deep-set-stone-surface* nil)
(defvar *turf-edge-surface* nil)
(defvar *foundation-stone-surface* nil)
(defvar *crystal-surface* nil)
(defvar *torch-body-surface* nil)

(setf *grass-surface*
      (ensure-surface-assembly *grass-surface* :grass :face *grass-reading*
                               :kernel :grass)
      *soil-surface*
      (ensure-surface-assembly *soil-surface* :soil :face *soil-reading*
                               :kernel :soil)
      *subsoil-surface*
      (ensure-surface-assembly *subsoil-surface* :subsoil :face
                               *subsoil-reading* :kernel :subsoil)
      *stone-surface*
      (ensure-surface-assembly *stone-surface* :limestone :face *stone-reading*
                               :kernel :stone)
      *turf-set-stone-surface*
      (ensure-surface-assembly
       *turf-set-stone-surface* :turf-set-limestone :contact *stone-reading*
       :secondary *grass-reading*
       :kernel :earth-set-stone
       :closure-readings (list *stone-reading* *grass-reading*))
      *soil-set-stone-surface*
      (ensure-surface-assembly
       *soil-set-stone-surface* :soil-set-limestone :contact *stone-reading*
       :secondary *soil-reading*
       :kernel :earth-set-stone
       :closure-readings (list *stone-reading* *soil-reading*))
      *deep-set-stone-surface*
      (ensure-surface-assembly
       *deep-set-stone-surface* :deep-set-limestone :contact *stone-reading*
       :secondary *subsoil-reading*
       :kernel :earth-set-stone
       :closure-readings (list *stone-reading* *subsoil-reading*))
      *turf-edge-surface*
      (ensure-surface-assembly
       *turf-edge-surface* :turf-edge :contact *grass-reading*
       :secondary *soil-reading* :kernel :turf-edge
       :closure-readings (list *grass-reading* *soil-reading*))
      *foundation-stone-surface*
      (ensure-surface-assembly
       *foundation-stone-surface* :foundation-limestone :face
       *foundation-stone-reading* :kernel :foundation-stone)
      *crystal-surface*
      (ensure-surface-assembly
       *crystal-surface* :aether-crystal :face *crystal-reading*
       :kernel :crystal)
      *torch-body-surface*
      (ensure-surface-assembly
       *torch-body-surface* :torch-bronze :attachment *torch-body-reading*
       :kernel :torch-body))

(defparameter *surface-assembly-vocabulary*
  (domains:make-identity-vocabulary-domain
   :members (list *grass-surface* *soil-surface* *subsoil-surface*
                  *stone-surface* *turf-set-stone-surface*
                  *soil-set-stone-surface* *deep-set-stone-surface*
                  *turf-edge-surface* *foundation-stone-surface*
                  *crystal-surface* *torch-body-surface*)
   :limit #x1000)
  "The renderer-global assembly ABI; scene programs close their own subsets.")

(defun surface-assembly-offset (assembly)
  (domains:identity-vocabulary-offset *surface-assembly-vocabulary* assembly))

(defun surface-assembly-at (offset)
  (domains:identity-vocabulary-member *surface-assembly-vocabulary* offset))

;;; Geometry policy is semantic at this boundary and dense below it.  The
;;; proof-of-concept profile compiles once to a stock-indexed byte lane; mesh
;;; instance partitioning never dispatches on material objects.

(defclass material-bevel-profile ()
  ((terrain-width :initarg :terrain-width :initform 4
                  :type (integer 1 4)
                  :reader material-bevel-profile-terrain-width)
   (architecture-width :initarg :architecture-width :initform 1
                       :type (integer 1 4)
                       :reader material-bevel-profile-architecture-width)
   (crystal-width :initarg :crystal-width :initform 4
                  :type (integer 1 4)
                  :reader material-bevel-profile-crystal-width)
   (contact-width :initarg :contact-width :initform 2
                  :type (integer 1 4)
                  :reader material-bevel-profile-contact-width)
   (terrain-architecture-width
    :initarg :terrain-architecture-width :initform nil
    :type (or null (integer 1 4))
    :reader material-bevel-profile-terrain-architecture-width)
   (terrain-crystal-width
    :initarg :terrain-crystal-width :initform nil
    :type (or null (integer 1 4))
    :reader material-bevel-profile-terrain-crystal-width)
   (architecture-crystal-width
    :initarg :architecture-crystal-width :initform nil
    :type (or null (integer 1 4))
    :reader material-bevel-profile-architecture-crystal-width)
   (three-way-width
    :initarg :three-way-width :initform nil
    :type (or null (integer 1 4))
    :reader material-bevel-profile-three-way-width))
  (:documentation
   "A semantic assignment of LUFT integer bevel widths to material relations."))

(defun make-material-bevel-profile
    (&key (terrain-width 4) (architecture-width 1) (crystal-width 4)
          (contact-width 2)
          (terrain-architecture-width contact-width)
          (terrain-crystal-width contact-width)
          (architecture-crystal-width contact-width)
          (three-way-width contact-width))
  (dolist (width (list terrain-width architecture-width crystal-width
                       contact-width terrain-architecture-width
                       terrain-crystal-width architecture-crystal-width
                       three-way-width))
    (unless (and (integerp width)
                 (<= 1 width (/ luft:+mesh-cell-size+ 2)))
      (error "Material bevel width ~S must be an integer between one and four ticks."
             width)))
  (make-instance 'material-bevel-profile
                 :terrain-width terrain-width
                 :architecture-width architecture-width
                 :crystal-width crystal-width
                 :contact-width contact-width
                 :terrain-architecture-width terrain-architecture-width
                 :terrain-crystal-width terrain-crystal-width
                 :architecture-crystal-width architecture-crystal-width
                 :three-way-width three-way-width))

(defun material-bevel-profile-mixed-width (profile width)
  "Resolve an optional relation-specific WIDTH through CONTACT-WIDTH."
  (or width (material-bevel-profile-contact-width profile)))

(defun surface-assembly-readings (assembly)
  "Return the actual incident readings retained by ASSEMBLY's closure summary."
  (coerce
   (surface-closure-summary-readings
    (surface-assembly-closure-summary assembly))
   'list))

(defconstant +material-bevel-terrain-mask+ #b01)
(defconstant +material-bevel-architecture-mask+ #b10)
(defconstant +material-bevel-crystal-mask+ #b100)
(defconstant +material-bevel-terrain-architecture-mask+ #b011)
(defconstant +material-bevel-terrain-crystal-mask+ #b101)
(defconstant +material-bevel-architecture-crystal-mask+ #b110)
(defconstant +material-bevel-three-way-mask+ #b111)
(defconstant +material-bevel-non-meshed-mask+ #x80)

(defgeneric material-bevel-family-mask (kind reading)
  (:documentation
   "Compile KIND and its semantic surface READING to one topology-family bit.

This cold protocol deliberately distinguishes rendered attachments from mesh
materials.  The positive NON-MESHED sentinel keeps dense policy lanes total
without assigning an attachment a false terrain or architecture identity."))

(defmethod material-bevel-family-mask
    ((kind earth-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    ((:exposed-top :exposed-side :underside)
     +material-bevel-terrain-mask+)))

(defmethod material-bevel-family-mask
    ((kind stone-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    ((:architecture :foundation) +material-bevel-architecture-mask+)
    (:natural-rock +material-bevel-terrain-mask+)))

(defmethod material-bevel-family-mask
    ((kind crystal-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    (:crystal +material-bevel-crystal-mask+)))

(defmethod material-bevel-family-mask
    ((kind metal-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    (:torch-body +material-bevel-non-meshed-mask+)))

(defun surface-reading-material-bevel-mask (reading)
  (material-bevel-family-mask (surface-reading-kind reading) reading))

(defun surface-assembly-material-bevel-mask (assembly)
  "Return the semantic material-family mask represented by ASSEMBLY.

Descriptor secondary and tertiary readings may be shader-only context.  The
closure summary is the authoritative set of participating material families."
  (reduce #'logior (surface-assembly-readings assembly)
          :key #'surface-reading-material-bevel-mask
          :initial-value 0))

(defgeneric material-bevel-width (profile assembly)
  (:documentation
   "Return ASSEMBLY's LUFT bevel width, or zero for a non-meshed descriptor."))

(defmethod material-bevel-width
    ((profile material-bevel-profile) (assembly surface-assembly))
  (let ((mask (surface-assembly-material-bevel-mask assembly)))
    (cond ((= mask +material-bevel-non-meshed-mask+) 0)
          ((= mask +material-bevel-terrain-mask+)
           (material-bevel-profile-terrain-width profile))
          ((= mask +material-bevel-architecture-mask+)
           (material-bevel-profile-architecture-width profile))
          ((= mask +material-bevel-crystal-mask+)
           (material-bevel-profile-crystal-width profile))
          ((= mask +material-bevel-terrain-architecture-mask+)
           (material-bevel-profile-mixed-width
            profile
            (material-bevel-profile-terrain-architecture-width profile)))
          ((= mask +material-bevel-terrain-crystal-mask+)
           (material-bevel-profile-mixed-width
            profile (material-bevel-profile-terrain-crystal-width profile)))
          ((= mask +material-bevel-architecture-crystal-mask+)
           (material-bevel-profile-mixed-width
            profile
            (material-bevel-profile-architecture-crystal-width profile)))
          ((= mask +material-bevel-three-way-mask+)
           (material-bevel-profile-mixed-width
            profile (material-bevel-profile-three-way-width profile)))
          (t
           (error "Material assembly ~S has invalid bevel mask ~3,'0B."
                  assembly mask)))))

(defun compile-material-bevel-profile
    (profile &optional (vocabulary *surface-assembly-vocabulary*))
  "Compile PROFILE to one byte per stock; zero denotes a non-meshed stock."
  (check-type profile material-bevel-profile)
  (let* ((members (domains:identity-vocabulary-members vocabulary))
         (widths (make-array (length members) :element-type '(unsigned-byte 8))))
    (loop for assembly across members
          for stock from 0
          for mask = (surface-assembly-material-bevel-mask assembly)
          for width = (material-bevel-width profile assembly)
          do (unless (if (= mask +material-bevel-non-meshed-mask+)
                         (eql width 0)
                         (and (integerp width)
                              (<= 1 width (/ luft:+mesh-cell-size+ 2))))
               (error "Material bevel policy assigned invalid width ~S to ~S."
                      width assembly))
             (setf (aref widths stock) width))
    widths))

(defun compile-material-bevel-site-policy
    (profile &optional (vocabulary *surface-assembly-vocabulary*))
  "Compile PROFILE into dense stock masks and site widths.

The first value is one terrain/architecture/crystal bit mask per packed
assembly stock.  Non-meshed renderer descriptors receive the named positive
rejection sentinel, which preserves the paged byte compiler and necessarily
indexes outside the second value if such a stock ever reaches topology.  The
second value is an eight-entry byte table indexed by the OR of every valid
incident stock mask.  Pure and mixed sites each select their semantic relation
width; CONTACT-WIDTH is constructor shorthand for all four mixed relations."
  (check-type profile material-bevel-profile)
  ;; Compile the ordinary stock widths as the validation oracle.  In
  ;; particular, a profile made without MAKE-MATERIAL-BEVEL-PROFILE must not
  ;; smuggle an out-of-range width into the site table.
  (compile-material-bevel-profile profile vocabulary)
  (dolist (width
           (list
            (material-bevel-profile-mixed-width
             profile
             (material-bevel-profile-terrain-architecture-width profile))
            (material-bevel-profile-mixed-width
             profile (material-bevel-profile-terrain-crystal-width profile))
            (material-bevel-profile-mixed-width
             profile
             (material-bevel-profile-architecture-crystal-width profile))
            (material-bevel-profile-mixed-width
             profile (material-bevel-profile-three-way-width profile))))
    (unless (and (integerp width)
                 (<= 1 width (/ luft:+mesh-cell-size+ 2)))
      (error "Material bevel policy assigned invalid mixed width ~S." width)))
  (let* ((members (domains:identity-vocabulary-members vocabulary))
         (stock-masks
           (make-array (length members) :element-type '(unsigned-byte 8)))
         (site-widths
           (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (aref site-widths 0) 0
          (aref site-widths +material-bevel-terrain-mask+)
          (material-bevel-profile-terrain-width profile)
          (aref site-widths +material-bevel-architecture-mask+)
          (material-bevel-profile-architecture-width profile)
          (aref site-widths +material-bevel-crystal-mask+)
          (material-bevel-profile-crystal-width profile)
          (aref site-widths +material-bevel-terrain-architecture-mask+)
          (material-bevel-profile-mixed-width
           profile
           (material-bevel-profile-terrain-architecture-width profile))
          (aref site-widths +material-bevel-terrain-crystal-mask+)
          (material-bevel-profile-mixed-width
           profile (material-bevel-profile-terrain-crystal-width profile))
          (aref site-widths +material-bevel-architecture-crystal-mask+)
          (material-bevel-profile-mixed-width
           profile
           (material-bevel-profile-architecture-crystal-width profile))
          (aref site-widths +material-bevel-three-way-mask+)
          (material-bevel-profile-mixed-width
           profile (material-bevel-profile-three-way-width profile)))
    (loop for assembly across members
          for stock from 0
          do (setf (aref stock-masks stock)
                   (surface-assembly-material-bevel-mask assembly)))
    (values stock-masks site-widths)))

;; These named built-in offsets are a stable renderer ABI.  Material closure
;; never classifies an assembly by this prefix or by a numeric stock range.
(defconstant +grass-stock+ 0)
(defconstant +soil-stock+ 1)
(defconstant +subsoil-stock+ 2)
(defconstant +stone-stock+ 3)
(defconstant +turf-set-stone-stock+ 4)
(defconstant +soil-set-stone-stock+ 5)
(defconstant +deep-set-stone-stock+ 6)
(defconstant +turf-edge-stock+ 7)
(defconstant +foundation-stone-stock+ 8)

(defconstant +surface-assembly-descriptor-row-count+ 8)

(defun surface-kernel-code (kernel)
  "Compile the intentionally closed shader-kernel ABI."
  (ecase kernel
    (:grass 7)
    (:soil 5)
    (:subsoil 6)
    (:stone 4)
    (:earth-set-stone 1)
    (:turf-edge 2)
    (:foundation-stone 3)
    (:crystal 8)
    (:torch-body 9)))

(defun material-relief-code (relief)
  "Compile the closed procedural-relief ABI shared by all surface kernels."
  (ecase relief
    (:granular 1)
    (:weathered-stone 2)
    (:crystal 3)
    (:forged-metal 4)))

(defun material-relief-amplitude (relief)
  (ecase relief
    (:granular 0.028)
    (:weathered-stone 0.020)
    (:crystal 0.012)
    (:forged-metal 0.010)))

(defun surface-assembly-material-kind (assembly)
  (surface-reading-kind (surface-assembly-primary assembly)))

(defun surface-assembly-opacity (assembly)
  (material-kind-opacity (surface-assembly-material-kind assembly)))

(defun surface-assembly-surface-emission (assembly)
  (material-kind-surface-emission
   (surface-assembly-material-kind assembly)))

(defun surface-assembly-metalness (assembly)
  (material-kind-metalness (surface-assembly-material-kind assembly)))

(defun surface-assembly-translucent-p (assembly)
  (< (surface-assembly-opacity assembly) 1.0))

(defun surface-contact-variant (assembly)
  (if (eq (surface-assembly-kernel assembly) :earth-set-stone)
      (let ((secondary (surface-assembly-secondary assembly)))
        (cond ((eq (surface-reading-role secondary) :exposed-top) 0)
              ((eq (surface-reading-role secondary) :exposed-side) 1)
              ((eq (surface-reading-role secondary) :underside) 2)
              (t (error "Unknown earth contact reading ~S." secondary))))
      0))

(defun surface-assembly-descriptor-words
    (&optional (vocabulary *surface-assembly-vocabulary*))
  "Compile VOCABULARY into fixed-stride float32 rows for direct GPU indexing.

Each assembly owns eight vec4 rows: primary/kernel, secondary/contact variant,
tertiary/roughness, an explicit physical row, then frame origin/relief profile
and its three axes.  Physical X is metalness; YZW are reserved and compile to
zero.  A crystal reuses otherwise redundant secondary and tertiary tone lanes
for IOR, dispersion, scatter, chatoyancy, anisotropic sharpness,
adularescence, and arris lustre; tertiary W remains roughness.  The X axis row
carries relief amplitude, Y carries visual opacity, and Z carries HDR surface
emission.  Propagation loss and RGB source strength remain CPU-only material
facts."
  (let* ((members (domains:identity-vocabulary-members vocabulary))
         (words
           (make-array (* (length members)
                          +surface-assembly-descriptor-row-count+ 4)
                       :element-type 'single-float)))
    (labels ((put-row (row values)
               (loop for value in values
                     for lane from (* row 4)
                     do (setf (aref words lane)
                              (coerce value 'single-float))))
             (tone-of (reading fallback)
               (surface-reading-tone (or reading fallback))))
      (loop for assembly across members
            for index from 0
            for primary = (surface-assembly-primary assembly)
            for secondary = (or (surface-assembly-secondary assembly) primary)
            for tertiary = (or (surface-assembly-tertiary assembly) secondary)
            for kind = (surface-reading-kind primary)
            for frame = (surface-reading-frame primary)
            for row = (* index +surface-assembly-descriptor-row-count+)
            do (put-row row
                        (append (tone-of primary primary)
                                (list (surface-kernel-code
                                       (surface-assembly-kernel assembly)))))
               (if (typep kind 'crystal-material-kind)
                   (progn
                     (put-row
                      (+ row 1)
                      (list (crystal-material-index-of-refraction kind)
                            (crystal-material-dispersion kind)
                            (crystal-material-internal-scatter kind)
                            (crystal-material-chatoyancy kind)))
                     (put-row
                      (+ row 2)
                      (list (crystal-material-anisotropic-sharpness kind)
                            (crystal-material-adularescence kind)
                            (crystal-material-arris-lustre kind)
                            (material-kind-roughness kind))))
                   (progn
                     (put-row (+ row 1)
                              (append
                               (tone-of secondary primary)
                               (list (surface-contact-variant assembly))))
                     (put-row (+ row 2)
                              (append
                               (tone-of tertiary secondary)
                               (list (material-kind-roughness kind))))))
               (put-row (+ row 3)
                        (list (surface-assembly-metalness assembly)
                              0.0 0.0 0.0))
               (put-row (+ row 4)
                        (append
                         (material-frame-origin frame)
                         (list (material-relief-code
                                (material-kind-relief
                                 (surface-reading-kind primary))))))
               (loop for axis in (material-frame-axes frame)
                     for axis-row from (+ row 5)
                     for amplitude = (material-relief-amplitude
                                      (material-kind-relief
                                       (surface-reading-kind primary)))
                     do (put-row
                         axis-row
                         (append
                          axis
                          (list
                           (cond ((= axis-row (+ row 5)) amplitude)
                                 ((= axis-row (+ row 6))
                                  (surface-assembly-opacity assembly))
                                 (t
                                  (surface-assembly-surface-emission
                                   assembly)))))))))
    words))

(defun make-scene-material-vocabulary ()
  "Return the authored placement vocabulary shared by one scene's cells."
  (domains:make-identity-vocabulary-domain
   :members (list *terrain-material-placement* *highland-rock-material-placement*
                  *sanctuary-material-placement* *crystal-material-placement*)
   :limit #x10000))

(defgeneric material-face-reading (kind placement scene cell axis side)
  (:documentation
   "Derive one exposed face reading from an authored material placement."))

(defmethod material-face-reading
    ((kind earth-material-kind) placement scene cell axis side)
  (declare (ignore scene cell))
  (material-role-surface-reading
   kind placement
   (cond ((not (eq axis :z)) :exposed-side)
         ((eq side :backward) :exposed-top)
         (t :underside))))

(defmethod material-face-reading
    ((kind stone-material-kind) placement scene cell axis side)
  (declare (ignore axis side))
  (material-role-surface-reading
   kind placement
   (cond ((eq :terrain-rock (material-placement-role placement))
          :natural-rock)
         ((scene-foundation-cell-p scene cell) :foundation)
         (t :architecture))))

(defmethod material-face-reading
    ((kind crystal-material-kind) placement scene cell axis side)
  (declare (ignore scene cell axis side))
  (material-role-surface-reading kind placement :crystal))

(defun find-surface-assembly
    (relation primary secondary tertiary kernel closure-summary)
  (find-if (lambda (assembly)
             (and (eq relation (surface-assembly-relation assembly))
                  (eq primary (surface-assembly-primary assembly))
                  (eq secondary (surface-assembly-secondary assembly))
                  (eq tertiary (surface-assembly-tertiary assembly))
                  (eq kernel (surface-assembly-kernel assembly))
                  (eq closure-summary
                      (surface-assembly-closure-summary assembly))))
           (domains:identity-vocabulary-members
            *surface-assembly-vocabulary*)))

(defun intern-surface-assembly
    (relation primary
     &key secondary tertiary kernel name closure-readings closure-summary)
  "Intern a small semantic assembly at mesh-compilation time."
  (let ((primary (canonical-surface-reading primary))
        (secondary (canonical-surface-reading-or-nil secondary))
        (tertiary (canonical-surface-reading-or-nil tertiary)))
    (when (and closure-readings closure-summary)
      (error "Specify closure readings or a closure summary, not both."))
    (let ((closure-summary
            (or closure-summary
                (intern-surface-closure-summary
                 (or closure-readings (list primary))))))
      (or (find-surface-assembly
           relation primary secondary tertiary kernel closure-summary)
        (let ((assembly
                (make-instance
                 'surface-assembly
                 :name (or name
                           (list relation (surface-reading-name primary)))
                 :relation relation :primary primary :secondary secondary
                 :tertiary tertiary :kernel kernel
                 :closure-summary closure-summary)))
          (surface-assembly-offset assembly)
          assembly)))))

(defgeneric material-reading-surface-kernel (kind reading)
  (:documentation
   "Select the closed shader kernel for one canonical surface reading.

This protocol runs only while compiling semantic material objects.  Methods
are deliberately total only for supported material families and roles: a new
reading must declare its rendering law here instead of silently becoming a
stone face in the dense mesh ABI."))

(defmethod material-reading-surface-kernel
    ((kind earth-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    (:exposed-top :grass)
    (:exposed-side :soil)
    (:underside :subsoil)))

(defmethod material-reading-surface-kernel
    ((kind stone-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    ((:architecture :natural-rock) :stone)
    (:foundation :foundation-stone)))

(defmethod material-reading-surface-kernel
    ((kind crystal-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    (:crystal :crystal)))

(defmethod material-reading-surface-kernel
    ((kind metal-material-kind) reading)
  (declare (ignore kind))
  (ecase (surface-reading-role reading)
    (:torch-body :torch-body)))

(defun face-reading-assembly (reading)
  (let ((reading (canonical-surface-reading reading)))
    (cond ((eq reading *grass-reading*) *grass-surface*)
          ((eq reading *soil-reading*) *soil-surface*)
          ((eq reading *subsoil-reading*) *subsoil-surface*)
          ((eq reading *stone-reading*) *stone-surface*)
          ((eq reading *foundation-stone-reading*) *foundation-stone-surface*)
          ((eq reading *crystal-reading*) *crystal-surface*)
          ((eq reading *torch-body-reading*) *torch-body-surface*)
          (t
           (intern-surface-assembly
            :face reading
            :kernel
            (material-reading-surface-kernel
             (surface-reading-kind reading) reading)
            :closure-readings (list reading))))))

(defun stone-reading-p (reading)
  (typep (surface-reading-kind reading) 'stone-material-kind))

(defun contact-reading-semantic-key (reading)
  "Return a stable cold-path key for otherwise equivalent contact readings."
  (prin1-to-string (surface-reading-semantic-key reading)))

(defun higher-ranked-contact-reading (left right role-ranks)
  "Choose one owner by semantic rank, independent of argument order."
  (let ((left (canonical-surface-reading left))
        (right (canonical-surface-reading right)))
    (if (eq left right)
        left
        (let ((left-rank
              (or (cdr (assoc (surface-reading-role left) role-ranks)) 0))
            (right-rank
              (or (cdr (assoc (surface-reading-role right) role-ranks)) 0)))
          (cond ((> left-rank right-rank) left)
                ((< left-rank right-rank) right)
                (t
                 (let ((left-key (contact-reading-semantic-key left))
                       (right-key (contact-reading-semantic-key right)))
                   (cond ((string< left-key right-key) left)
                         ((string< right-key left-key) right)
                         (t
                          (error
                           "Distinct contact readings ~S and ~S have the same semantic key."
                           left right))))))))))

(defun host-contact-surface-assembly (host guest &key closure-summary)
  "Intern a contact rendered by HOST while retaining GUEST as context."
  (if (eq host guest)
      (face-reading-assembly host)
      (intern-surface-assembly
       :contact host :secondary guest
       :kernel (surface-assembly-kernel (face-reading-assembly host))
       :closure-summary
       (or closure-summary
           (intern-surface-closure-summary (list host guest))))))

(defun earth-set-stone-contact-surface-assembly
    (stone earth &key closure-summary)
  "Intern a stone-owned contact weathered by an actual incident earth reading."
  (intern-surface-assembly
   :contact stone :secondary earth
   :kernel :earth-set-stone
   :closure-summary
   (or closure-summary
       (intern-surface-closure-summary (list stone earth)))))

(defun surface-assembly-with-closure-summary (appearance summary)
  "Return APPEARANCE with complete closure provenance SUMMARY."
  (if (eq summary (surface-assembly-closure-summary appearance))
      appearance
      (intern-surface-assembly
       (surface-assembly-relation appearance)
       (surface-assembly-primary appearance)
       :secondary (surface-assembly-secondary appearance)
       :tertiary (surface-assembly-tertiary appearance)
       :kernel (surface-assembly-kernel appearance)
       :name (surface-assembly-name appearance)
       :closure-summary summary)))

(defgeneric material-contact-surface-assembly
    (left-kind right-kind left-reading right-reading)
  (:documentation
   "Resolve an unordered semantic contact to an assembly whose primary owns it.

Methods are intentionally defined only for supported material families.  The
cold compiler calls both argument orders and requires the same interned result;
there is no unknown-material fallback for the dense mesher to inherit."))

(defmethod material-contact-surface-assembly
    ((left-kind earth-material-kind) (right-kind earth-material-kind)
     left right)
  (declare (ignore left-kind right-kind))
  (let* ((summary (intern-surface-closure-summary (list left right)))
         (owner
           (higher-ranked-contact-reading
            left right '((:exposed-top . 3) (:exposed-side . 2)
                         (:underside . 1))))
         (guest (if (eq owner left) right left)))
    (cond ((eq owner guest) (face-reading-assembly owner))
          ((eq (surface-reading-role owner) :exposed-top)
           (intern-surface-assembly
            :contact owner :secondary guest :kernel :turf-edge
            :closure-summary summary))
          (t
           (surface-assembly-with-closure-summary
            (face-reading-assembly owner) summary)))))

(defmethod material-contact-surface-assembly
    ((left-kind stone-material-kind) (right-kind stone-material-kind)
     left right)
  (declare (ignore left-kind right-kind))
  (surface-assembly-with-closure-summary
   (face-reading-assembly
    (higher-ranked-contact-reading
     left right '((:architecture . 4) (:foundation . 3)
                  (:natural-rock . 2) (:torch-body . 1))))
   (intern-surface-closure-summary (list left right))))

(defmethod material-contact-surface-assembly
    ((left-kind luminous-material-kind) (right-kind luminous-material-kind)
     left right)
  (declare (ignore left-kind right-kind))
  (surface-assembly-with-closure-summary
   (face-reading-assembly
    (higher-ranked-contact-reading left right '((:crystal . 1))))
   (intern-surface-closure-summary (list left right))))

(defmethod material-contact-surface-assembly
    ((left-kind earth-material-kind) (right-kind stone-material-kind)
     earth stone)
  (declare (ignore left-kind right-kind))
  (earth-set-stone-contact-surface-assembly stone earth))

(defmethod material-contact-surface-assembly
    ((left-kind stone-material-kind) (right-kind earth-material-kind)
     stone earth)
  (declare (ignore left-kind right-kind))
  (earth-set-stone-contact-surface-assembly stone earth))

(defmethod material-contact-surface-assembly
    ((left-kind earth-material-kind) (right-kind luminous-material-kind)
     earth luminous)
  (declare (ignore left-kind right-kind))
  (host-contact-surface-assembly earth luminous))

(defmethod material-contact-surface-assembly
    ((left-kind luminous-material-kind) (right-kind earth-material-kind)
     luminous earth)
  (declare (ignore left-kind right-kind))
  (host-contact-surface-assembly earth luminous))

(defmethod material-contact-surface-assembly
    ((left-kind stone-material-kind) (right-kind luminous-material-kind)
     stone luminous)
  (declare (ignore left-kind right-kind))
  (host-contact-surface-assembly stone luminous))

(defmethod material-contact-surface-assembly
    ((left-kind luminous-material-kind) (right-kind stone-material-kind)
     luminous stone)
  (declare (ignore left-kind right-kind))
  (host-contact-surface-assembly stone luminous))

(defun reading-contact-surface-assembly (left right)
  "Resolve one reading pair through the cold semantic contact protocol."
  (material-contact-surface-assembly
   (surface-reading-kind left) (surface-reading-kind right) left right))

(defun chamfer-surface-assembly (readings)
  "Resolve actual incident READINGS through one canonical closure summary."
  (surface-closure-summary-assembly
   (intern-surface-closure-summary readings)))

(defun highest-ranked-summary-reading (readings role-ranks)
  "Choose one deterministic reading from READINGS using ROLE-RANKS."
  (reduce (lambda (left right)
            (higher-ranked-contact-reading left right role-ranks))
          readings))

(defun surface-closure-host-reading (summary)
  "Choose the appearance owner for SUMMARY through the contact protocol."
  (let ((readings
          (coerce (surface-closure-summary-readings summary) 'list)))
    (reduce
     (lambda (left right)
       (surface-assembly-primary
        (reading-contact-surface-assembly left right)))
     readings)))

(defgeneric material-closure-surface-assembly (host-kind host summary)
  (:documentation
   "Select one renderer appearance for complete incident SUMMARY.

This is a cold semantic protocol.  The material program invokes it while
closing its finite summary algebra; meshing consumes only the resulting dense
stock tables."))

(defmethod material-closure-surface-assembly
    ((host-kind earth-material-kind) host summary)
  (declare (ignore host-kind))
  (let* ((readings
           (coerce (surface-closure-summary-readings summary) 'list))
         (luminous
           (remove-if-not
            (lambda (reading)
              (typep (surface-reading-kind reading)
                     'luminous-material-kind))
            readings))
         (earth
           (remove-if-not
            (lambda (reading)
              (typep (surface-reading-kind reading) 'earth-material-kind))
            readings))
         (appearance
           (cond
             ;; A luminous guest remains explicit material context; it never
             ;; degrades to the soil fallback once a fan joins derived bands.
             (luminous
              (host-contact-surface-assembly
               host
               (highest-ranked-summary-reading
                luminous '((:crystal . 1)))))
             ;; Living earth edges retain the strongest non-top earth reading
             ;; as appearance context.  The full set remains in SUMMARY.
             ((and (eq :exposed-top (surface-reading-role host))
                   (> (length earth) 1))
              (reading-contact-surface-assembly
               host
               (highest-ranked-summary-reading
                (remove host earth :test #'eq)
                '((:exposed-side . 2) (:underside . 1)))))
             (t (face-reading-assembly host)))))
    (surface-assembly-with-closure-summary appearance summary)))

(defmethod material-closure-surface-assembly
    ((host-kind stone-material-kind) host summary)
  (declare (ignore host-kind))
  (let* ((readings
           (coerce (surface-closure-summary-readings summary) 'list))
         (earth
           (remove-if-not
            (lambda (reading)
              (typep (surface-reading-kind reading) 'earth-material-kind))
            readings))
         (luminous
           (remove-if-not
            (lambda (reading)
              (typep (surface-reading-kind reading)
                     'luminous-material-kind))
            readings))
         (appearance
           (cond
             ;; A luminous guest is the visible bezel context at a three-way
             ;; closure too; the complete summary still retains earth for any
             ;; later join.  Stone remains the host in the descriptor.
             (luminous
              (host-contact-surface-assembly
               host
               (highest-ranked-summary-reading
                luminous '((:crystal . 1)))))
             ;; Stone weathering is governed by the deepest earth surface
             ;; which was actually incident, never by descriptor tertiary.
             (earth
              (earth-set-stone-contact-surface-assembly
               host
               (highest-ranked-summary-reading
                earth '((:underside . 3) (:exposed-side . 2)
                        (:exposed-top . 1)))))
             (t (face-reading-assembly host)))))
    (surface-assembly-with-closure-summary appearance summary)))

(defmethod material-closure-surface-assembly
    ((host-kind luminous-material-kind) host summary)
  (declare (ignore host-kind))
  (surface-assembly-with-closure-summary
   (face-reading-assembly host) summary))

(defun surface-closure-summary-assembly (summary)
  "Select and intern the appearance stock for canonical SUMMARY."
  (let ((host (surface-closure-host-reading summary)))
    (material-closure-surface-assembly
     (surface-reading-kind host) host summary)))

(defun closure-surface-assembly (assemblies)
  "Join face or derived band ASSEMBLIES without losing incident provenance."
  (unless assemblies
    (error "A surface closure must have at least one incident assembly."))
  (surface-closure-summary-assembly
   (reduce #'join-surface-closure-summaries assemblies
           :key #'surface-assembly-closure-summary)))

;;; ---------------------------------------------------------------------------
;;; Scene material compilation

(defconstant +material-placement-face-stride+ 7)
(defconstant +material-placement-architecture-flag+ #x01)
(defconstant +material-placement-earth-flag+ #x02)

(defconstant +material-program-no-summary+ #xffff)

(defclass material-program ()
  ((placement-face-stocks :initarg :placement-face-stocks
                          :reader material-program-placement-face-stocks)
   (placement-flags :initarg :placement-flags
                    :reader material-program-placement-flags)
   (assembly-summary-masks
    :initarg :assembly-summary-masks
    :reader material-program-assembly-summary-masks)
   (summary-stocks :initarg :summary-stocks
                   :reader material-program-summary-stocks)
   (summary-count :initarg :summary-count
                  :reader material-program-summary-count)
   (chamfer-algebra :initarg :chamfer-algebra
                    :reader material-program-chamfer-algebra))
  (:documentation
   "Scene-closed dense decisions consumed by face, band, and fan meshing.

ASSEMBLY-SUMMARY-MASKS recovers provenance from any reachable input or
derived stock.  Bitwise OR is the finite associative, commutative, idempotent
set-union algebra; SUMMARY-STOCKS lowers each nonzero joined mask back to its
fully interned appearance assembly."))

(defgeneric compile-material-placement (kind placement)
  (:documentation
   "Compile PLACEMENT once into six oriented readings and one foundation row."))

(defvar *material-placement-compilation-count* 0
  "Dynamically bindable witness for the cold semantic compilation boundary.")

(defmethod compile-material-placement
    ((kind earth-material-kind) placement)
  (incf *material-placement-compilation-count*)
  (let ((side (material-role-surface-reading
               kind placement :exposed-side))
        (top (material-role-surface-reading
              kind placement :exposed-top))
        (underside (material-role-surface-reading
                    kind placement :underside)))
    (vector side side side side top underside nil)))

(defmethod compile-material-placement
    ((kind stone-material-kind) placement)
  (incf *material-placement-compilation-count*)
  (if (eq :terrain-rock (material-placement-role placement))
      (let ((rock
              (material-role-surface-reading
               kind placement :natural-rock)))
        (vector rock rock rock rock rock rock nil))
      (let ((architecture
              (material-role-surface-reading
               kind placement :architecture))
            (foundation
              (material-role-surface-reading
               kind placement :foundation)))
        (vector architecture architecture architecture architecture
                architecture architecture foundation))))

(defmethod compile-material-placement
    ((kind crystal-material-kind) placement)
  (incf *material-placement-compilation-count*)
  (let ((crystal
          (material-role-surface-reading kind placement :crystal)))
    (vector crystal crystal crystal crystal crystal crystal nil)))

(defun compile-material-light-opacity-table (placement-vocabulary)
  "Compile authored placement optics to one entered-cell u8 loss lane."
  (let* ((placements
           (domains:identity-vocabulary-members placement-vocabulary))
         (opacities
           (make-array (length placements) :element-type '(unsigned-byte 8))))
    (loop for placement across placements
          for index from 0
          for opacity = (material-kind-light-opacity
                         (material-placement-kind placement))
          do (unless (typep opacity '(integer 0 15))
               (error "Material ~S has invalid voxel-light opacity ~S."
                      (material-placement-name placement) opacity))
             (setf (aref opacities index) opacity))
    opacities))

(defun material-kind-packed-light-emission (kind)
  (destructuring-bind (red green blue) (material-kind-light-emission kind)
    (luft:pack-voxel-light red green blue)))

(defun compile-material-light-sources (material-cells placement-vocabulary)
  "Return packed RGB4 sources for every emissive authored cell."
  (let ((placements
          (domains:identity-vocabulary-members placement-vocabulary))
        (sources nil))
    (maphash
     (lambda (cell placement-offset)
       (let* ((placement (aref placements placement-offset))
              (emission
                (material-kind-packed-light-emission
                 (material-placement-kind placement))))
         (unless (zerop emission)
           (push (luft:make-voxel-light-source cell emission) sources))))
     material-cells)
    sources))

(defun compile-material-closure-automaton (seed-assemblies)
  "Close SEED-ASSEMBLIES under semantic summary union and compile dense lanes.

Only assemblies reachable from this material program participate.  Unrelated
assemblies retained by the renderer-global ABI cannot enlarge the automaton."
  (let ((seed-by-reading (make-hash-table :test #'eq))
        (readings nil))
    (dolist (assembly seed-assemblies)
      (let* ((summary (surface-assembly-closure-summary assembly))
             (members (surface-closure-summary-readings summary)))
        (unless (= 1 (length members))
          (error "Material closure seed ~S is not a face singleton."
                 assembly))
        (let ((reading (aref members 0)))
          (pushnew reading readings :test #'eq)
          (setf (gethash reading seed-by-reading) assembly))))
    (setf readings
          (sort readings #'string< :key #'surface-closure-reading-order-key))
    (unless readings
      (error "A material program must expose at least one face assembly."))
    (let* ((reading-count (length readings))
           (summary-count (1- (ash 1 reading-count))))
      (when (>= summary-count #x1000)
        (error
         "~D face readings require ~D closure stocks, exceeding the 12-bit ABI."
         reading-count summary-count))
      (let* ((global-count
               (length
                (domains:identity-vocabulary-members
                 *surface-assembly-vocabulary*)))
             (new-stock-upper-bound (- summary-count reading-count)))
        (when (> (+ global-count new-stock-upper-bound) #x1000)
          (error
           "Closing ~D face readings may require ~D new stocks, but only ~D renderer ABI slots remain."
           reading-count new-stock-upper-bound (- #x1000 global-count))))
      (let ((summaries (make-array summary-count))
            (assemblies (make-array summary-count)))
        ;; Offset M-1 denotes exactly the nonempty seed-reading bit set M.
        ;; Enumerating each set once avoids a pairwise fixed-point interning
        ;; pass while preserving the inspectable canonical summary objects.
        (loop for mask from 1 to summary-count
              for offset from 0
              for members =
                (loop for reading in readings
                      for bit from 0
                      when (logbitp bit mask) collect reading)
              for summary = (intern-surface-closure-summary members)
              for assembly =
                (if (= 1 (logcount mask))
                    (or (gethash (first members) seed-by-reading)
                        (error "Material closure lost singleton seed ~S."
                               (first members)))
                    (surface-closure-summary-assembly summary))
              do (unless (eq summary
                              (surface-assembly-closure-summary assembly))
                   (error "Closure appearance ~S did not retain its summary."
                          assembly))
                 (setf (aref summaries offset) summary
                       (aref assemblies offset) assembly))
        (let* ((maximum-stock
                 (loop for assembly across assemblies
                       maximize (surface-assembly-offset assembly)))
               (stock-summary-masks
                 (make-array (1+ maximum-stock)
                             :element-type '(unsigned-byte 16)
                             :initial-element +material-program-no-summary+))
               (summary-stocks
                 (make-array (1+ summary-count)
                             :element-type '(unsigned-byte 16))))
          (loop for summary across summaries
                for assembly across assemblies
                for summary-mask from 1
                for stock = (surface-assembly-offset assembly)
                do (unless (= +material-program-no-summary+
                              (aref stock-summary-masks stock))
                     (error
                      "Assembly stock ~D represents more than one closure summary."
                      stock))
                   (setf (aref stock-summary-masks stock) summary-mask
                         (aref summary-stocks summary-mask) stock)
                   (unless (eq summary
                               (surface-assembly-closure-summary
                                (surface-assembly-at stock)))
                     (error
                      "Assembly stock ~D does not round-trip its closure summary."
                      stock)))
          ;; A nonzero summary mask is the actual seed-reading set.  LOGIOR in
          ;; the hot resolver is therefore the concrete ACI join, without a
          ;; redundant quadratic table.
          (values
           stock-summary-masks summary-stocks summary-count))))))

(defun make-material-program
    (placement-vocabulary
     &key (active-placement-offsets nil active-placement-offsets-supplied-p))
  "Bind semantic placement and assembly meaning once into dense stock tables.

ACTIVE-PLACEMENT-OFFSETS, when supplied, is a duplicate-free scene-local set
of authored placements which can actually reach meshing.  Every placement
still receives its face rows, but unrelated vocabulary members do not enlarge
the closure automaton.  An explicitly empty set uses placement zero as an
inert, callable empty-scene fallback.  Omitting the keyword retains the
all-members inspection/test convention."
  (let* ((placements
           (domains:identity-vocabulary-members placement-vocabulary))
         (placement-count (length placements))
         (active-placements
           (when active-placement-offsets-supplied-p
             (unless (plusp placement-count)
               (error "A material program requires a nonempty placement vocabulary."))
             (let ((active (make-hash-table :test #'eql)))
               (dolist (offset (or active-placement-offsets (list 0)) active)
                 (unless (and (integerp offset)
                              (<= 0 offset) (< offset placement-count))
                   (error "Active material placement offset ~S is outside 0..~D."
                          offset (1- placement-count)))
                 (when (gethash offset active)
                   (error "Duplicate active material placement offset ~D."
                          offset))
                 (setf (gethash offset active) t)))))
         (placement-readings (make-array placement-count))
         (placement-face-stocks
           (make-array (* placement-count +material-placement-face-stride+)
                       :element-type '(unsigned-byte 16)))
         (placement-flags
           (make-array placement-count :element-type '(unsigned-byte 8)
                                       :initial-element 0))
         (seed-assemblies nil))
    ;; Placement dispatch happens once here, never in dense meshing.
    (loop for placement across placements
          for placement-offset from 0
          for readings = (compile-material-placement
                          (material-placement-kind placement) placement)
          do (setf (aref placement-readings placement-offset) readings)
             (when (eq :architecture (material-placement-role placement))
               (setf (aref placement-flags placement-offset)
                     (logior (aref placement-flags placement-offset)
                             +material-placement-architecture-flag+)))
             (when (typep (material-placement-kind placement)
                          'earth-material-kind)
               (setf (aref placement-flags placement-offset)
                     (logior (aref placement-flags placement-offset)
                             +material-placement-earth-flag+)))
             (loop for reading across readings
                   for face from 0
                   when reading do
                     (let ((assembly (face-reading-assembly reading)))
                       (setf (aref placement-face-stocks
                                   (+ (* placement-offset
                                         +material-placement-face-stride+)
                                      face))
                             (surface-assembly-offset assembly))
                       (when (or (null active-placements)
                                 (gethash placement-offset active-placements))
                         (pushnew assembly seed-assemblies :test #'eq)))))
    (multiple-value-bind (assembly-summary-masks summary-stocks summary-count)
        (compile-material-closure-automaton seed-assemblies)
      (make-instance
       'material-program
       :placement-face-stocks placement-face-stocks
       :placement-flags placement-flags
       :assembly-summary-masks assembly-summary-masks
       :summary-stocks summary-stocks
       :summary-count summary-count
       :chamfer-algebra
       (luft:make-compiled-chamfer-algebra
        assembly-summary-masks summary-stocks summary-count)))))

(defun make-compiled-material-chamfer-stock-function (program)
  "Capture PROGRAM's dense lanes as one dispatch-free chamfer resolver."
  (let ((assembly-summary-masks
          (the (simple-array (unsigned-byte 16) (*))
               (material-program-assembly-summary-masks program)))
        (summary-stocks
          (the (simple-array (unsigned-byte 16) (*))
               (material-program-summary-stocks program)))
        (summary-count
          (the (integer 1 #xffff)
               (material-program-summary-count program))))
    (lambda (stocks)
      (declare (optimize (speed 3) (safety 1)))
      (unless stocks
        (error "A compiled material chamfer cannot be empty."))
      (let* ((first-stock (the (unsigned-byte 16) (first stocks)))
             (summary-mask (aref assembly-summary-masks first-stock)))
        (when (= summary-mask +material-program-no-summary+)
          (error "Assembly stock ~D is outside this material program."
                 first-stock))
        (dolist (stock (rest stocks))
          (declare (type (unsigned-byte 16) stock))
          (let ((right (aref assembly-summary-masks stock)))
            (when (= right +material-program-no-summary+)
              (error "Assembly stock ~D is outside this material program."
                     stock))
            (setf summary-mask (logior summary-mask right))))
        (when (> summary-mask summary-count)
          (error "Material closure mask ~D exceeds this material program."
                 summary-mask))
        (aref summary-stocks summary-mask)))))

(defun compiled-material-chamfer-stock (program stocks)
  "Resolve STOCKS through a freshly captured PROGRAM, for inspection/tests."
  (funcall (make-compiled-material-chamfer-stock-function program) stocks))
