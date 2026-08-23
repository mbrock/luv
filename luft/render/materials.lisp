(in-package #:luft.render)

;;; Semantic material definitions live here, above scene storage and mesh
;;; realization.  The objects are the inspectable vocabulary; cells and GPU
;;; instances carry only dense offsets closed by an owning vocabulary.

(defclass material-kind ()
  ((name :initarg :name :reader material-kind-name)
   (base-tone :initarg :base-tone :reader material-kind-base-tone)
   (roughness :initarg :roughness :reader material-kind-roughness)
   (relief :initarg :relief :reader material-kind-relief))
  (:documentation "A reusable substance and its renderer-facing response."))

(defclass earth-material-kind (material-kind) ())
(defclass stone-material-kind (material-kind) ())

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

(defclass surface-assembly ()
  ((name :initarg :name :reader surface-assembly-name)
   (relation :initarg :relation :reader surface-assembly-relation)
   (primary :initarg :primary :reader surface-assembly-primary)
   (secondary :initarg :secondary :initform nil
              :reader surface-assembly-secondary)
   (tertiary :initarg :tertiary :initform nil
             :reader surface-assembly-tertiary)
   (kernel :initarg :kernel :reader surface-assembly-kernel))
  (:documentation
   "An interned face, band, or fan material relation compiled for rendering."))

(defun ensure-semantic-instance (current class &rest initargs)
  "Reinitialize CURRENT when it still has CLASS, preserving live identity."
  (if (and current (eq (class-of current) (find-class class)))
      (progn (apply #'reinitialize-instance current initargs) current)
      (apply #'make-instance class initargs)))

(defvar *earth-material* nil)
(defvar *limestone-material* nil)
(defvar *world-material-frame* nil)
(defvar *sanctuary-material-frame* nil)
(defvar *beacon-material-frame* nil)
(defvar *terrain-material-placement* nil)
(defvar *sanctuary-material-placement* nil)
(defvar *beacon-material-placement* nil)

(setf *earth-material*
      (ensure-semantic-instance
       *earth-material* 'earth-material-kind
       :name :earth :base-tone '(0.42 0.32 0.21)
       :roughness 0.92 :relief :granular)
      *limestone-material*
      (ensure-semantic-instance
       *limestone-material* 'stone-material-kind
       :name :limestone :base-tone '(0.53 0.49 0.39)
       :roughness 0.78 :relief :weathered-stone)
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
      *sanctuary-material-placement*
      (ensure-semantic-instance
       *sanctuary-material-placement* 'material-placement
       :name :sanctuary-limestone :kind *limestone-material* :finish :dressed
       :frame *sanctuary-material-frame* :role :architecture)
      *beacon-material-placement*
      (ensure-semantic-instance
       *beacon-material-placement* 'material-placement
       :name :ridge-beacon-limestone :kind *limestone-material* :finish :dressed
       :frame *beacon-material-frame* :role :architecture))

(defvar *grass-reading* nil)
(defvar *soil-reading* nil)
(defvar *subsoil-reading* nil)
(defvar *stone-reading* nil)
(defvar *foundation-stone-reading* nil)

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
       :role :foundation))

(flet ((seed (placement &rest readings)
         (clrhash (material-placement-readings placement))
         (dolist (reading readings)
           (setf (gethash (surface-reading-role reading)
                          (material-placement-readings placement))
                 reading))))
  ;; Seeding retains the first nine assembly identities as a rebuild oracle;
  ;; any later placement receives the same semantic readings in its own frame.
  (seed *terrain-material-placement*
        *grass-reading* *soil-reading* *subsoil-reading*)
  (seed *sanctuary-material-placement*
        *stone-reading* *foundation-stone-reading*)
  (seed *beacon-material-placement*))

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

(defun ensure-surface-assembly
    (current name relation primary &key secondary tertiary kernel)
  (ensure-semantic-instance
   current 'surface-assembly :name name :relation relation :primary primary
   :secondary secondary :tertiary tertiary :kernel kernel))

(defvar *grass-surface* nil)
(defvar *soil-surface* nil)
(defvar *subsoil-surface* nil)
(defvar *stone-surface* nil)
(defvar *turf-set-stone-surface* nil)
(defvar *soil-set-stone-surface* nil)
(defvar *deep-set-stone-surface* nil)
(defvar *turf-edge-surface* nil)
(defvar *foundation-stone-surface* nil)

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
       :secondary *grass-reading* :tertiary *subsoil-reading*
       :kernel :earth-set-stone)
      *soil-set-stone-surface*
      (ensure-surface-assembly
       *soil-set-stone-surface* :soil-set-limestone :contact *stone-reading*
       :secondary *soil-reading* :tertiary *subsoil-reading*
       :kernel :earth-set-stone)
      *deep-set-stone-surface*
      (ensure-surface-assembly
       *deep-set-stone-surface* :deep-set-limestone :contact *stone-reading*
       :secondary *subsoil-reading* :tertiary *subsoil-reading*
       :kernel :earth-set-stone)
      *turf-edge-surface*
      (ensure-surface-assembly
       *turf-edge-surface* :turf-edge :contact *grass-reading*
       :secondary *soil-reading* :kernel :turf-edge)
      *foundation-stone-surface*
      (ensure-surface-assembly
       *foundation-stone-surface* :foundation-limestone :face
       *foundation-stone-reading* :secondary *soil-reading*
       :tertiary *subsoil-reading* :kernel :foundation-stone))

(defparameter *surface-assembly-vocabulary*
  (domains:make-identity-vocabulary-domain
   :members (list *grass-surface* *soil-surface* *subsoil-surface*
                  *stone-surface* *turf-set-stone-surface*
                  *soil-set-stone-surface* *deep-set-stone-surface*
                  *turf-edge-surface* *foundation-stone-surface*)
   :limit #x1000)
  "The assembly domain; its first nine offsets retain the legacy GPU oracle.")

(defun surface-assembly-offset (assembly)
  (domains:identity-vocabulary-offset *surface-assembly-vocabulary* assembly))

(defun surface-assembly-at (offset)
  (domains:identity-vocabulary-member *surface-assembly-vocabulary* offset))

;; The first nine vocabulary members are the stable renderer ABI.  The tests
;; rebuild this exact prefix from the semantic objects, so these literals are
;; available while this file itself is being compiled in a clean image.
(defconstant +grass-stock+ 0)
(defconstant +soil-stock+ 1)
(defconstant +subsoil-stock+ 2)
(defconstant +stone-stock+ 3)
(defconstant +turf-set-stone-stock+ 4)
(defconstant +soil-set-stone-stock+ 5)
(defconstant +deep-set-stone-stock+ 6)
(defconstant +turf-edge-stock+ 7)
(defconstant +foundation-stone-stock+ 8)

(defconstant +surface-assembly-descriptor-row-count+ 7)

(defun surface-kernel-code (kernel)
  "Compile the intentionally closed shader-kernel ABI."
  (ecase kernel
    (:grass 7)
    (:soil 5)
    (:subsoil 6)
    (:stone 4)
    (:earth-set-stone 1)
    (:turf-edge 2)
    (:foundation-stone 3)))

(defun material-relief-code (relief)
  "Compile the closed procedural-relief ABI shared by all surface kernels."
  (ecase relief
    (:granular 1)
    (:weathered-stone 2)))

(defun material-relief-amplitude (relief)
  (ecase relief
    (:granular 0.028)
    (:weathered-stone 0.020)))

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

Each assembly owns seven vec4 rows: primary/kernel, secondary/contact variant,
tertiary/roughness, then frame origin/relief profile and its three axes (with
relief amplitude beside X). The fixed stride is small enough for direct
indexing while leaving material meaning on the CPU."
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
            for frame = (surface-reading-frame primary)
            for row = (* index +surface-assembly-descriptor-row-count+)
            do (put-row row
                        (append (tone-of primary primary)
                                (list (surface-kernel-code
                                       (surface-assembly-kernel assembly)))))
               (put-row (+ row 1)
                        (append (tone-of secondary primary)
                                (list (surface-contact-variant assembly))))
               (put-row (+ row 2)
                        (append (tone-of tertiary secondary)
                                (list (material-kind-roughness
                                       (surface-reading-kind primary)))))
               (put-row (+ row 3)
                        (append
                         (material-frame-origin frame)
                         (list (material-relief-code
                                (material-kind-relief
                                 (surface-reading-kind primary))))))
               (loop for axis in (material-frame-axes frame)
                     for axis-row from (+ row 4)
                     for amplitude = (material-relief-amplitude
                                      (material-kind-relief
                                       (surface-reading-kind primary)))
                     do (put-row axis-row
                                 (append axis
                                         (list (if (= axis-row (+ row 4))
                                                   amplitude 0.0)))))))
    words))

(defun make-scene-material-vocabulary ()
  "Return the authored placement vocabulary shared by one scene's cells."
  (domains:make-identity-vocabulary-domain
   :members (list *terrain-material-placement* *sanctuary-material-placement*)
   :limit #x10000))

(defgeneric material-face-reading (kind placement scene cell axis side)
  (:documentation
   "Derive one exposed face reading from an authored material placement."))

(defmethod material-face-reading
    ((kind earth-material-kind) placement scene cell axis side)
  (declare (ignore kind scene cell))
  (cond ((not (eq axis :z))
         (placement-surface-reading
          placement :exposed-side :soil '(0.42 0.32 0.21) :cut))
        ((eq side :backward)
         (placement-surface-reading
          placement :exposed-top :grass '(0.18 0.31 0.105) :living))
        (t
         (placement-surface-reading
          placement :underside :subsoil '(0.24 0.18 0.13) :broken))))

(defmethod material-face-reading
    ((kind stone-material-kind) placement scene cell axis side)
  (declare (ignore kind axis side))
  (if (scene-foundation-cell-p scene cell)
      (placement-surface-reading
       placement :foundation :foundation-limestone '(0.53 0.49 0.39)
       :earth-weathered)
      (placement-surface-reading
       placement :architecture :dressed-limestone '(0.53 0.49 0.39)
       :dressed)))

(defun find-surface-assembly
    (relation primary secondary tertiary kernel)
  (find-if (lambda (assembly)
             (and (eq relation (surface-assembly-relation assembly))
                  (eq primary (surface-assembly-primary assembly))
                  (eq secondary (surface-assembly-secondary assembly))
                  (eq tertiary (surface-assembly-tertiary assembly))
                  (eq kernel (surface-assembly-kernel assembly))))
           (domains:identity-vocabulary-members
            *surface-assembly-vocabulary*)))

(defun intern-surface-assembly
    (relation primary &key secondary tertiary kernel name)
  "Intern a small semantic assembly at mesh-compilation time."
  (or (find-surface-assembly relation primary secondary tertiary kernel)
      (let ((assembly
              (make-instance
               'surface-assembly :name (or name
                                           (list relation
                                                 (surface-reading-name primary)))
               :relation relation :primary primary :secondary secondary
               :tertiary tertiary :kernel kernel)))
        (surface-assembly-offset assembly)
        assembly)))

(defun face-reading-assembly (reading)
  (cond ((eq reading *grass-reading*) *grass-surface*)
        ((eq reading *soil-reading*) *soil-surface*)
        ((eq reading *subsoil-reading*) *subsoil-surface*)
        ((eq reading *stone-reading*) *stone-surface*)
        ((eq reading *foundation-stone-reading*) *foundation-stone-surface*)
        (t
         (intern-surface-assembly
          :face reading
          :secondary (and (eq (surface-reading-role reading) :foundation)
                          *soil-reading*)
          :tertiary (and (eq (surface-reading-role reading) :foundation)
                         *subsoil-reading*)
          :kernel (case (surface-reading-role reading)
                    (:exposed-top :grass)
                    (:exposed-side :soil)
                    (:underside :subsoil)
                    (:foundation :foundation-stone)
                    (otherwise :stone))))))

(defun stone-reading-p (reading)
  (typep (surface-reading-kind reading) 'stone-material-kind))

(defun chamfer-surface-assembly (readings)
  "Resolve incident face READINGS as one current-width surface assembly."
  (labels ((role-reading (role)
             (find role readings :key #'surface-reading-role))
           (stone-reading () (find-if #'stone-reading-p readings))
           (earth-set (earth)
             (let ((stone (stone-reading)))
               (intern-surface-assembly
                :contact stone :secondary earth :tertiary *subsoil-reading*
                :kernel :earth-set-stone))))
    (cond ((and (stone-reading) (role-reading :underside))
           (earth-set (role-reading :underside)))
          ((and (stone-reading) (role-reading :exposed-side))
           (earth-set (role-reading :exposed-side)))
          ((and (stone-reading) (role-reading :exposed-top))
           (earth-set (role-reading :exposed-top)))
          ((every (lambda (reading) (eq reading (first readings)))
                  (rest readings))
           (face-reading-assembly (first readings)))
          ((stone-reading) (face-reading-assembly (stone-reading)))
          ((and (role-reading :exposed-top)
                (or (role-reading :exposed-side)
                    (role-reading :underside)))
           *turf-edge-surface*)
          (t *soil-surface*))))

(defun closure-surface-assembly (assemblies)
  "Resolve an edge or fan closure without erasing incident assembly identity.

Fan construction may present assemblies already derived for surrounding bands;
those are not interchangeable with their primary face readings. This resolver
therefore preserves the legacy recursive decision exactly while expressing it
in semantic identities rather than numeric ranges."
  (flet ((stone-face-p (assembly)
           (member assembly (list *stone-surface*
                                  *foundation-stone-surface*))))
    (if (every (lambda (assembly)
                 (member assembly
                         (list *grass-surface* *soil-surface* *subsoil-surface*
                               *stone-surface* *turf-set-stone-surface*
                               *soil-set-stone-surface* *deep-set-stone-surface*
                               *turf-edge-surface* *foundation-stone-surface*)))
               assemblies)
        (cond ((and (some #'stone-face-p assemblies)
                    (member *subsoil-surface* assemblies))
               *deep-set-stone-surface*)
              ((and (some #'stone-face-p assemblies)
                    (member *soil-surface* assemblies))
               *soil-set-stone-surface*)
              ((and (some #'stone-face-p assemblies)
                    (member *grass-surface* assemblies))
               *turf-set-stone-surface*)
              ((every (lambda (assembly) (eq assembly (first assemblies)))
                      (rest assemblies))
               (first assemblies))
              ((some #'stone-face-p assemblies) *stone-surface*)
              ((and (member *grass-surface* assemblies)
                    (or (member *soil-surface* assemblies)
                        (member *subsoil-surface* assemblies)))
               *turf-edge-surface*)
              (t *soil-surface*))
        (chamfer-surface-assembly
         (mapcar #'surface-assembly-primary assemblies)))))

;;; ---------------------------------------------------------------------------
;;; Scene material compilation

(defconstant +material-placement-face-stride+ 7)
(defconstant +material-placement-architecture-flag+ #x01)
(defconstant +material-placement-earth-flag+ #x02)

(defconstant +assembly-legacy-flag+ #x01)
(defconstant +assembly-legacy-stone-flag+ #x02)
(defconstant +assembly-legacy-grass-flag+ #x04)
(defconstant +assembly-legacy-soil-flag+ #x08)
(defconstant +assembly-legacy-subsoil-flag+ #x10)

(defconstant +assembly-primary-stone-flag+ #x01)
(defconstant +assembly-primary-grass-flag+ #x02)
(defconstant +assembly-primary-soil-flag+ #x04)
(defconstant +assembly-primary-subsoil-flag+ #x08)

(defclass material-program ()
  ((placement-face-stocks :initarg :placement-face-stocks
                          :reader material-program-placement-face-stocks)
   (placement-flags :initarg :placement-flags
                    :reader material-program-placement-flags)
   (assembly-flags :initarg :assembly-flags
                   :reader material-program-assembly-flags)
   (assembly-primary-flags :initarg :assembly-primary-flags
                           :reader material-program-assembly-primary-flags)
   (assembly-primary-reading-offsets
    :initarg :assembly-primary-reading-offsets
    :reader material-program-assembly-primary-reading-offsets)
   (assembly-face-stocks :initarg :assembly-face-stocks
                         :reader material-program-assembly-face-stocks)
   (reading-contact-stocks :initarg :reading-contact-stocks
                           :reader material-program-reading-contact-stocks)
   (reading-count :initarg :reading-count
                  :reader material-program-reading-count))
  (:documentation
   "Scene-closed dense decisions consumed by face, band, and fan meshing."))

(defgeneric compile-material-placement (kind placement)
  (:documentation
   "Compile PLACEMENT once into six oriented readings and one foundation row."))

(defvar *material-placement-compilation-count* 0
  "Dynamically bindable witness for the cold semantic compilation boundary.")

(defmethod compile-material-placement
    ((kind earth-material-kind) placement)
  (declare (ignore kind))
  (incf *material-placement-compilation-count*)
  (let ((side (placement-surface-reading
               placement :exposed-side :soil '(0.42 0.32 0.21) :cut))
        (top (placement-surface-reading
              placement :exposed-top :grass '(0.18 0.31 0.105) :living))
        (underside (placement-surface-reading
                    placement :underside :subsoil '(0.24 0.18 0.13) :broken)))
    (vector side side side side top underside nil)))

(defmethod compile-material-placement
    ((kind stone-material-kind) placement)
  (declare (ignore kind))
  (incf *material-placement-compilation-count*)
  (let ((architecture
          (placement-surface-reading
           placement :architecture :dressed-limestone '(0.53 0.49 0.39)
           :dressed))
        (foundation
          (placement-surface-reading
           placement :foundation :foundation-limestone '(0.53 0.49 0.39)
           :earth-weathered)))
    (vector architecture architecture architecture architecture
            architecture architecture foundation)))

(defun legacy-surface-assembly-p (assembly)
  (member assembly
          (list *grass-surface* *soil-surface* *subsoil-surface*
                *stone-surface* *turf-set-stone-surface*
                *soil-set-stone-surface* *deep-set-stone-surface*
                *turf-edge-surface* *foundation-stone-surface*)))

(defun ensure-reading-closure-assemblies (reading)
  "Intern the face result that the dense resolver may select for READING."
  (face-reading-assembly reading))

(defun earth-contact-reading-p (reading)
  (and (typep (surface-reading-kind reading) 'earth-material-kind)
       (member (surface-reading-role reading)
               '(:exposed-top :exposed-side :underside))))

(defun make-material-program (placement-vocabulary)
  "Bind semantic placement and assembly meaning once into dense stock tables."
  (let* ((placements
           (domains:identity-vocabulary-members placement-vocabulary))
         (placement-count (length placements))
         (placement-readings (make-array placement-count))
         (placement-face-stocks
           (make-array (* placement-count +material-placement-face-stride+)
                       :element-type '(unsigned-byte 16)))
         (placement-flags
           (make-array placement-count :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
    ;; This is the only material-kind generic dispatch in scene compilation.
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
                   when reading do (ensure-reading-closure-assemblies reading)))
    ;; A previous scene may have extended the renderer-global assembly ABI.
    ;; Close those primaries too before fixing this program's array bounds.
    (loop for assembly across
          (copy-seq
           (domains:identity-vocabulary-members *surface-assembly-vocabulary*))
          do (ensure-reading-closure-assemblies
              (surface-assembly-primary assembly)))
    ;; Intern the cross-product of authored stone and earth readings once.  A
    ;; contact then retains both placements' tone and frame without a hot hash.
    (let ((primary-readings
            (make-array 16 :adjustable t :fill-pointer 0))
          (primary-reading-offsets (make-hash-table :test #'eq)))
      (loop for assembly across
            (domains:identity-vocabulary-members
             *surface-assembly-vocabulary*)
            for reading = (surface-assembly-primary assembly)
            unless (gethash reading primary-reading-offsets) do
              (setf (gethash reading primary-reading-offsets)
                    (length primary-readings))
              (vector-push-extend reading primary-readings))
      (loop for stone across primary-readings
            when (stone-reading-p stone) do
              (loop for earth across primary-readings
                    when (earth-contact-reading-p earth) do
                      (chamfer-surface-assembly (list stone earth))))
      ;; All assembly interning is complete before the arrays receive their size.
      (let* ((assemblies
               (domains:identity-vocabulary-members
                *surface-assembly-vocabulary*))
             (assembly-count (length assemblies))
             (reading-count (length primary-readings))
             (assembly-flags
               (make-array assembly-count :element-type '(unsigned-byte 8)
                                          :initial-element 0))
             (primary-flags
               (make-array assembly-count :element-type '(unsigned-byte 8)
                                          :initial-element 0))
             (assembly-reading-offsets
               (make-array assembly-count :element-type '(unsigned-byte 16)))
             (face-stocks
               (make-array assembly-count :element-type '(unsigned-byte 16)))
             (reading-contacts
               (make-array (* reading-count reading-count)
                           :element-type '(unsigned-byte 16)
                           :initial-element +soil-stock+)))
        (loop for readings across placement-readings
              for placement-offset from 0
              do (loop for reading across readings
                       for face from 0
                       when reading do
                         (setf (aref placement-face-stocks
                                     (+ (* placement-offset
                                           +material-placement-face-stride+)
                                        face))
                               (surface-assembly-offset
                                (face-reading-assembly reading)))))
        (loop for assembly across assemblies
              for stock from 0
              for primary = (surface-assembly-primary assembly)
              for role = (surface-reading-role primary)
              for stone-p = (stone-reading-p primary)
              do (setf (aref assembly-reading-offsets stock)
                       (gethash primary primary-reading-offsets)
                       (aref face-stocks stock)
                       (surface-assembly-offset
                        (face-reading-assembly primary)))
                 (when (legacy-surface-assembly-p assembly)
                   (setf (aref assembly-flags stock)
                         (logior +assembly-legacy-flag+
                                 (cond
                                   ((member assembly
                                            (list *stone-surface*
                                                  *foundation-stone-surface*))
                                    +assembly-legacy-stone-flag+)
                                   ((eq assembly *grass-surface*)
                                    +assembly-legacy-grass-flag+)
                                   ((eq assembly *soil-surface*)
                                    +assembly-legacy-soil-flag+)
                                   ((eq assembly *subsoil-surface*)
                                    +assembly-legacy-subsoil-flag+)
                                   (t 0)))))
                 (setf (aref primary-flags stock)
                       (cond (stone-p +assembly-primary-stone-flag+)
                             ((eq role :exposed-top)
                              +assembly-primary-grass-flag+)
                             ((eq role :exposed-side)
                              +assembly-primary-soil-flag+)
                             ((eq role :underside)
                              +assembly-primary-subsoil-flag+)
                             (t 0))))
        (loop for stone across primary-readings
              for stone-offset from 0
              when (stone-reading-p stone) do
                (loop for earth across primary-readings
                      for earth-offset from 0
                      when (earth-contact-reading-p earth) do
                        (setf (aref reading-contacts
                                    (+ (* stone-offset reading-count)
                                       earth-offset))
                              (surface-assembly-offset
                               (chamfer-surface-assembly
                                (list stone earth))))))
        (make-instance
         'material-program
         :placement-face-stocks placement-face-stocks
         :placement-flags placement-flags
         :assembly-flags assembly-flags
         :assembly-primary-flags primary-flags
         :assembly-primary-reading-offsets assembly-reading-offsets
         :assembly-face-stocks face-stocks
         :reading-contact-stocks reading-contacts
         :reading-count reading-count)))))

(defun make-compiled-material-chamfer-stock-function (program)
  "Capture PROGRAM's dense lanes as one dispatch-free chamfer resolver."
  (let ((assembly-flags
          (the (simple-array (unsigned-byte 8) (*))
               (material-program-assembly-flags program)))
        (primary-flags
          (the (simple-array (unsigned-byte 8) (*))
               (material-program-assembly-primary-flags program)))
        (face-stocks
          (the (simple-array (unsigned-byte 16) (*))
               (material-program-assembly-face-stocks program)))
        (reading-offsets
          (the (simple-array (unsigned-byte 16) (*))
               (material-program-assembly-primary-reading-offsets program)))
        (reading-contacts
          (the (simple-array (unsigned-byte 16) (*))
               (material-program-reading-contact-stocks program)))
        (reading-count
          (the (integer 0 #x1000)
               (material-program-reading-count program))))
    (lambda (stocks)
      (declare (optimize (speed 3) (safety 1)))
      (let ((all-legacy-p t)
            (same-stock-p t)
            (same-primary-p t)
            (first-stock (the (unsigned-byte 16) (first stocks)))
            (first-primary (aref face-stocks (first stocks)))
            legacy-stone legacy-grass-p legacy-soil-p legacy-subsoil-p
            primary-stone primary-grass primary-soil primary-subsoil)
        (dolist (stock stocks)
          (declare (type (unsigned-byte 16) stock))
          (let ((legacy (aref assembly-flags stock))
                (primary (aref primary-flags stock)))
            (unless (logtest +assembly-legacy-flag+ legacy)
              (setf all-legacy-p nil))
            (unless (= stock first-stock) (setf same-stock-p nil))
            (unless (= (aref face-stocks stock) first-primary)
              (setf same-primary-p nil))
            (when (logtest +assembly-legacy-stone-flag+ legacy)
              (unless legacy-stone (setf legacy-stone stock)))
            (when (logtest +assembly-legacy-grass-flag+ legacy)
              (setf legacy-grass-p t))
            (when (logtest +assembly-legacy-soil-flag+ legacy)
              (setf legacy-soil-p t))
            (when (logtest +assembly-legacy-subsoil-flag+ legacy)
              (setf legacy-subsoil-p t))
            (when (logtest +assembly-primary-stone-flag+ primary)
              (unless primary-stone (setf primary-stone stock)))
            (when (logtest +assembly-primary-grass-flag+ primary)
              (unless primary-grass (setf primary-grass stock)))
            (when (logtest +assembly-primary-soil-flag+ primary)
              (unless primary-soil (setf primary-soil stock)))
            (when (logtest +assembly-primary-subsoil-flag+ primary)
              (unless primary-subsoil (setf primary-subsoil stock)))))
        (if all-legacy-p
            (cond ((and legacy-stone legacy-subsoil-p)
                   +deep-set-stone-stock+)
                  ((and legacy-stone legacy-soil-p) +soil-set-stone-stock+)
                  ((and legacy-stone legacy-grass-p) +turf-set-stone-stock+)
                  (same-stock-p first-stock)
                  (legacy-stone +stone-stock+)
                  ((and legacy-grass-p (or legacy-soil-p legacy-subsoil-p))
                   +turf-edge-stock+)
                  (t +soil-stock+))
            (flet ((contact (stone earth)
                     (aref reading-contacts
                           (+ (* (aref reading-offsets stone) reading-count)
                              (aref reading-offsets earth)))))
              (cond ((and primary-stone primary-subsoil)
                     (contact primary-stone primary-subsoil))
                    ((and primary-stone primary-soil)
                     (contact primary-stone primary-soil))
                    ((and primary-stone primary-grass)
                     (contact primary-stone primary-grass))
                    (same-primary-p first-primary)
                    (primary-stone (aref face-stocks primary-stone))
                    ((and primary-grass (or primary-soil primary-subsoil))
                     +turf-edge-stock+)
                    (t +soil-stock+))))))))

(defun compiled-material-chamfer-stock (program stocks)
  "Resolve STOCKS through a freshly captured PROGRAM, for inspection/tests."
  (funcall (make-compiled-material-chamfer-stock-function program) stocks))
