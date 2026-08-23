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
