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
   (role :initarg :role :reader material-placement-role))
  (:documentation
   "Authored cell meaning: substance, deliberate finish, frame, and role."))

(defclass surface-reading ()
  ((name :initarg :name :reader surface-reading-name)
   (kind :initarg :kind :reader surface-reading-kind)
   (finish :initarg :finish :reader surface-reading-finish)
   (role :initarg :role :reader surface-reading-role))
  (:documentation
   "One exposed face's derived interpretation of an authored placement."))

(defclass surface-assembly ()
  ((name :initarg :name :reader surface-assembly-name)
   (relation :initarg :relation :reader surface-assembly-relation)
   (primary :initarg :primary :reader surface-assembly-primary)
   (secondary :initarg :secondary :initform nil
              :reader surface-assembly-secondary)
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
(defvar *terrain-material-placement* nil)
(defvar *sanctuary-material-placement* nil)

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
       :name :sanctuary :origin '(30 54 19)
       :axes '((1 0 0) (0 1 0) (0 0 1)))
      *terrain-material-placement*
      (ensure-semantic-instance
       *terrain-material-placement* 'material-placement
       :name :terrain :kind *earth-material* :finish :living
       :frame *world-material-frame* :role :terrain)
      *sanctuary-material-placement*
      (ensure-semantic-instance
       *sanctuary-material-placement* 'material-placement
       :name :sanctuary-limestone :kind *limestone-material* :finish :dressed
       :frame *sanctuary-material-frame* :role :architecture))

(defvar *grass-reading* nil)
(defvar *soil-reading* nil)
(defvar *subsoil-reading* nil)
(defvar *stone-reading* nil)
(defvar *foundation-stone-reading* nil)

(setf *grass-reading*
      (ensure-semantic-instance
       *grass-reading* 'surface-reading :name :grass
       :kind *earth-material* :finish :living :role :exposed-top)
      *soil-reading*
      (ensure-semantic-instance
       *soil-reading* 'surface-reading :name :soil
       :kind *earth-material* :finish :cut :role :exposed-side)
      *subsoil-reading*
      (ensure-semantic-instance
       *subsoil-reading* 'surface-reading :name :subsoil
       :kind *earth-material* :finish :broken :role :underside)
      *stone-reading*
      (ensure-semantic-instance
       *stone-reading* 'surface-reading :name :dressed-limestone
       :kind *limestone-material* :finish :dressed :role :architecture)
      *foundation-stone-reading*
      (ensure-semantic-instance
       *foundation-stone-reading* 'surface-reading :name :foundation-limestone
       :kind *limestone-material* :finish :earth-weathered :role :foundation))

(defun ensure-surface-assembly
    (current name relation primary &key secondary kernel)
  (ensure-semantic-instance
   current 'surface-assembly :name name :relation relation :primary primary
   :secondary secondary :kernel kernel))

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
       :secondary *grass-reading* :kernel :earth-set-stone)
      *soil-set-stone-surface*
      (ensure-surface-assembly
       *soil-set-stone-surface* :soil-set-limestone :contact *stone-reading*
       :secondary *soil-reading* :kernel :earth-set-stone)
      *deep-set-stone-surface*
      (ensure-surface-assembly
       *deep-set-stone-surface* :deep-set-limestone :contact *stone-reading*
       :secondary *subsoil-reading* :kernel :earth-set-stone)
      *turf-edge-surface*
      (ensure-surface-assembly
       *turf-edge-surface* :turf-edge :contact *grass-reading*
       :secondary *soil-reading* :kernel :turf-edge)
      *foundation-stone-surface*
      (ensure-surface-assembly
       *foundation-stone-surface* :foundation-limestone :face
       *foundation-stone-reading* :kernel :foundation-stone))

(defparameter *surface-assembly-vocabulary*
  (domains:make-identity-vocabulary-domain
   :members (list *grass-surface* *soil-surface* *subsoil-surface*
                  *stone-surface* *turf-set-stone-surface*
                  *soil-set-stone-surface* *deep-set-stone-surface*
                  *turf-edge-surface* *foundation-stone-surface*)
   :limit 16)
  "The legacy-width assembly domain; its order is the current GPU oracle.")

(defun surface-assembly-offset (assembly)
  (domains:identity-vocabulary-offset *surface-assembly-vocabulary* assembly nil))

(defun surface-assembly-at (offset)
  (domains:identity-vocabulary-member *surface-assembly-vocabulary* offset))

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
  (declare (ignore kind placement scene cell))
  (cond ((not (eq axis :z)) *soil-reading*)
        ((eq side :backward) *grass-reading*)
        (t *subsoil-reading*)))

(defmethod material-face-reading
    ((kind stone-material-kind) placement scene cell axis side)
  (declare (ignore kind placement axis side))
  (if (scene-foundation-cell-p scene cell)
      *foundation-stone-reading*
      *stone-reading*))

(defun face-reading-assembly (reading)
  (cond ((eq reading *grass-reading*) *grass-surface*)
        ((eq reading *soil-reading*) *soil-surface*)
        ((eq reading *subsoil-reading*) *subsoil-surface*)
        ((eq reading *stone-reading*) *stone-surface*)
        ((eq reading *foundation-stone-reading*) *foundation-stone-surface*)
        (t (error "No face surface assembly for reading ~S." reading))))

(defun stone-reading-p (reading)
  (typep (surface-reading-kind reading) 'stone-material-kind))

(defun chamfer-surface-assembly (readings)
  "Resolve incident face READINGS as one current-width surface assembly."
  (cond ((and (some #'stone-reading-p readings)
              (member *subsoil-reading* readings))
         *deep-set-stone-surface*)
        ((and (some #'stone-reading-p readings)
              (member *soil-reading* readings))
         *soil-set-stone-surface*)
        ((and (some #'stone-reading-p readings)
              (member *grass-reading* readings))
         *turf-set-stone-surface*)
        ((every (lambda (reading) (eq reading (first readings)))
                (rest readings))
         (face-reading-assembly (first readings)))
        ((some #'stone-reading-p readings) *stone-surface*)
        ((and (member *grass-reading* readings)
              (or (member *soil-reading* readings)
                  (member *subsoil-reading* readings)))
         *turf-edge-surface*)
        (t *soil-surface*)))

(defun closure-surface-assembly (assemblies)
  "Resolve an edge or fan closure without erasing incident assembly identity.

Fan construction may present assemblies already derived for surrounding bands;
those are not interchangeable with their primary face readings. This resolver
therefore preserves the legacy recursive decision exactly while expressing it
in semantic identities rather than numeric ranges."
  (flet ((stone-face-p (assembly)
           (member assembly (list *stone-surface*
                                  *foundation-stone-surface*))))
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
          (t *soil-surface*))))
