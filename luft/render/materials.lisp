(in-package #:luft.render)

;;; Cells retain only authored placement identity and the optical facts needed
;;; by voxel lighting.  Terrain appearance is owned by the fixed star shader;
;;; there is deliberately no per-face material vocabulary.

(defclass material-kind ()
  ((name :initarg :name :reader material-kind-name)
   (base-tone :initarg :base-tone :reader material-kind-base-tone)
   (light-opacity :initarg :light-opacity :initform 15
                  :reader material-kind-light-opacity)
   (light-emission :initarg :light-emission :initform '(0 0 0)
                   :reader material-kind-light-emission)
   (surface-emission :initarg :surface-emission :initform 0.0
                     :reader material-kind-surface-emission))
  (:documentation
   "An authored cell substance with the facts needed by voxel light."))

(defclass material-placement ()
  ((name :initarg :name :reader material-placement-name)
   (kind :initarg :kind :reader material-placement-kind))
  (:documentation
   "A named authored-editing choice referring to one cell material."))

(defun ensure-semantic-instance (current class &rest initargs)
  "Reinitialize CURRENT when it still has CLASS, preserving live identity."
  (if (and current (eq (class-of current) (find-class class)))
      (progn (apply #'reinitialize-instance current initargs) current)
      (apply #'make-instance class initargs)))

(defvar *earth-material* nil)
(defvar *limestone-material* nil)
(defvar *highland-rock-material* nil)
(defvar *crystal-material* nil)
(defvar *torch-flame-material* nil)
(defvar *terrain-material-placement* nil)
(defvar *highland-rock-material-placement* nil)
(defvar *sanctuary-material-placement* nil)
(defvar *beacon-material-placement* nil)
(defvar *crystal-material-placement* nil)

(setf *earth-material*
      (ensure-semantic-instance
       *earth-material* 'material-kind
       :name :earth :base-tone '(0.42 0.32 0.21))
      *limestone-material*
      (ensure-semantic-instance
       *limestone-material* 'material-kind
       :name :limestone :base-tone '(0.53 0.49 0.39))
      *highland-rock-material*
      (ensure-semantic-instance
       *highland-rock-material* 'material-kind
       :name :highland-rock :base-tone '(0.29 0.30 0.27))
      *crystal-material*
      (ensure-semantic-instance
       *crystal-material* 'material-kind
       :name :aether-crystal :base-tone '(0.16 0.68 0.94)
       :light-opacity 1 :light-emission '(3 11 15)
       :surface-emission 0.30)
      *torch-flame-material*
      (ensure-semantic-instance
       *torch-flame-material* 'material-kind
       :name :torch-flame :base-tone '(1.0 0.36 0.055)
       :light-emission '(15 9 3) :surface-emission 1.8)
      *terrain-material-placement*
      (ensure-semantic-instance
       *terrain-material-placement* 'material-placement
       :name :terrain :kind *earth-material*)
      *highland-rock-material-placement*
      (ensure-semantic-instance
       *highland-rock-material-placement* 'material-placement
       :name :highland-rock :kind *highland-rock-material*)
      *sanctuary-material-placement*
      (ensure-semantic-instance
       *sanctuary-material-placement* 'material-placement
       :name :sanctuary-limestone :kind *limestone-material*)
      *beacon-material-placement*
      (ensure-semantic-instance
       *beacon-material-placement* 'material-placement
       :name :ridge-beacon-limestone :kind *limestone-material*)
      *crystal-material-placement*
      (ensure-semantic-instance
       *crystal-material-placement* 'material-placement
       :name :aether-crystal :kind *crystal-material*))

(defun make-scene-material-vocabulary ()
  "Return the authored placement vocabulary shared by one scene's cells."
  (domains:make-identity-vocabulary-domain
   :members (list *terrain-material-placement*
                  *highland-rock-material-placement*
                  *sanctuary-material-placement*
                  *crystal-material-placement*)
   :limit #x10000))

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
