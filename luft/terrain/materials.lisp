(in-package #:luft.terrain)

;;; Cells retain only authored placement identity and the optical facts needed
;;; by voxel lighting.  The material classes and their compilation are terrain;
;;; which materials exist is a game's palette (luft/world/palette.lisp).
;;; Terrain appearance is owned by the fixed star shader; there is
;;; deliberately no per-face material vocabulary.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +material-reliefs+
    '(:none :grass :stone :grain :wood :leaves :planks :snow :gloss)
    "The relief fields the star shader knows, in the order of their codes."))

(defclass material-kind ()
  ((name :initarg :name :reader material-kind-name)
   (base-tone :initarg :base-tone :reader material-kind-base-tone)
   (top-tone :initarg :top-tone :initform nil :reader material-kind-top-tone)
   (side-tone :initarg :side-tone :initform nil :reader material-kind-side-tone)
   (bottom-tone :initarg :bottom-tone :initform nil
                :reader material-kind-bottom-tone)
   (light-opacity :initarg :light-opacity :initform 15
                  :reader material-kind-light-opacity)
   (light-emission :initarg :light-emission :initform '(0 0 0)
                   :reader material-kind-light-emission)
   (surface-emission :initarg :surface-emission :initform 0.0
                     :reader material-kind-surface-emission)
   (relief :initarg :relief :initform :none :type #.`(member ,@+material-reliefs+)
           :reader material-kind-relief
           :documentation "The world-space height field the surface is cut from.")
   (relief-strength :initarg :relief-strength :initform 0.5
                    :reader material-kind-relief-strength)
   (roughness :initarg :roughness :initform 0.85
              :reader material-kind-roughness)
   (edge-wear :initarg :edge-wear :initform 0.5
              :reader material-kind-edge-wear
              :documentation "How much convex edges lighten and hollows darken."))
  (:documentation
   "An authored cell substance: its tones, the relief and edge response the
star shader cuts it with (#HYBFRT), and the facts needed by voxel light."))

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

(defun material-kind-oriented-tones (kind)
  "Return KIND's top, side, and underside terrain tones."
  (let ((base (material-kind-base-tone kind)))
    (values (or (material-kind-top-tone kind) base)
            (or (material-kind-side-tone kind) base)
            (or (material-kind-bottom-tone kind) base))))

(defun pack-terrain-tone (tone)
  "Pack one scene-linear RGB tone into an inspectable RGB8 descriptor word."
  (destructuring-bind (red green blue) tone
    (flet ((lane (value)
             (round (* 255 (max 0.0 (min 1.0 value))))))
      (logior (lane red) (ash (lane green) 8) (ash (lane blue) 16)))))

(defun pack-terrain-surface (kind)
  "Pack KIND's relief, its strength, roughness, and edge wear.

Bits 0-3 are the relief code, 4-7 its strength, 8-15 roughness, and 16-23
edge wear; the star shader unpacks the same layout."
  (flet ((unit (value bits)
           (round (* (1- (ash 1 bits)) (max 0.0 (min 1.0 value))))))
    (logior (position (material-kind-relief kind) +material-reliefs+)
            (ash (unit (material-kind-relief-strength kind) 4) 4)
            (ash (unit (material-kind-roughness kind) 8) 8)
            (ash (unit (material-kind-edge-wear kind) 8) 16))))

(defun compile-terrain-material-descriptors (placement-vocabulary)
  "Compile AIR plus the scene's u8 placements to descriptor rows: top, side,
and bottom RGB8 tones, then the packed surface (PACK-TERRAIN-SURFACE)."
  (let* ((placements
           (domains:identity-vocabulary-members placement-vocabulary))
         (words
           (make-array (* 4 (1+ (length placements)))
                       :element-type '(unsigned-byte 32)
                       :initial-element 0)))
    (loop for placement across placements
          for code from 1
          for kind = (material-placement-kind placement)
          do (multiple-value-bind (top side bottom)
                 (material-kind-oriented-tones kind)
               (let ((row (* code 4)))
                 (setf (aref words row) (pack-terrain-tone top)
                       (aref words (+ row 1)) (pack-terrain-tone side)
                       (aref words (+ row 2)) (pack-terrain-tone bottom)
                       (aref words (+ row 3)) (pack-terrain-surface kind)))))
    words))

;;; Published material fields share immutable per-chunk tables. Construction
;;; may fill a local table; an edit copies only that table and the chunk index.
(defstruct (material-store (:constructor make-material-store ()))
  (chunks (make-hash-table :test #'eql) :type hash-table :read-only t))

(defun material-cell-at (field cell)
  "Return the placement offset and presence of CELL in FIELD."
  (etypecase field
    (hash-table (gethash cell field))
    (function (funcall field cell))
    (material-store
     (let ((chunk (gethash (luft:site-chunk-key cell)
                           (material-store-chunks field))))
       (if chunk (gethash cell chunk) (values nil nil))))))

(defun map-material-cells (function field)
  (etypecase field
    (hash-table (maphash function field))
    (material-store
     (maphash (lambda (key chunk)
                (declare (ignore key))
                (maphash function chunk))
              (material-store-chunks field)))))

(defun material-store-from-table (table)
  (let ((store (make-material-store)))
    (maphash
     (lambda (cell offset)
       (let* ((key (luft:site-chunk-key cell))
              (chunk (or (gethash key (material-store-chunks store))
                         (setf (gethash key (material-store-chunks store))
                               (make-hash-table :test #'eql)))))
         (setf (gethash cell chunk) offset)))
     table)
    store))

(defun material-store-with-cell (store cell offset)
  "A new field with CELL changed; NIL removes it. Offset zero is occupied."
  (let* ((copy (make-material-store))
         (key (luft:site-chunk-key cell))
         (old (gethash key (material-store-chunks store)))
         (chunk (if old (alexandria:copy-hash-table old)
                    (make-hash-table :test #'eql))))
    (maphash (lambda (key value)
               (setf (gethash key (material-store-chunks copy)) value))
             (material-store-chunks store))
    (if offset (setf (gethash cell chunk) offset) (remhash cell chunk))
    (setf (gethash key (material-store-chunks copy)) chunk)
    copy))

(defun material-cell-reader (field)
  "A read-only lookup that retains this exact material snapshot."
  (lambda (cell) (material-cell-at field cell)))

(defun compile-surface-mesh-appearance (mesh material-cells descriptors)
  "Compile one eight-u8 material record per active star in MESH.

Code zero is authored air; occupied samples carry one plus their stable scene
vocabulary offset.  The active (X Y Z STAR) words are only read and therefore
remain byte-identical under repainting."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((domain (luft:surface-mesh-domain mesh))
         (sites (luft:surface-mesh-star-site-words mesh))
         (codes (make-array (* 2 (length sites))
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (loop for site-offset fixnum from 0 below (length sites) by 4
          for appearance-offset fixnum from 0 by 8
          for x = (aref sites site-offset)
          for y = (aref sites (+ site-offset 1))
          for z = (aref sites (+ site-offset 2))
          for star = (aref sites (+ site-offset 3))
          do (dotimes (sample 8)
               (when (logbitp sample star)
                 (let* ((cell-x (- x (if (logbitp 0 sample) 0 1)))
                        (cell-y (- y (if (logbitp 1 sample) 0 1)))
                        (cell-z (- z (if (logbitp 2 sample) 0 1)))
                        (cell (luft:make-site domain cell-x cell-y cell-z
                                              luft:+cell-extent+ 1)))
                   (multiple-value-bind (offset present-p)
                       (material-cell-at material-cells cell)
                     (unless present-p
                       (error "Occupied star sample ~D at (~D ~D ~D) has no material."
                              sample x y z))
                     (unless (typep offset '(integer 0 254))
                       (error "Material offset ~S does not fit the u8 terrain palette."
                              offset))
                     (setf (aref codes (+ appearance-offset sample))
                           (1+ offset)))))))
    (setf (luft:surface-mesh-appearance-codes mesh) codes
          (luft:surface-mesh-appearance-descriptor-words mesh) descriptors)
    mesh))

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
    (map-material-cells
     (lambda (cell placement-offset)
       (let* ((placement (aref placements placement-offset))
              (emission
                (material-kind-packed-light-emission
                 (material-placement-kind placement))))
         (unless (zerop emission)
           (push (luft:make-voxel-light-source cell emission) sources))))
     material-cells)
    sources))
