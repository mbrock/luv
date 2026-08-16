;;; The semantic block vocabulary and its procedural texture atlas.
;;;
;;; Blocks and faces are ordinary CLOS objects so the interesting semantic
;;; choices stay inspectable and redefinable at the REPL.  The material
;;; palette here is what the player places, what the mesher shades, and what
;;; the shader lab browses; the atlas is generated arithmetic, not an asset.

(in-package #:luv)

(defclass block-kind ()
  ((name :initarg :name :reader block-kind-name)
   (face-tiles :initarg :face-tiles :reader block-kind-face-tiles)
   ;; Light behavior is distinct from collision: opacity is how much a cell
   ;; costs propagating light, emission seeds the propagated blocklight
   ;; field, and surface emission is the material's own visible radiance.
   ;; One glowing crystal may configure both emissions, but they are
   ;; different facts.
   (light-opacity :initarg :light-opacity :initform 15
                  :type (integer 0 15)
                  :quantity (:quantity :block-light-attenuation-step
                             :unit :one)
                  :reader block-kind-light-opacity)
   (light-emission :initarg :light-emission :initform 0
                   :type (integer 0 15)
                   :quantity (:quantity :block-light-emission-step
                              :unit :one)
                   :reader block-kind-light-emission)
   (surface-emission :initarg :surface-emission :initform 0.0
                     :type real
                     :quantity (:quantity :material-emission :unit :one
                                :character :absolute)
                     :reader block-kind-surface-emission))
  (:metaclass luv.arithmetic.records:quantity-class))

(defgeneric block-solid-p (block))
(defgeneric block-face-tile (block face))

(defgeneric block-light-opacity (block)
  (:documentation "How strongly BLOCK attenuates propagated light, 0..15."))
(defgeneric block-light-emission (block)
  (:documentation "The blocklight level BLOCK seeds into its own cell, 0..15."))
(defgeneric block-surface-emission (block)
  (:documentation "BLOCK's own linear material radiance, independent of
propagated light."))

(defmethod block-solid-p ((block null)) nil)
(defmethod block-solid-p ((block block-kind)) t)

(defmethod block-light-opacity ((block null)) 0)
(defmethod block-light-emission ((block null)) 0)
(defmethod block-surface-emission ((block null)) 0.0)

(defmethod block-light-opacity ((block block-kind))
  (block-kind-light-opacity block))
(defmethod block-light-emission ((block block-kind))
  (block-kind-light-emission block))
(defmethod block-surface-emission ((block block-kind))
  (block-kind-surface-emission block))

(defclass block-face ()
  ((name :initarg :name :reader block-face-name)
   (neighbor :initarg :neighbor :reader block-face-neighbor)
   (corners :initarg :corners :reader block-face-corners)))

(defun make-block-face (name neighbor corners)
  (make-instance 'block-face :name name :neighbor neighbor
                              :corners corners))

(defun block-face-local-uv (face corner)
  "Project CORNER onto FACE's plane as tile-local UV coordinates.

The rule is read from the face's own outward normal rather than its name:
horizontal faces map world X,Z straight across the tile, and lateral faces
keep the world-horizontal axis as U while flipping world Y into V so tiles
hang upright."
  (let ((normal (block-face-neighbor face)))
    (cond ((not (zerop (voxel-direction-dy normal)))
           (values (first corner) (third corner)))
          ((not (zerop (voxel-direction-dz normal)))
           (values (first corner) (- 1 (second corner))))
          (t (values (third corner) (- 1 (second corner)))))))

(defparameter *block-faces*
  (list
   (make-block-face :left +voxel-negative-x+
                    '((0 0 0) (0 0 1) (0 1 1) (0 1 0)))
   (make-block-face :right +voxel-positive-x+
                    '((1 0 1) (1 0 0) (1 1 0) (1 1 1)))
   (make-block-face :bottom +voxel-negative-y+
                    '((0 0 1) (0 0 0) (1 0 0) (1 0 1)))
   (make-block-face :top +voxel-positive-y+
                    '((0 1 0) (0 1 1) (1 1 1) (1 1 0)))
   (make-block-face :back +voxel-negative-z+
                    '((1 0 0) (0 0 0) (0 1 0) (1 1 0)))
   (make-block-face :front +voxel-positive-z+
                    '((0 0 1) (1 0 1) (1 1 1) (0 1 1)))))

(defmethod block-face-tile ((block block-kind) (face block-face))
  (let ((tiles (block-kind-face-tiles block)))
    (or (getf tiles (block-face-name face))
        (and (member (block-face-name face) '(:left :right :back :front))
             (getf tiles :side))
        (getf tiles :all)
        (error "No texture tile for ~S face ~S."
               (block-kind-name block) (block-face-name face)))))

(defparameter *grass-block*
  (make-instance 'block-kind :name :grass
                             :face-tiles '(:top 0 :side 1 :bottom 2)))
(defparameter *dirt-block*
  (make-instance 'block-kind :name :dirt
                             :face-tiles '(:all 2)))
(defparameter *stone-block*
  (make-instance 'block-kind :name :stone
                             :face-tiles '(:all 3)))
(defparameter *wood-block*
  (make-instance 'block-kind :name :wood
                             :face-tiles '(:top 5 :bottom 5 :side 4)))
(defparameter *leaf-block*
  (make-instance 'block-kind :name :leaves
                             :face-tiles '(:all 6)))
(defparameter *sand-block*
  (make-instance 'block-kind :name :sand
                             :face-tiles '(:all 7)))
(defparameter *snow-block*
  (make-instance 'block-kind :name :snow
                             :face-tiles '(:top 8 :side 8 :bottom 2)))
(defparameter *crystal-block*
  (make-instance 'block-kind :name :crystal
                             :face-tiles '(:all 9)
                             :light-emission 12
                             :surface-emission 1.2))

(defparameter *placeable-block-kinds*
  (list *grass-block* *dirt-block* *stone-block* *wood-block*
        *leaf-block* *sand-block* *snow-block* *crystal-block*))

(defun placeable-block-kinds ()
  "Return the numbered material palette used by luvcraft and its tools."
  (copy-list *placeable-block-kinds*))

(defun block-kind-named (name &optional (error-p t))
  "Return the shared block kind whose durable semantic name is NAME.

When ERROR-P is true, signal an error rather than returning NIL for an
unknown name.  Save files and other external descriptions refer to block
kinds through this vocabulary instead of printing CLOS object identities."
  (or (find name *placeable-block-kinds* :key #'block-kind-name :test #'eq)
      (when error-p
        (error "No block kind is named ~S." name))))

(defconstant +block-atlas-tile-size+ 16)
(defconstant +block-atlas-tile-count+ 10)

(defun block-atlas-byte (value)
  (max 0 (min 255 (round value))))

(defun pack-block-atlas-rgba (red green blue)
  (logior (block-atlas-byte red)
          (ash (block-atlas-byte green) 8)
          (ash (block-atlas-byte blue) 16)
          #xff000000))

(defun block-atlas-variation (x y salt)
  (- (mod (+ (* x 17) (* y 31) (* salt 43) (* x y 7)) 25) 12))

(defun shaded-block-atlas-pixel (red green blue &optional (variation 0))
  (pack-block-atlas-rgba (+ red variation)
                         (+ green variation)
                         (+ blue variation)))

(defgeneric paint-block-atlas-tile (tile x y)
  (:documentation
   "Return the packed RGBA pixel of atlas tile TILE at tile-local X,Y.

Each numbered tile is one EQL method, so a live image can repaint a single
material and rebuild the atlas without touching the rest of the palette."))

(defmethod paint-block-atlas-tile (tile x y)
  (declare (ignore x y))
  (error "Unknown block atlas tile ~S." tile))

(defmethod paint-block-atlas-tile ((tile (eql 0)) x y)
  "Grass top."
  (shaded-block-atlas-pixel 91 171 68 (block-atlas-variation x y tile)))

(defmethod paint-block-atlas-tile ((tile (eql 1)) x y)
  "Grass side: a green fringe over dirt."
  (let ((variation (block-atlas-variation x y tile)))
    (if (< y 4)
        (shaded-block-atlas-pixel 86 158 61 variation)
        (shaded-block-atlas-pixel 123 82 48 (round variation 2)))))

(defmethod paint-block-atlas-tile ((tile (eql 2)) x y)
  "Dirt."
  (shaded-block-atlas-pixel 126 84 49 (block-atlas-variation x y tile)))

(defmethod paint-block-atlas-tile ((tile (eql 3)) x y)
  "Stone, with sparse bright flecks."
  (shaded-block-atlas-pixel
   126 132 136
   (+ (round (block-atlas-variation x y tile) 2)
      (if (zerop (mod (+ (* x 3) (* y 5)) 19)) 20 0))))

(defmethod paint-block-atlas-tile ((tile (eql 4)) x y)
  "Wood bark, with vertical grain stripes."
  (shaded-block-atlas-pixel
   116 76 39
   (+ (round (block-atlas-variation x y tile) 3)
      (if (zerop (mod x 5)) 18 0))))

(defmethod paint-block-atlas-tile ((tile (eql 5)) x y)
  "Wood end grain: concentric rings around the tile centre."
  (let* ((dx (- x 7.5))
         (dy (- y 7.5))
         (ring (mod (floor (+ (* dx dx) (* dy dy))) 18)))
    (shaded-block-atlas-pixel 133 91 49 (- ring 9))))

(defmethod paint-block-atlas-tile ((tile (eql 6)) x y)
  "Leaves: a strong checker so the canopy reads as foliage."
  (shaded-block-atlas-pixel
   51 132 58
   (+ (block-atlas-variation x y tile) (if (evenp (+ x y)) 8 -8))))

(defmethod paint-block-atlas-tile ((tile (eql 7)) x y)
  "Sand, with sparse darker grains."
  (shaded-block-atlas-pixel
   205 185 128
   (+ (round (block-atlas-variation x y tile) 2)
      (if (zerop (mod (+ x (* y 3)) 13)) 13 0))))

(defmethod paint-block-atlas-tile ((tile (eql 8)) x y)
  "Snow, with sparse glints."
  (shaded-block-atlas-pixel
   226 238 242
   (+ (round (block-atlas-variation x y tile) 3)
      (if (zerop (mod (+ (* x 5) (* y 7)) 23)) 14 0))))

(defmethod paint-block-atlas-tile ((tile (eql 9)) x y)
  "Blue crystal, with bright diagonal facets."
  (let* ((diagonal (abs (- x y)))
         (facet (if (or (<= diagonal 1)
                        (<= (abs (- (+ x y) 15)) 1))
                    42
                    0))
         (edge (if (or (zerop x) (zerop y)
                       (= x (1- +block-atlas-tile-size+))
                       (= y (1- +block-atlas-tile-size+)))
                   28
                   0))
         (variation (block-atlas-variation x y tile)))
    (pack-block-atlas-rgba
     (+ 72 (round variation 3) edge)
     (+ 176 (round variation 2) facet edge)
     (+ 235 variation facet edge))))

(defun make-block-texture-atlas ()
  "Return the little world's horizontal RGBA8 atlas as packed pixel words."
  (let* ((width (* +block-atlas-tile-size+ +block-atlas-tile-count+))
         (pixels (make-array (list +block-atlas-tile-size+ width)
                             :element-type '(unsigned-byte 32))))
    (dotimes (y +block-atlas-tile-size+)
      (dotimes (tile +block-atlas-tile-count+)
        (dotimes (x +block-atlas-tile-size+)
          (setf (aref pixels y (+ x (* tile +block-atlas-tile-size+)))
                (paint-block-atlas-tile tile x y)))))
    pixels))
