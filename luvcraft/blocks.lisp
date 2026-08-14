;;; The semantic block vocabulary and its procedural texture atlas.
;;;
;;; Blocks and faces are ordinary CLOS objects so the interesting semantic
;;; choices stay inspectable and redefinable at the REPL.  The material
;;; palette here is what the player places, what the mesher shades, and what
;;; the shader lab browses; the atlas is generated arithmetic, not an asset.

(in-package #:luv)

(defclass block-kind ()
  ((name :initarg :name :reader block-kind-name)
   (face-tiles :initarg :face-tiles :reader block-kind-face-tiles)))

(defgeneric block-solid-p (block))
(defgeneric block-face-tile (block face))

(defmethod block-solid-p ((block null)) nil)
(defmethod block-solid-p ((block block-kind)) t)

(defclass block-face ()
  ((name :initarg :name :reader block-face-name)
   (neighbor :initarg :neighbor :reader block-face-neighbor)
   (corners :initarg :corners :reader block-face-corners)))

(defun make-block-face (name neighbor corners)
  (make-instance 'block-face :name name :neighbor neighbor
                              :corners corners))

(defparameter *block-faces*
  (list
   (make-block-face :left '(-1 0 0)
                    '((0 0 0) (0 0 1) (0 1 1) (0 1 0)))
   (make-block-face :right '(1 0 0)
                    '((1 0 1) (1 0 0) (1 1 0) (1 1 1)))
   (make-block-face :bottom '(0 -1 0)
                    '((0 0 1) (0 0 0) (1 0 0) (1 0 1)))
   (make-block-face :top '(0 1 0)
                    '((0 1 0) (0 1 1) (1 1 1) (1 1 0)))
   (make-block-face :back '(0 0 -1)
                    '((1 0 0) (0 0 0) (0 1 0) (1 1 0)))
   (make-block-face :front '(0 0 1)
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

(defparameter *placeable-block-kinds*
  (list *grass-block* *dirt-block* *stone-block* *wood-block*
        *leaf-block* *sand-block* *snow-block*))

(defun placeable-block-kinds ()
  "Return the numbered material palette used by luvcraft and its tools."
  (copy-list *placeable-block-kinds*))

(defconstant +block-atlas-tile-size+ 16)
(defconstant +block-atlas-tile-count+ 9)

(defun block-atlas-byte (value)
  (max 0 (min 255 (round value))))

(defun pack-block-atlas-rgba (red green blue)
  (logior (block-atlas-byte red)
          (ash (block-atlas-byte green) 8)
          (ash (block-atlas-byte blue) 16)
          #xff000000))

(defun block-atlas-variation (x y salt)
  (- (mod (+ (* x 17) (* y 31) (* salt 43) (* x y 7)) 25) 12))

(defun block-atlas-pixel (tile x y)
  (labels ((pixel (red green blue &optional (variation 0))
             (pack-block-atlas-rgba (+ red variation)
                                    (+ green variation)
                                    (+ blue variation))))
    (let ((variation (block-atlas-variation x y tile)))
      (case tile
        (0 (pixel 91 171 68 variation))
        (1 (if (< y 4)
               (pixel 86 158 61 variation)
               (pixel 123 82 48 (round variation 2))))
        (2 (pixel 126 84 49 variation))
        (3 (pixel 126 132 136
                  (+ (round variation 2)
                     (if (zerop (mod (+ (* x 3) (* y 5)) 19)) 20 0))))
        (4 (pixel 116 76 39
                  (+ (round variation 3)
                     (if (zerop (mod x 5)) 18 0))))
        (5 (let* ((dx (- x 7.5))
                  (dy (- y 7.5))
                  (ring (mod (floor (+ (* dx dx) (* dy dy))) 18)))
             (pixel 133 91 49 (- ring 9))))
        (6 (pixel 51 132 58
                  (+ variation (if (evenp (+ x y)) 8 -8))))
        (7 (pixel 205 185 128
                  (+ (round variation 2)
                     (if (zerop (mod (+ x (* y 3)) 13)) 13 0))))
        (8 (pixel 226 238 242
                  (+ (round variation 3)
                     (if (zerop (mod (+ (* x 5) (* y 7)) 23)) 14 0))))
        (otherwise (error "Unknown block atlas tile ~D." tile))))))

(defun make-block-texture-atlas ()
  "Return the little world's horizontal RGBA8 atlas as packed pixel words."
  (let* ((width (* +block-atlas-tile-size+ +block-atlas-tile-count+))
         (pixels (make-array (list +block-atlas-tile-size+ width)
                             :element-type '(unsigned-byte 32))))
    (dotimes (y +block-atlas-tile-size+)
      (dotimes (tile +block-atlas-tile-count+)
        (dotimes (x +block-atlas-tile-size+)
          (setf (aref pixels y (+ x (* tile +block-atlas-tile-size+)))
                (block-atlas-pixel tile x y)))))
    pixels))
