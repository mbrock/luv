;;; Isometric block icons.
;;;
;;; A material shown to the player as a flat swatch is a colour; shown as a
;;; cube it is the thing they are about to place.  These are the same
;;; procedural tiles the block atlas generates for the world, projected onto
;;; the three faces of a 2:1 dimetric cube and lit the way the world lights
;;; them -- top full, left in half shadow, right darker still.
;;;
;;; Each icon is rasterized once into an RGBA array and kept as a CLIM
;;; pattern, so drawing one is a single image command rather than seven
;;; hundred little parallelograms.  That is what makes it affordable to put
;;; the same icon in the hotbar, the inventory grid, the quickbar, and the
;;; item inspector.

(in-package #:mcluv)

(defconstant +block-icon-tile-size+ 16
  "The atlas's tile edge, in texels.  It is 16 for the same reason
Minecraft's is: it is enough to read a material and few enough to author by
hand.")

(defparameter *block-icon-face-shades*
  '(:top 1.0 :left 0.74 :right 0.55)
  "How much of the light each visible face gets.  The ratios matter more than
the numbers: equal faces read as a hexagon rather than as a cube.")

(defvar *block-icon-atlas* nil)

(defun block-icon-atlas ()
  "The shared block atlas, built on first use.

Generating it walks every tile of every material, which is far too much to do
per frame and trivial to do once.  CLEAR-BLOCK-ICON-CACHE drops it, which is
how a redefined PAINT-BLOCK-ATLAS-TILE method reaches the icons."
  (or *block-icon-atlas*
      (setf *block-icon-atlas* (luvcraft:make-block-texture-atlas))))

(defvar *block-icon-cache* (make-hash-table :test #'equal))

(defun clear-block-icon-cache ()
  "Forget every rasterized icon and the atlas behind them."
  (setf *block-icon-atlas* nil)
  (clrhash *block-icon-cache*)
  (values))

(defun block-icon-face-tile (block face)
  "The atlas tile BLOCK shows on FACE, falling back the way the world does."
  (let ((tiles (luvcraft:block-kind-face-tiles block)))
    (ecase face
      (:top (or (getf tiles :top) (getf tiles :all) (getf tiles :side)))
      ((:left :right)
       (or (getf tiles :side) (getf tiles :all) (getf tiles :top))))))

(defun shaded-icon-word (word shade)
  "Pack an atlas word into a CLIM pattern word at SHADE.

The atlas keeps red in the low byte; a pattern wants it in the third, which
is the same turn-around the video path makes."
  (flet ((channel (position)
           (min 255 (round (* shade (ldb (byte 8 position) word))))))
    (logior (ash 255 24)
            (ash (channel 0) 16)
            (ash (channel 8) 8)
            (channel 16))))

(defun block-icon-texel (atlas tile column row)
  (let ((offset (luvcraft:block-atlas-tile-offset tile)))
    (aref atlas
          (max 0 (min (1- +block-icon-tile-size+) row))
          (+ (max 0 (min (1- +block-icon-tile-size+) column))
             (* offset +block-icon-tile-size+)))))

(defun rasterize-block-icon (block size)
  "Draw BLOCK as a SIZE by SIZE isometric cube of RGBA words.

The cube is the standard two-to-one projection: a diamond SIZE wide and half
that tall for the top, and two parallelograms of the same half-height below
it.  Every pixel asks which of the three faces it belongs to and reads the
matching texel, so the icon carries the material's real texture rather than
its average colour."
  (let* ((atlas (block-icon-atlas))
         (pixels (make-array (list size size)
                             :element-type '(unsigned-byte 32)
                             :initial-element 0))
         (width (float size))
         (half (/ width 2.0))
         (quarter (/ width 4.0))
         (tiles (list :top (block-icon-face-tile block :top)
                      :left (block-icon-face-tile block :left)
                      :right (block-icon-face-tile block :right)))
         (edge (float +block-icon-tile-size+)))
    (flet ((paint (x y face u v)
             (let ((word (block-icon-texel
                          atlas (getf tiles face)
                          (floor (* u edge)) (floor (* v edge)))))
               (setf (aref pixels y x)
                     (shaded-icon-word
                      word (getf *block-icon-face-shades* face))))))
      (dotimes (y size pixels)
        (dotimes (x size)
          (let* ((px (+ x 0.5))
                 (py (+ y 0.5))
                 ;; Coordinates along the diamond's two edges from its left
                 ;; corner.  Both inside the unit square means the top face.
                 (dy (- py quarter))
                 (a (- (/ px width) (* 2.0 (/ dy width))))
                 (b (+ (/ px width) (* 2.0 (/ dy width)))))
            (cond
              ((and (<= 0.0 a) (< a 1.0) (<= 0.0 b) (< b 1.0))
               (paint x y :top a b))
              ((< px half)
               (let ((top-edge (+ quarter (/ px 2.0))))
                 (when (and (<= top-edge py) (< py (+ top-edge half)))
                   (paint x y :left (/ px half) (/ (- py top-edge) half)))))
              (t
               (let ((top-edge (- half (/ (- px half) 2.0))))
                 (when (and (<= top-edge py) (< py (+ top-edge half)))
                   (paint x y :right (/ (- px half) half)
                          (/ (- py top-edge) half))))))))))))

(defun block-icon-pattern (block &optional (size 32))
  "BLOCK's isometric icon at SIZE, rasterized on first ask and kept."
  (let ((key (cons (luvcraft:block-kind-name block) size)))
    (or (gethash key *block-icon-cache*)
        (setf (gethash key *block-icon-cache*)
              (make-pattern (rasterize-block-icon block size) nil)))))

(defun draw-block-icon (stream block x y &optional (size 32))
  "Draw BLOCK's cube with its top-left corner at X,Y."
  (draw-pattern* stream (block-icon-pattern block size) x y))
