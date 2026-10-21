;;; Architectural materials: dressed stone, timber, roofs, and furnishings.
;;;
;;; The block vocabulary in BLOCKS.LISP names these; this file paints them.
;;; As with every other material, each tile is one EQL method for its colour
;;; and one for its relief, so a single surface can be re-cut in a live image.
;;; Where several tiles share a construction -- a raised panel, a riveted
;;; plate, a run of planks -- the construction is a function and the tiles
;;; are its arguments.

(in-package #:luvcraft)

(defconstant +block-atlas-last-texel+ (1- +block-atlas-tile-size+))

(defun block-atlas-edge-distance (x y)
  "How many texels X,Y lies inside the nearest edge of its tile."
  (min x y (- +block-atlas-last-texel+ x) (- +block-atlas-last-texel+ y)))

(defun block-atlas-bevel-light (x y)
  "+1 on a tile's upper-left rim, -1 on its lower-right rim, else 0.

A raised panel catches the light on the edges facing the sky and drops into
shadow on the others; this is that one fact, read by colour and relief."
  (cond ((or (zerop x) (zerop y)) 1)
        ((or (= x +block-atlas-last-texel+) (= y +block-atlas-last-texel+)) -1)
        (t 0)))

(defun block-atlas-grain (tile x y &optional (divisor 3))
  (round (block-atlas-variation x y tile) divisor))

;;; ---------------------------------------------------------------------
;;; Dressed stone.

(defmethod paint-block-atlas-tile ((tile (eql :polished-stone)) x y)
  "Polished stone: one honed slab with a crisp lit-and-shadowed arris."
  (shaded-block-atlas-pixel
   150 154 156
   (+ (* 22 (block-atlas-bevel-light x y))
      (block-atlas-grain tile x y 5))))

(defmethod paint-block-atlas-relief ((tile (eql :polished-stone)) x y)
  "Polished stone: flat but for the slight drop of its edge."
  (block-atlas-byte
   (+ 140 (if (zerop (block-atlas-edge-distance x y)) -30 0)
      (* 0.04 (- (block-atlas-lattice-hash x y 401) 128)))))

(defun chiseled-stone-depth (x y)
  "The carving on a chiseled block: 0 field, 1 groove, 2 boss."
  (let* ((edge (block-atlas-edge-distance x y))
         (dx (abs (- x 7.5)))
         (dy (abs (- y 7.5)))
         (diamond (+ dx dy)))
    (cond ((= edge 2) 1)
          ((<= diamond 2.5) 2)
          ((< (abs (- diamond 5)) 0.75) 1)
          (t 0))))

(defmethod paint-block-atlas-tile ((tile (eql :chiseled-stone)) x y)
  "Chiseled stone: a sunk border and a diamond cut round a raised boss."
  (shaded-block-atlas-pixel
   140 142 136
   (+ (ecase (chiseled-stone-depth x y) (0 0) (1 -38) (2 18))
      (* 14 (block-atlas-bevel-light x y))
      (block-atlas-grain tile x y 4))))

(defmethod paint-block-atlas-relief ((tile (eql :chiseled-stone)) x y)
  (block-atlas-byte
   (+ (ecase (chiseled-stone-depth x y) (0 150) (1 60) (2 200))
      (* 0.10 (- (block-atlas-lattice-hash x y 411) 128)))))

(defun marble-vein (x y)
  "0..1 strength of the marble vein through X,Y: a warped diagonal wave."
  (let* ((phase (+ (* 0.55 x) (* 0.85 y) (* 2.2 (sin (* 0.45 y)))
                   (* 1.3 (sin (* 0.6 x)))))
         (wave (abs (sin (* 0.5 phase)))))
    (max 0.0 (- 1.0 (* 2.2 wave)))))

(defmethod paint-block-atlas-tile ((tile (eql :marble)) x y)
  "Marble: warm white stone threaded with soft grey veins."
  (let ((vein (marble-vein x y)))
    (shaded-block-atlas-pixel
     230 228 222
     (- (block-atlas-grain tile x y 6) (round (* 85 vein))))))

(defmethod paint-block-atlas-relief ((tile (eql :marble)) x y)
  "Marble: honed flat; the veins are colour, not texture."
  (block-atlas-byte
   (+ 140 (* -12 (marble-vein x y))
      (* 0.03 (- (block-atlas-lattice-hash x y 421) 128)))))

(defun marble-tiles-grout-p (x y)
  (or (zerop (mod x 8)) (zerop (mod y 8))))

(defmethod paint-block-atlas-tile ((tile (eql :marble-tiles)) x y)
  "Marble tiles: four square slabs, two pale and two dove grey, grouted."
  (cond ((marble-tiles-grout-p x y)
         (shaded-block-atlas-pixel 120 118 112))
        (t
         (let ((pale-p (evenp (+ (floor x 8) (floor y 8))))
               (vein (round (* 50 (marble-vein (+ x 3) (* 2 y))))))
           (if pale-p
               (shaded-block-atlas-pixel 228 226 220 (- (block-atlas-grain tile x y 6) vein))
               (shaded-block-atlas-pixel 176 178 180 (- (block-atlas-grain tile x y 6) vein)))))))

(defmethod paint-block-atlas-relief ((tile (eql :marble-tiles)) x y)
  (if (marble-tiles-grout-p x y) 70 150))

(defmethod paint-block-atlas-tile ((tile (eql :granite)) x y)
  "Granite: rose feldspar, grey quartz, and black mica, coarsely mixed."
  (let ((grain (block-atlas-lattice-hash x y 431))
        (clump (block-atlas-clump x y 432 3)))
    (cond ((> grain 225) (pack-block-atlas-rgba 38 34 36))
          ((> clump 150)
           (shaded-block-atlas-pixel 196 132 116 (round (- grain 128) 10)))
          (t (shaded-block-atlas-pixel 166 158 154 (round (- grain 128) 9))))))

(defmethod paint-block-atlas-relief ((tile (eql :granite)) x y)
  (block-atlas-byte
   (+ 140 (* 0.14 (- (block-atlas-lattice-hash x y 431) 128)))))

(defun basalt-column-joint-p (x)
  (member (mod x 16) '(0 5 11)))

(defmethod paint-block-atlas-tile ((tile (eql :basalt-side)) x y)
  "Basalt side: dark columns standing shoulder to shoulder."
  (if (basalt-column-joint-p x)
      (shaded-block-atlas-pixel 22 22 25)
      (shaded-block-atlas-pixel
       58 58 63
       (+ (block-atlas-grain tile x y 3)
          (if (zerop (mod (+ y (* 3 x)) 9)) -8 0)))))

(defmethod paint-block-atlas-relief ((tile (eql :basalt-side)) x y)
  (if (basalt-column-joint-p x)
      40
      (block-atlas-byte
       (+ 150 (* 0.22 (- (block-atlas-lattice-hash x y 441) 128))))))

(defun basalt-top-distance (x y)
  "Distance from X,Y to its column's centre in a staggered, near-hex plate."
  (block-atlas-plate-distance x y 6 5))

(defmethod paint-block-atlas-tile ((tile (eql :basalt-top)) x y)
  "Basalt top: the polygonal ends of the columns, each slightly domed."
  (let ((distance (basalt-top-distance x y)))
    (if (> distance 0.82)
        (shaded-block-atlas-pixel 24 24 27)
        (shaded-block-atlas-pixel
         66 66 72 (+ (round (* 20 (- 0.6 distance)))
                     (block-atlas-grain tile x y 4))))))

(defmethod paint-block-atlas-relief ((tile (eql :basalt-top)) x y)
  (let ((distance (basalt-top-distance x y)))
    (block-atlas-byte (if (> distance 0.82) 50 (- 200 (* 70 distance))))))

(defun limestone-mortar-p (x y)
  "Two courses of long ashlar per tile, the upper one offset by half."
  (or (zerop (mod y 8))
      (zerop (mod (+ x (if (oddp (floor y 8)) 8 0)) 16))))

(defmethod paint-block-atlas-tile ((tile (eql :limestone)) x y)
  "Limestone ashlar: long pale blocks with fine joints and fossil flecks."
  (if (limestone-mortar-p x y)
      (shaded-block-atlas-pixel 168 158 132)
      (shaded-block-atlas-pixel
       218 206 172
       (+ (block-atlas-grain tile x y 4)
          (if (> (block-atlas-lattice-hash x y 451) 238) -24 0)))))

(defmethod paint-block-atlas-relief ((tile (eql :limestone)) x y)
  (if (limestone-mortar-p x y)
      80
      (block-atlas-byte
       (+ 150 (* 0.14 (- (block-atlas-lattice-hash x y 452) 128))))))

(defun mossy-stone-moss-p (x y)
  "Whether moss has taken X,Y: clumps that favour the joints and the base."
  (> (+ (block-atlas-clump x y 461 4)
        (if (block-atlas-brick-mortar-p x y 8 5) 50 0)
        (* 4 y))
     230))

(defmethod paint-block-atlas-tile ((tile (eql :mossy-stone-bricks)) x y)
  "Mossy stone bricks: the old wall, moss creeping up out of its joints."
  (cond ((mossy-stone-moss-p x y)
         (let ((clump (- (block-atlas-clump x y 462 2) 128)))
           (pack-block-atlas-rgba (+ 74 (round clump 8))
                                  (+ 118 (round clump 5))
                                  (+ 44 (round clump 10)))))
        ((block-atlas-brick-mortar-p x y 8 5)
         (shaded-block-atlas-pixel 70 74 66))
        (t (shaded-block-atlas-pixel
            134 138 128 (block-atlas-grain tile x y 3)))))

(defmethod paint-block-atlas-relief ((tile (eql :mossy-stone-bricks)) x y)
  (cond ((mossy-stone-moss-p x y)
         (block-atlas-byte (+ 150 (* 0.4 (- (block-atlas-clump x y 462 2) 128)))))
        ((block-atlas-brick-mortar-p x y 8 5) 54)
        (t (block-atlas-byte
            (+ 155 (* 0.18 (- (block-atlas-lattice-hash x y 463) 128)))))))

(defun cracked-stone-crack-p (x y)
  "Whether X,Y is on one of the two cracks wandering down the masonry."
  (or (< (abs (- x (+ 5 (* 2.2 (sin (* 0.8 y)))))) 0.55)
      (and (> y 6)
           (< (abs (- x (+ 12 (* 1.6 (sin (+ 1.0 (* 1.1 y))))))) 0.5))))

(defmethod paint-block-atlas-tile ((tile (eql :cracked-stone-bricks)) x y)
  "Cracked stone bricks: the stone-brick bond, split by settling."
  (cond ((block-atlas-brick-mortar-p x y 8 5)
         (shaded-block-atlas-pixel 72 74 71))
        ((cracked-stone-crack-p x y)
         (shaded-block-atlas-pixel 52 53 51))
        (t (shaded-block-atlas-pixel
            128 130 124 (block-atlas-grain tile x y 2)))))

(defmethod paint-block-atlas-relief ((tile (eql :cracked-stone-bricks)) x y)
  (cond ((block-atlas-brick-mortar-p x y 8 5) 54)
        ((cracked-stone-crack-p x y) 30)
        (t (block-atlas-byte
            (+ 152 (* 0.22 (- (block-atlas-lattice-hash x y 471) 128)))))))

(defmethod paint-block-atlas-tile ((tile (eql :terracotta)) x y)
  "Terracotta: smooth fired clay, the colour of a flowerpot."
  (shaded-block-atlas-pixel
   176 102 68 (+ (block-atlas-grain tile x y 4)
                 (round (- (block-atlas-clump x y 481 6) 128) 14))))

(defmethod paint-block-atlas-relief ((tile (eql :terracotta)) x y)
  (block-atlas-byte
   (+ 140 (* 0.08 (- (block-atlas-lattice-hash x y 482) 128)))))

(defmethod paint-block-atlas-tile ((tile (eql :plaster)) x y)
  "Plaster: a lime-washed wall, trowelled unevenly by hand."
  (shaded-block-atlas-pixel
   230 222 204 (+ (round (- (block-atlas-clump x y 491 5) 128) 12)
                  (block-atlas-grain tile x y 5))))

(defmethod paint-block-atlas-relief ((tile (eql :plaster)) x y)
  "Plaster: soft trowel swells with a little sand in the finish."
  (block-atlas-byte
   (+ (* 0.55 (block-atlas-clump x y 491 5))
      (* 0.12 (block-atlas-lattice-hash x y 492))
      50)))

(defun concrete-tie-hole-p (x y)
  (and (member x '(3 12)) (member y '(4 11))))

(defmethod paint-block-atlas-tile ((tile (eql :concrete)) x y)
  "Concrete: a poured face with board seams and the tie holes of its form."
  (cond ((concrete-tie-hole-p x y) (shaded-block-atlas-pixel 70 70 68))
        ((zerop (mod y 8)) (shaded-block-atlas-pixel 124 124 120))
        (t (shaded-block-atlas-pixel
            148 148 144 (block-atlas-grain tile x y 4)))))

(defmethod paint-block-atlas-relief ((tile (eql :concrete)) x y)
  (cond ((concrete-tie-hole-p x y) 40)
        ((zerop (mod y 8)) 110)
        (t (block-atlas-byte
            (+ 140 (* 0.10 (- (block-atlas-lattice-hash x y 501) 128)))))))

(defmethod paint-block-atlas-tile ((tile (eql :checker-floor)) x y)
  "Checker floor: four glazed squares, black and white, across each block."
  (let ((grain (block-atlas-grain tile x y 6)))
    (if (evenp (+ (floor x 8) (floor y 8)))
        (shaded-block-atlas-pixel 226 224 218 grain)
        (shaded-block-atlas-pixel 34 34 38 grain))))

(defmethod paint-block-atlas-relief ((tile (eql :checker-floor)) x y)
  (if (or (zerop (mod x 8)) (zerop (mod y 8))) 110 145))

(defparameter *mosaic-tesserae*
  '((178 64 52) (226 172 72) (66 112 160) (232 226 210) (70 128 96)
    (128 76 132))
  "The glass and stone a mosaic is set from, as sRGB bytes.")

(defun mosaic-grout-p (x y)
  (or (= 2 (mod x 3)) (= 2 (mod y 3))))

(defmethod paint-block-atlas-tile ((tile (eql :mosaic)) x y)
  "Mosaic: little square tesserae of coloured glass set in pale grout."
  (if (mosaic-grout-p x y)
      (shaded-block-atlas-pixel 200 194 180)
      (destructuring-bind (red green blue)
          (nth (mod (block-atlas-lattice-hash (floor x 3) (floor y 3) 511)
                    (length *mosaic-tesserae*))
               *mosaic-tesserae*)
        (shaded-block-atlas-pixel red green blue
                                  (block-atlas-grain tile x y 4)))))

(defmethod paint-block-atlas-relief ((tile (eql :mosaic)) x y)
  (if (mosaic-grout-p x y)
      80
      (block-atlas-byte
       (+ 150 (* 0.12 (- (block-atlas-lattice-hash (floor x 3) (floor y 3) 512)
                         128))))))

(defun blue-tile-ink (x y)
  "What a quarter-tile azulejo shows at X,Y: :GROUT, :BLUE, :PALE, or :GLAZE.

Each 8x8 tile carries a diamond outline round a solid centre and a quarter
flower in each corner, so four tiles together make one larger rosette."
  (let* ((u (mod x 8)) (v (mod y 8))
         (du (abs (- u 3.5))) (dv (abs (- v 3.5)))
         (diamond (+ du dv))
         (corner (min (+ u v) (+ (- 7 u) v) (+ u (- 7 v))
                      (+ (- 7 u) (- 7 v)))))
    (cond ((or (zerop u) (zerop v)) :grout)
          ((<= diamond 1.0) :blue)
          ((< (abs (- diamond 3.0)) 0.6) :blue)
          ((<= corner 2) :pale)
          (t :glaze))))

(defmethod paint-block-atlas-tile ((tile (eql :blue-tiles)) x y)
  "Blue tiles: glazed white azulejos painted in cobalt."
  (ecase (blue-tile-ink x y)
    (:grout (shaded-block-atlas-pixel 170 168 160))
    (:blue (shaded-block-atlas-pixel 38 74 160 (block-atlas-grain tile x y 6)))
    (:pale (shaded-block-atlas-pixel 112 150 210))
    (:glaze (shaded-block-atlas-pixel 234 236 236 (block-atlas-grain tile x y 8)))))

(defmethod paint-block-atlas-relief ((tile (eql :blue-tiles)) x y)
  (if (eq :grout (blue-tile-ink x y)) 80 150))

(defmethod paint-block-atlas-tile ((tile (eql :obsidian)) x y)
  "Obsidian: black volcanic glass with violet sheen in its flow lines."
  (let ((sheen (max 0 (round (* 40 (- (sin (+ (* 0.7 x) (* 0.35 y)
                                               (* 1.5 (sin (* 0.5 y)))))
                                      0.6))))))
    (pack-block-atlas-rgba (+ 22 (round sheen 2))
                           (+ 16 (round sheen 3))
                           (+ 32 sheen))))

(defmethod paint-block-atlas-relief ((tile (eql :obsidian)) x y)
  "Obsidian: conchoidal ripples in otherwise glassy flatness."
  (block-atlas-byte
   (+ 140 (* 18 (sin (* 0.9 (+ x (* 0.5 y) (* 1.5 (sin (* 0.5 y))))))))))

;;; ---------------------------------------------------------------------
;;; Roofs.  A course of shingles is lit at its top and in shadow at its
;;; lower lip, where it overhangs the course below; that gradient is what
;;; makes a roof read as laid rather than painted.

(defun shingle-course (x y width height)
  "Return X,Y's row-fraction 0..1 down its shingle and whether it is a gap."
  (let* ((course (floor y height))
         (offset (if (oddp course) (floor width 2) 0))
         (u (mod (+ x offset) width))
         (v (mod y height)))
    (values (/ v (float (1- height))) (zerop u))))

(defun roof-tile-scallop-p (x y)
  "Whether X,Y lies below the round lower lip of its roof tile."
  (let* ((course (floor y 4))
         (offset (if (oddp course) 2 0))
         (u (- (mod (+ x offset) 4) 1.5))
         (v (mod y 4)))
    (and (= v 3) (> (abs u) 1.2))))

(defmethod paint-block-atlas-tile ((tile (eql :roof-tiles)) x y)
  "Roof tiles: staggered courses of rounded clay tiles."
  (multiple-value-bind (down gap-p) (shingle-course x y 4 4)
    (if (or gap-p (roof-tile-scallop-p x y))
        (shaded-block-atlas-pixel 92 38 26)
        (shaded-block-atlas-pixel
         182 78 50 (+ (round (* -40 down)) 10
                      (block-atlas-grain tile x y 4))))))

(defmethod paint-block-atlas-relief ((tile (eql :roof-tiles)) x y)
  (multiple-value-bind (down gap-p) (shingle-course x y 4 4)
    (if (or gap-p (roof-tile-scallop-p x y))
        50
        (block-atlas-byte (+ 110 (* 100 down))))))

(defmethod paint-block-atlas-tile ((tile (eql :slate-shingles)) x y)
  "Slate shingles: thin dark rectangles laid in a half bond."
  (multiple-value-bind (down gap-p) (shingle-course x y 6 4)
    (if gap-p
        (shaded-block-atlas-pixel 24 28 33)
        (shaded-block-atlas-pixel
         70 80 92 (+ (round (* -30 down)) 6
                     (block-atlas-grain tile x y 4)
                     (round (- (block-atlas-lattice-hash
                                (floor x 6) (floor y 4) 521)
                               128)
                            16))))))

(defmethod paint-block-atlas-relief ((tile (eql :slate-shingles)) x y)
  (multiple-value-bind (down gap-p) (shingle-course x y 6 4)
    (if gap-p 50 (block-atlas-byte (+ 120 (* 80 down))))))

(defmethod paint-block-atlas-tile ((tile (eql :thatch)) x y)
  "Thatch: layered bundles of straw, each layer's butt ends in shadow."
  (let ((straw (round (* 16 (sin (+ (* 2.1 x) (* 0.4 y)
                                     (block-atlas-lattice-hash x 0 531))))))
        (layer (mod y 5)))
    (shaded-block-atlas-pixel
     196 160 84 (+ straw (if (= layer 4) -46 (* -4 layer))
                   (block-atlas-grain tile x y 5)))))

(defmethod paint-block-atlas-relief ((tile (eql :thatch)) x y)
  (block-atlas-byte
   (+ 120 (* 22 (sin (+ (* 2.1 x) (* 0.4 y))))
      (if (= (mod y 5) 4) -60 (* 8 (mod y 5))))))

;;; ---------------------------------------------------------------------
;;; Woodwork.

(defun plank-pixel (tile x y red green blue)
  "Sawn boards the width of PLANKS: horizontal seams and wandering grain."
  (let ((grain (round (* 9 (sin (/ (+ (* x 2) y
                                      (* 3 (floor y 5)))
                                   2.4))))))
    (if (zerop (mod y 5))
        (shaded-block-atlas-pixel (round red 1.9) (round green 1.9)
                                  (round blue 1.9))
        (shaded-block-atlas-pixel red green blue
                                  (+ grain (block-atlas-grain tile x y 5))))))

(defun plank-relief (x y)
  (if (zerop (mod y 5))
      45
      (block-atlas-byte (+ 145 (* 18 (sin (/ (+ (* x 2) y) 2.4)))))))

(defmethod paint-block-atlas-tile ((tile (eql :dark-planks)) x y)
  "Dark planks: oiled oak boards."
  (plank-pixel tile x y 96 62 36))

(defmethod paint-block-atlas-relief ((tile (eql :dark-planks)) x y)
  (plank-relief x y))

(defmethod paint-block-atlas-tile ((tile (eql :birch-planks)) x y)
  "Birch planks: pale, close-grained boards."
  (plank-pixel tile x y 214 192 140))

(defmethod paint-block-atlas-relief ((tile (eql :birch-planks)) x y)
  (plank-relief x y))

(defun parquet-slat (x y)
  "Return X,Y's slat coordinates in a basketweave: ACROSS, ALONG, and the
direction the slats in its 8x8 square run."
  (if (evenp (+ (floor x 8) (floor y 8)))
      (values (mod y 8) (mod x 8) :horizontal)
      (values (mod x 8) (mod y 8) :vertical)))

(defmethod paint-block-atlas-tile ((tile (eql :parquet)) x y)
  "Parquet: short oak slats laid in a basketweave of alternating squares."
  (multiple-value-bind (across along direction) (parquet-slat x y)
    (let ((seam (or (zerop (mod across 3)) (zerop (mod x 8))
                    (zerop (mod y 8))))
          (tone (if (eq direction :horizontal) 0 -14))
          (grain (round (* 7 (sin (* 1.3 along))))))
      (if seam
          (shaded-block-atlas-pixel 92 58 30)
          (shaded-block-atlas-pixel 172 118 62
                                    (+ tone grain
                                       (block-atlas-grain tile x y 5)))))))

(defmethod paint-block-atlas-relief ((tile (eql :parquet)) x y)
  (multiple-value-bind (across along) (parquet-slat x y)
    (if (or (zerop (mod across 3)) (zerop (mod x 8)) (zerop (mod y 8)))
        70
        (block-atlas-byte (+ 145 (* 10 (sin (* 1.3 along))))))))

(defmethod paint-block-atlas-tile ((tile (eql :timber-side)) x y)
  "Timber side: an adzed face, the grain running straight along the beam."
  (shaded-block-atlas-pixel
   122 82 46 (+ (round (* 10 (sin (+ (* 1.7 x) (* 0.15 y)))))
                (if (zerop (mod (+ (* 7 x) (floor y 6)) 11)) -14 0)
                (* 10 (block-atlas-bevel-light x y))
                (block-atlas-grain tile x y 5))))

(defmethod paint-block-atlas-relief ((tile (eql :timber-side)) x y)
  (block-atlas-byte
   (+ 140 (* 16 (sin (+ (* 1.7 x) (* 0.15 y))))
      (if (zerop (block-atlas-edge-distance x y)) -40 0))))

(defmethod paint-block-atlas-tile ((tile (eql :timber-end)) x y)
  "Timber end: growth rings boxed by the hewn beam's chamfer."
  (let* ((dx (- x 7.5)) (dy (- y 7.5))
         (ring (mod (floor (* 1.4 (sqrt (+ (* dx dx) (* dy dy))))) 2)))
    (if (zerop (block-atlas-edge-distance x y))
        (shaded-block-atlas-pixel 92 60 32)
        (shaded-block-atlas-pixel 168 124 76
                                  (+ (* ring -16)
                                     (block-atlas-grain tile x y 5))))))

(defmethod paint-block-atlas-relief ((tile (eql :timber-end)) x y)
  (let* ((dx (- x 7.5)) (dy (- y 7.5))
         (ring (mod (floor (* 1.4 (sqrt (+ (* dx dx) (* dy dy))))) 2)))
    (if (zerop (block-atlas-edge-distance x y)) 70 (+ 130 (* 18 ring)))))

(defun wainscot-part (x y)
  "Which part of a frame-and-panel X,Y is: :FRAME, :PANEL, or a :BEVEL-LIT /
:BEVEL-DARK slope between them."
  (let ((edge (block-atlas-edge-distance x y)))
    (cond ((< edge 2) :frame)
          ((= edge 2)
           (if (or (= x 2) (= y 2)) :bevel-dark :bevel-lit))
          (t :panel))))

(defmethod paint-block-atlas-tile ((tile (eql :wainscot)) x y)
  "Wainscot: a raised field panel inside a stile-and-rail frame."
  (let ((grain (+ (round (* 6 (sin (* 0.9 y)))) (block-atlas-grain tile x y 5))))
    (ecase (wainscot-part x y)
      (:frame (shaded-block-atlas-pixel 110 64 34 grain))
      (:bevel-dark (shaded-block-atlas-pixel 78 44 22))
      (:bevel-lit (shaded-block-atlas-pixel 156 100 58))
      (:panel (shaded-block-atlas-pixel 132 82 44 grain)))))

(defmethod paint-block-atlas-relief ((tile (eql :wainscot)) x y)
  (ecase (wainscot-part x y)
    (:frame 150)
    (:bevel-dark 90)
    (:bevel-lit 110)
    (:panel 175)))

(defun half-timber-beam-p (x y)
  "Whether X,Y is on the frame or the X brace of a half-timbered panel."
  (or (< (block-atlas-edge-distance x y) 2)
      (<= (abs (- x y)) 1)
      (<= (abs (- (+ x y) +block-atlas-last-texel+)) 1)))

(defmethod paint-block-atlas-tile ((tile (eql :half-timber)) x y)
  "Half-timber: whitewashed infill between dark oak posts and a cross brace."
  (if (half-timber-beam-p x y)
      (shaded-block-atlas-pixel 70 44 26 (block-atlas-grain tile x y 4))
      (shaded-block-atlas-pixel 226 216 194 (block-atlas-grain tile x y 5))))

(defmethod paint-block-atlas-relief ((tile (eql :half-timber)) x y)
  (if (half-timber-beam-p x y)
      (block-atlas-byte (+ 170 (* 0.10 (- (block-atlas-lattice-hash x y 541) 128))))
      (block-atlas-byte (+ 120 (* 0.30 (- (block-atlas-clump x y 542 4) 128))))))

(defparameter *book-colours*
  '((122 34 30) (36 62 108) (44 92 52) (148 112 48) (88 40 92) (60 36 26)
    (162 140 104))
  "Cloth and leather bindings, as sRGB bytes.")

(defun bookshelf-part (x y)
  "Return :BOARD, :GAP, or the book's (column . height) at X,Y.

The side of a bookshelf is two shelves of books between three boards.  Each
book is two texels wide; its height varies, and above it is the dark back of
the case."
  (let ((shelf-top (if (< y 8) 1 9)))
    (cond ((or (<= y 0) (= y 8) (>= y 15)
               (zerop x) (= x +block-atlas-last-texel+))
           :board)
          (t
           (let* ((column (floor (1- x) 2))
                  (height (+ 4 (mod (block-atlas-lattice-hash column shelf-top 551)
                                    3)))
                  (book-top (- (+ shelf-top 6) height)))
             (if (< y book-top) :gap (cons column shelf-top)))))))

(defmethod paint-block-atlas-tile ((tile (eql :bookshelf)) x y)
  "Bookshelf: two shelves of bound books in a plank case."
  (let ((part (bookshelf-part x y)))
    (case part
      (:board (shaded-block-atlas-pixel 150 102 52 (block-atlas-grain tile x y 4)))
      (:gap (shaded-block-atlas-pixel 40 26 16))
      (t
       (destructuring-bind (red green blue)
           (nth (mod (block-atlas-lattice-hash (car part) (cdr part) 553)
                     (length *book-colours*))
                *book-colours*)
         ;; A spine's left texel catches the light; a gilt band crosses it.
         (shaded-block-atlas-pixel
          red green blue
          (+ (if (oddp x) 14 -6)
             (if (= y (+ (cdr part) 4)) 50 0))))))))

(defmethod paint-block-atlas-relief ((tile (eql :bookshelf)) x y)
  (let ((part (bookshelf-part x y)))
    (case part
      (:board 180)
      (:gap 40)
      (t (if (oddp x) 150 130)))))

(defun crate-part (x y)
  (cond ((< (block-atlas-edge-distance x y) 2) :frame)
        ((<= (abs (- x y)) 1) :brace)
        ((zerop (mod y 5)) :seam)
        (t :board)))

(defmethod paint-block-atlas-tile ((tile (eql :crate)) x y)
  "Crate: rough boards behind a nailed frame and a diagonal brace."
  (let ((grain (block-atlas-grain tile x y 4)))
    (ecase (crate-part x y)
      (:frame (shaded-block-atlas-pixel 120 82 44 (+ grain 6)))
      (:brace (shaded-block-atlas-pixel 128 88 48 (+ grain 4)))
      (:seam (shaded-block-atlas-pixel 84 58 32))
      (:board (shaded-block-atlas-pixel 182 140 86 grain)))))

(defmethod paint-block-atlas-relief ((tile (eql :crate)) x y)
  (ecase (crate-part x y)
    (:frame 190) (:brace 180) (:seam 60) (:board 130)))

(defun barrel-hoop-p (y)
  (member y '(2 3 12 13)))

(defmethod paint-block-atlas-tile ((tile (eql :barrel-side)) x y)
  "Barrel side: curved staves bound by two black iron hoops."
  (if (barrel-hoop-p y)
      (shaded-block-atlas-pixel 52 50 50 (if (member y '(2 12)) 18 0))
      (let ((stave (mod x 4)))
        (if (zerop stave)
            (shaded-block-atlas-pixel 70 44 24)
            (shaded-block-atlas-pixel
             130 86 46 (+ (* 6 (- 2 (abs (- stave 2))))
                          (block-atlas-grain tile x y 4)))))))

(defmethod paint-block-atlas-relief ((tile (eql :barrel-side)) x y)
  (cond ((barrel-hoop-p y) 190)
        ((zerop (mod x 4)) 70)
        (t (+ 130 (* 12 (- 2 (abs (- (mod x 4) 2))))))))

(defmethod paint-block-atlas-tile ((tile (eql :barrel-top)) x y)
  "Barrel top: a planked lid set into the rim of its hoop."
  (let ((radius (reel-radius x y)))
    (cond ((> radius 7.2) (shaded-block-atlas-pixel 70 44 24))
          ((> radius 6.0) (shaded-block-atlas-pixel 60 58 56 12))
          ((zerop (mod x 4)) (shaded-block-atlas-pixel 92 60 32))
          (t (shaded-block-atlas-pixel 150 104 58
                                       (block-atlas-grain tile x y 4))))))

(defmethod paint-block-atlas-relief ((tile (eql :barrel-top)) x y)
  (let ((radius (reel-radius x y)))
    (cond ((> radius 7.2) 100)
          ((> radius 6.0) 190)
          ((zerop (mod x 4)) 90)
          (t 140))))

(defmethod paint-block-atlas-tile ((tile (eql :hay-top)) x y)
  "Hay bale top: cut straw ends, packed tight."
  (let ((clump (- (block-atlas-clump x y 561 2) 128))
        (fleck (block-atlas-lattice-hash x y 562)))
    (pack-block-atlas-rgba (+ 204 (round clump 6) (if (> fleck 220) -40 0))
                           (+ 172 (round clump 6) (if (> fleck 220) -40 0))
                           (+ 78 (round clump 8)))))

(defmethod paint-block-atlas-relief ((tile (eql :hay-top)) x y)
  (block-atlas-byte
   (+ 70 (* 0.5 (block-atlas-clump x y 561 2))
      (* 0.3 (block-atlas-lattice-hash x y 562)))))

(defun hay-twine-p (x)
  (member x '(3 4 11 12)))

(defmethod paint-block-atlas-tile ((tile (eql :hay-side)) x y)
  "Hay bale side: long straw bound by two runs of red twine."
  (if (hay-twine-p x)
      (shaded-block-atlas-pixel 168 44 36 (if (evenp y) 10 -10))
      (shaded-block-atlas-pixel
       206 174 82 (+ (round (* 18 (sin (+ (* 0.5 x) (* 2.3 y)
                                          (* 0.003 (block-atlas-lattice-hash
                                                    0 y 571))))))
                     (block-atlas-grain tile x y 5)))))

(defmethod paint-block-atlas-relief ((tile (eql :hay-side)) x y)
  (if (hay-twine-p x)
      90
      (block-atlas-byte (+ 140 (* 30 (sin (+ (* 0.5 x) (* 2.3 y))))))))

;;; ---------------------------------------------------------------------
;;; Metal.

(defun riveted-plate-part (x y)
  "Return :SEAM, :RIVET, or :PLATE for a face of four riveted 8x8 plates."
  (let ((u (mod x 8)) (v (mod y 8)))
    (cond ((or (zerop u) (zerop v)) :seam)
          ((and (member u '(2 6)) (member v '(2 6))) :rivet)
          (t :plate))))

(defun metal-sheen (x y)
  "A soft diagonal highlight, as if a window were reflected in the plate."
  (round (* 24 (max 0.0 (- 1.0 (/ (abs (- (+ x y) 11)) 5.0))))))

(defmethod paint-block-atlas-tile ((tile (eql :copper)) x y)
  "Copper: bright riveted sheet, freshly hung."
  (ecase (riveted-plate-part x y)
    (:seam (shaded-block-atlas-pixel 118 58 34))
    (:rivet (shaded-block-atlas-pixel 238 164 112))
    (:plate (shaded-block-atlas-pixel
             196 112 66 (+ (metal-sheen x y) (block-atlas-grain tile x y 6))))))

(defmethod paint-block-atlas-relief ((tile (eql :copper)) x y)
  (ecase (riveted-plate-part x y) (:seam 70) (:rivet 220) (:plate 145)))

(defmethod paint-block-atlas-tile ((tile (eql :verdigris)) x y)
  "Verdigris: the same copper after a century of rain, green but for patches."
  (let ((bare-p (> (block-atlas-clump x y 581 4) 196)))
    (ecase (riveted-plate-part x y)
      (:seam (shaded-block-atlas-pixel 44 92 76))
      (:rivet (shaded-block-atlas-pixel 150 206 184))
      (:plate
       (if bare-p
           (shaded-block-atlas-pixel 150 92 58 (block-atlas-grain tile x y 4))
           (shaded-block-atlas-pixel
            84 160 136 (+ (round (- (block-atlas-clump x y 582 2) 128) 8)
                          (block-atlas-grain tile x y 5))))))))

(defmethod paint-block-atlas-relief ((tile (eql :verdigris)) x y)
  (ecase (riveted-plate-part x y)
    (:seam 70) (:rivet 210)
    (:plate (block-atlas-byte
             (+ 140 (* 0.25 (- (block-atlas-clump x y 582 2) 128)))))))

(defun gold-ornament (x y)
  "Return :BORDER, :LOZENGE, or :FIELD for an ornamented gilt panel."
  (let ((edge (block-atlas-edge-distance x y))
        (diamond (+ (abs (- x 7.5)) (abs (- y 7.5)))))
    (cond ((= edge 1) :border)
          ((< (abs (- diamond 4.5)) 0.6) :lozenge)
          ((< diamond 1.6) :lozenge)
          (t :field))))

(defmethod paint-block-atlas-tile ((tile (eql :gold)) x y)
  "Gold: a gilt panel with a beaded border and a lozenge in its middle."
  (let ((sheen (metal-sheen x y)))
    (ecase (gold-ornament x y)
      (:border (shaded-block-atlas-pixel 255 222 120 (- sheen 10)))
      (:lozenge (shaded-block-atlas-pixel 252 214 104 sheen))
      (:field (shaded-block-atlas-pixel
               214 160 52 (+ sheen (* 14 (block-atlas-bevel-light x y))
                             (block-atlas-grain tile x y 6)))))))

(defmethod paint-block-atlas-relief ((tile (eql :gold)) x y)
  (ecase (gold-ornament x y) (:border 200) (:lozenge 210) (:field 140)))

(defun tread-plate-boss-p (x y)
  "Whether X,Y is on one of the short raised bars of a tread plate."
  (let* ((cell-x (floor x 4)) (cell-y (floor y 4))
         (u (mod x 4)) (v (mod y 4)))
    (if (evenp (+ cell-x cell-y))
        (and (= u v) (< 0 u 4))
        (and (= (+ u v) 3) (< 0 u 3)))))

(defmethod paint-block-atlas-tile ((tile (eql :iron-plate)) x y)
  "Iron plate: steel tread plate, its bosses worn bright by boots."
  (if (tread-plate-boss-p x y)
      (shaded-block-atlas-pixel 168 172 178)
      (shaded-block-atlas-pixel
       108 112 118 (+ (round (metal-sheen x y) 2)
                      (* 10 (block-atlas-bevel-light x y))
                      (block-atlas-grain tile x y 6)))))

(defmethod paint-block-atlas-relief ((tile (eql :iron-plate)) x y)
  (cond ((tread-plate-boss-p x y) 200)
        ((zerop (block-atlas-edge-distance x y)) 110)
        (t 135)))

;;; ---------------------------------------------------------------------
;;; Furnishing and light.

(defun tapestry-part (x y)
  (let ((edge (block-atlas-edge-distance x y))
        (diamond (+ (abs (- (mod x 8) 3.5)) (abs (- (mod y 8) 3.5)))))
    (cond ((= edge 0) :fringe)
          ((= edge 1) :border)
          ((< diamond 1.2) :medallion)
          ((< (abs (- diamond 3.0)) 0.6) :lattice)
          (t :field))))

(defmethod paint-block-atlas-tile ((tile (eql :tapestry)) x y)
  "Tapestry: a woven crimson field, gold lattice, indigo medallions."
  (let ((weave (if (evenp (+ x y)) 6 -6)))
    (ecase (tapestry-part x y)
      (:fringe (shaded-block-atlas-pixel 196 164 92 weave))
      (:border (shaded-block-atlas-pixel 40 46 96 weave))
      (:medallion (shaded-block-atlas-pixel 44 54 120 weave))
      (:lattice (shaded-block-atlas-pixel 214 170 70 weave))
      (:field (shaded-block-atlas-pixel 150 34 32 weave)))))

(defmethod paint-block-atlas-relief ((tile (eql :tapestry)) x y)
  "Tapestry: the fine up-and-down of a weave, with nothing else."
  (+ 140 (if (evenp (+ x y)) 10 -10)
     (if (eq :fringe (tapestry-part x y)) -30 0)))

(defun lantern-part (x y)
  "Return :FRAME, :CAP, :GLOW, or :FLAME for the face of a caged lantern."
  (let ((edge (block-atlas-edge-distance x y)))
    (cond ((< edge 2) :frame)
          ((or (= y 2) (= y 13)) :cap)
          ((or (= x 7) (= x 8)) :frame)
          ((< (+ (abs (- x 7.5)) (* 0.6 (abs (- y 8.5)))) 3.6) :flame)
          (t :glow))))

(defmethod paint-block-atlas-tile ((tile (eql :lantern)) x y)
  "Lantern: amber glass in a black iron cage, the flame bright inside."
  (ecase (lantern-part x y)
    (:frame (shaded-block-atlas-pixel 34 32 32 (if (zerop x) 10 0)))
    (:cap (shaded-block-atlas-pixel 58 54 50))
    (:flame (pack-block-atlas-rgba 255 236 170))
    (:glow (pack-block-atlas-rgba
            (- 248 (* 3 (abs (- y 8))))
            (- 178 (* 7 (abs (- y 8))))
            (- 82 (* 4 (abs (- y 8))))))))

(defmethod paint-block-atlas-relief ((tile (eql :lantern)) x y)
  (ecase (lantern-part x y)
    (:frame 200) (:cap 180) (:flame 120) (:glow 110)))

(defun lit-window-part (x y)
  "Return :FRAME, :MULLION, :SILL, or :PANE for a four-light window."
  (let ((edge (block-atlas-edge-distance x y)))
    (cond ((>= y 14) :sill)
          ((< edge 2) :frame)
          ((or (= x 7) (= x 8) (= y 7)) :mullion)
          (t :pane))))

(defmethod paint-block-atlas-tile ((tile (eql :lit-window)) x y)
  "Lit window: four warm panes behind oak mullions, a stone sill below."
  (ecase (lit-window-part x y)
    (:sill (shaded-block-atlas-pixel 168 162 150 (if (= y 14) 16 -8)))
    (:frame (shaded-block-atlas-pixel 84 54 30 (block-atlas-grain tile x y 4)))
    (:mullion (shaded-block-atlas-pixel 100 66 36))
    ;; Lamplight from below: brighter toward each pane's lower edge.
    (:pane (let ((low (mod y 7)))
             (pack-block-atlas-rgba (+ 222 (* 4 low))
                                    (+ 158 (* 9 low))
                                    (+ 70 (* 6 low)))))))

(defmethod paint-block-atlas-relief ((tile (eql :lit-window)) x y)
  (ecase (lit-window-part x y)
    (:sill 190) (:frame 180) (:mullion 170) (:pane 100)))

;;; ---------------------------------------------------------------------
;;; Finish.  Glazed tile, honed stone, and planed wood are smooth, but not
;;; mathematically so: a field of exactly equal height shades like plastic.
;;; Every architectural tile's relief therefore gets the same faint grain on
;;; top of its construction.

(defun block-atlas-relief-grain (tile x y &optional (amplitude 10))
  "A little height noise for TILE at X,Y, within +/- AMPLITUDE."
  (round (* amplitude (- (block-atlas-lattice-hash
                          x y (+ 601 (block-atlas-tile-offset tile)))
                         128))
         128))

(macrolet ((define-finished-reliefs (&rest tiles)
             `(progn
                ,@(loop for tile in tiles
                        collect
                        `(defmethod paint-block-atlas-relief :around
                             ((tile (eql ,tile)) x y)
                           (block-atlas-byte
                            (+ (call-next-method)
                               (block-atlas-relief-grain tile x y))))))))
  (define-finished-reliefs
    :polished-stone :chiseled-stone :marble :marble-tiles :granite
    :basalt-top :basalt-side :limestone :mossy-stone-bricks
    :cracked-stone-bricks :terracotta :plaster :concrete :checker-floor
    :mosaic :blue-tiles :obsidian :roof-tiles :slate-shingles :thatch
    :dark-planks :birch-planks :parquet :timber-side :timber-end :wainscot
    :half-timber :bookshelf :crate :barrel-side :barrel-top :hay-top
    :hay-side :copper :verdigris :gold :iron-plate :tapestry :lantern
    :lit-window))
