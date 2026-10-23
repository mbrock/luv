(in-package #:luvcraft.luft)

;;; Luvcraft's block kinds as one Luft palette.  Each kind becomes a material
;;; kind whose top, side, and bottom tones are the linear mean of the atlas
;;; tiles Luvcraft paints on those faces, so grass is green on top and dirt
;;; at the sides as it is in Luvcraft.  Light opacity, emission, and surface
;;; glow carry across; Luvcraft's single blocklight level becomes an RGB4
;;; emission tinted by the kind's display colour.  Relief and normals are
;;; not carried; *BLOCK-SURFACES* chooses a shader relief field per kind,
;;; and *BLOCK-TONE-ADJUSTMENTS* warms the stone family.

(defun srgb-byte-to-linear (byte)
  (let ((c (/ byte 255.0)))
    (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4))))

(defun tile-linear-mean (tile)
  "The linear RGB mean of Luvcraft atlas TILE, as a list of three reals."
  (let ((size luvcraft::+block-atlas-tile-size+)
        (red 0.0) (green 0.0) (blue 0.0))
    (dotimes (y size)
      (dotimes (x size)
        (let ((pixel (luvcraft::paint-block-atlas-tile tile x y)))
          (incf red (srgb-byte-to-linear (ldb (byte 8 0) pixel)))
          (incf green (srgb-byte-to-linear (ldb (byte 8 8) pixel)))
          (incf blue (srgb-byte-to-linear (ldb (byte 8 16) pixel))))))
    (let ((count (* size size)))
      (list (/ red count) (/ green count) (/ blue count)))))

(defparameter *block-tone-adjustments*
  ;; (warmth gain . names)
  '((1.0 1.3 :stone :cobblestone :stone-bricks :cracked-stone-bricks
     :chiseled-stone)
    (0.9 1.0 :polished-stone :concrete)
    (1.0 1.15 :iron-plate))
  "How far toward a warm grey of its own luminance, and how much brighter,
each kind's atlas tone is moved.  Luvcraft paints its stone a cold lavender
grey, which under Luft's blue sky fill reads as blue; Luft's own rock is a
warmer, lighter grey (#HYBFRT).  Unnamed kinds keep their atlas tone.")

(defparameter +warm-grey+ '(1.10 1.0 0.80)
  "The hue of a warm grey of unit luminance: Luft's rock, a little warmer.")

(defun adjusted-tone (name tone)
  "TONE, moved by NAME's entry in *BLOCK-TONE-ADJUSTMENTS*."
  (destructuring-bind (&optional (warmth 0) (gain 1) &rest names)
      (find name *block-tone-adjustments* :key #'cddr :test #'member)
    (declare (ignore names))
    (let ((luminance (+ (* 0.2126 (first tone)) (* 0.7152 (second tone))
                        (* 0.0722 (third tone)))))
      (mapcar (lambda (c warm)
                (* gain (+ (* (- 1 warmth) c) (* warmth luminance warm))))
              tone +warm-grey+))))

(defun block-face-tone (kind face)
  "The tone of KIND's FACE, one of :TOP, :SIDE, or :BOTTOM, adjusted."
  (adjusted-tone (luvcraft:block-kind-name kind) (atlas-face-tone kind face)))

(defun atlas-face-tone (kind face)
  "The atlas tone of KIND's FACE, one of :TOP, :SIDE, or :BOTTOM."
  (let* ((tiles (luvcraft::block-kind-face-tiles kind))
         (tile (or (getf tiles face)
                   (and (eq face :side)
                        (or (getf tiles :front) (getf tiles :left)))
                   (getf tiles :all))))
    (if tile
        (tile-linear-mean tile)
        (mapcar (lambda (c) (srgb-byte-to-linear (* 255 c)))
                (luvcraft::block-kind-display-color kind)))))

(defun block-light-emission-rgb (kind)
  "Luvcraft's blocklight level of KIND as an RGB4 emission in its colour."
  (let ((level (luvcraft::block-kind-light-emission kind))
        (colour (luvcraft::block-kind-display-color kind)))
    (if (zerop level)
        '(0 0 0)
        (let ((brightest (reduce #'max colour)))
          (mapcar (lambda (c) (round (* level (/ c brightest)))) colour)))))

(defparameter *block-surfaces*
  ;; (relief strength roughness edge-wear . names); unnamed kinds are stone.
  '((:grass 0.8 0.9 0.5 :grass :moss :flowers :hay-bale :thatch)
    (:grain 0.6 0.95 0.3 :dirt :sand :gravel :clay :mud :sandstone :cactus)
    (:grain 0.25 0.9 0.5 :plaster :concrete)
    (:stone 0.8 0.8 0.7 :stone :cobblestone :slate :granite :basalt :limestone
     :obsidian :mossy-stone-bricks :cracked-stone-bricks :stone-bricks :bricks
     :roof-tiles :slate-shingles :terracotta :chiseled-stone)
    (:wood 0.9 0.85 0.6 :wood :timber :half-timber :barrel :bookshelf)
    (:leaves 0.6 0.9 0.2 :leaves)
    (:planks 0.7 0.7 0.6 :planks :dark-planks :birch-planks :parquet :wainscot
     :crate :tape :film)
    (:snow 0.5 0.6 0.4 :snow)
    (:cloth 0.35 0.97 0.25 :red-wool :orange-wool :yellow-wool :lime-wool
     :light-blue-wool :blue-wool :purple-wool :pink-wool :white-wool
     :black-wool :tapestry)
    (:gloss 0.25 0.35 0.9 :copper :gold :iron-plate)
    (:grain 0.35 0.85 0.8 :verdigris)
    (:gloss 0.15 0.15 0.8 :crystal :marble :marble-tiles :polished-stone
     :blue-tiles :mosaic :checker-floor
     :lantern :lit-window :terminal :urbit :orb-mote :fountain :lava-spring))
  "Which Luft relief, of what strength, roughness, and edge wear, cuts each
block kind (#HYBFRT).  The atlas tiles' painted relief is not carried over;
these choose the closest of the shader's world-space fields.")

(defparameter *block-patterns*
  ;; (pattern-arguments . names): laid in world space by the star shader.
  '(((:bond :courses 4 :aspect 2 :mortar-width 0.14
      :mortar-tone (0.42 0.40 0.36) :variance 0.22)
     :bricks)
    ((:bond :courses 2 :aspect 2 :mortar-width 0.10
      :mortar-tone (0.20 0.20 0.21) :variance 0.12)
     :stone-bricks :mossy-stone-bricks :cracked-stone-bricks :chiseled-stone)
    ((:shingle :courses 4 :aspect 1 :mortar-width 0.08
      :mortar-tone (0.10 0.06 0.05) :variance 0.18)
     :roof-tiles)
    ((:shingle :courses 4 :aspect 1.5 :mortar-width 0.08
      :mortar-tone (0.06 0.07 0.08) :variance 0.15)
     :slate-shingles)
    ((:grid :courses 2 :aspect 1 :mortar-width 0.06
      :mortar-tone (0.55 0.55 0.52) :variance 0.06)
     :marble-tiles :blue-tiles)
    ((:grid :courses 6 :aspect 1 :mortar-width 0.10
      :mortar-tone (0.45 0.43 0.40) :variance 0.30)
     :mosaic)
    ((:checker :courses 2 :aspect 1 :mortar-width 0.02
      :mortar-tone (0.05 0.05 0.05) :variance 0.03)
     :checker-floor)
    ((:bond :courses 4 :aspect 4 :mortar-width 0.05
      :mortar-tone (0.12 0.08 0.05) :variance 0.15)
     :planks :dark-planks :birch-planks)
    ((:shelf :courses 2 :aspect 7 :mortar-width 0.14
      :mortar-tone (0.30 0.14 0.05) :variance 0.30
      :accent-tone (0.19 0.022 0.016))
     :bookshelf)
    ((:window :courses 2 :mortar-width 0.08
      :mortar-tone (0.16 0.08 0.03) :variance 0.0
      :accent-tone (0.72 0.32 0.07))
     :lit-window)
    ((:timbered :mortar-width 0.16
      :mortar-tone (0.06 0.025 0.01) :variance 0.0
      :accent-tone (0.78 0.70 0.52))
     :half-timber)
    ((:panel :mortar-width 0.14
      :mortar-tone (0.12 0.045 0.015) :variance 0.08)
     :wainscot)
    ((:grid :courses 4 :aspect 1 :mortar-width 0.05
      :mortar-tone (0.10 0.07 0.04) :variance 0.15)
     :parquet))
  "Which block kinds are laid in a pattern of bricks, tiles, shingles, or
boards, or a face-aware one of books, panes, timbers, or panels, and how.
The kind's tone is the elements'; mortar has its own.")

(defun block-pattern (name)
  (first (find name *block-patterns* :key #'rest :test #'member)))

(defun block-surface (name)
  (or (find name *block-surfaces* :key #'cddddr :test #'member)
      (first (member :stone *block-surfaces* :key #'first))))

(defun block-material-kind (kind)
  (make-instance
   'terrain:material-kind
   :name (luvcraft:block-kind-name kind)
   :base-tone (block-face-tone kind :side)
   :top-tone (block-face-tone kind :top)
   :side-tone (block-face-tone kind :side)
   :bottom-tone (block-face-tone kind :bottom)
   :light-opacity (luvcraft::block-kind-light-opacity kind)
   :light-emission (block-light-emission-rgb kind)
   :surface-emission (luvcraft::block-kind-surface-emission kind)
   ;; A lantern is drawn as a lantern standing on, or hanging from, its
   ;; neighbour rather than as a cube.
   :attachment-form (and (eq (luvcraft:block-kind-name kind) :lantern) :lantern)
   :relief (first (block-surface (luvcraft:block-kind-name kind)))
   :relief-strength (second (block-surface (luvcraft:block-kind-name kind)))
   :roughness (third (block-surface (luvcraft:block-kind-name kind)))
   :edge-wear (fourth (block-surface (luvcraft:block-kind-name kind)))
   :pattern (block-pattern (luvcraft:block-kind-name kind))))

(defclass luvcraft-palette ()
  ((vocabulary :initarg :vocabulary :reader luvcraft-palette-vocabulary)
   (by-name :initarg :by-name :reader luvcraft-palette-by-name))
  (:documentation
   "One placement per Luvcraft block kind, in Luvcraft's own order.

Kinds are found by name: every film is its own block kind but shares the
name :FILM, and a film's reel is what the terrain draws."))

(defun make-luvcraft-palette ()
  (let* ((kinds (append luvcraft::*block-kinds*
                        ;; Films are not in the vocabulary; a reel with the
                        ;; film's tiles (ENSURE-FILM) stands in for them all.
                        (list (make-instance
                               'luvcraft::block-kind
                               :name :film
                               :face-tiles '(:front :film-flange :back :film-flange
                                             :top :reel-rim :bottom :reel-rim
                                             :left :reel-rim :right :reel-rim)
                               :display-color '(0.62 0.55 0.42)))))
         (by-name (make-hash-table :test #'eq))
         (placements
           (loop for kind in kinds
                 for name = (luvcraft:block-kind-name kind)
                 unless (gethash name by-name)
                   collect (setf (gethash name by-name)
                                 (make-instance
                                  'terrain:material-placement
                                  :name name
                                  :kind (block-material-kind kind))))))
    (make-instance 'luvcraft-palette
                   :vocabulary (domains:make-identity-vocabulary-domain
                                :members placements :limit #xff)
                   :by-name by-name)))

(defvar *luvcraft-palette* nil
  "Luvcraft's block kinds as a Luft palette, built when this system loads.")

(setf *luvcraft-palette* (make-luvcraft-palette))

(defun block-placement (block &optional (palette *luvcraft-palette*))
  "The placement for Luvcraft BLOCK, or NIL for air."
  (and block
       (or (gethash (luvcraft:block-kind-name block)
                    (luvcraft-palette-by-name palette))
           (error "Luvcraft block ~S has no Luft placement."
                  (luvcraft:block-kind-name block)))))
