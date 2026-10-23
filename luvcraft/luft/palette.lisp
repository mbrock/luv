(in-package #:luvcraft.luft)

;;; Luvcraft's block kinds as one Luft palette.  Each kind becomes a material
;;; kind whose top, side, and bottom tones are the linear mean of the atlas
;;; tiles Luvcraft paints on those faces, so grass is green on top and dirt
;;; at the sides as it is in Luvcraft.  Light opacity, emission, and surface
;;; glow carry across; Luvcraft's single blocklight level becomes an RGB4
;;; emission tinted by the kind's display colour.  Relief and normals are
;;; not carried; *BLOCK-SURFACES* chooses a shader relief field per kind.
;;; Keep the original hues: existing buildings are authored in this palette.
;;; Saved-house comparison: #5AYVR9.

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

(defparameter *block-element-srgb-tones*
  '((:stone-bricks 139 142 135) (:bricks 166 72 49) (:planks 170 116 58)
    (:crystal 72 176 235) (:marble-tiles 228 226 220))
  "Unmixed element colours from the original atlas painters.
Do not average mortar or bright crystal facets into the element and then lay
those features over that average a second time.")

(defparameter *block-tone-overrides*
  '((:hay-bale 0.55 0.36 0.085) (:thatch 0.42 0.27 0.07)
    (:iron-plate 0.30 0.31 0.32)
    (:blue-tiles 0.025 0.07 0.38) (:mosaic 0.55 0.42 0.12)
    (:slate 0.085 0.105 0.13) (:basalt 0.055 0.045 0.035)
    (:obsidian 0.025 0.018 0.035))
  "Scene-linear substance colours, not atlas paintings averaged together.
Iron is neutral, not warmed; the dark family separates by hue and finish.")

(defun block-face-tone (kind face)
  "The tone of KIND's FACE, one of :TOP, :SIDE, or :BOTTOM, adjusted."
  (or (and (eq (luvcraft:block-kind-name kind) :flowers) (eq face :top)
           '(0.10 0.32 0.025))
      (cdr (assoc (luvcraft:block-kind-name kind) *block-tone-overrides*))
      (let ((element (cdr (assoc (luvcraft:block-kind-name kind) *block-element-srgb-tones*))))
        (if element (mapcar #'srgb-byte-to-linear element) (atlas-face-tone kind face)))))

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
  "Carry the original scalar blocklight equally in all three RGB4 lanes.
The game's shader applies the original warm response after squaring light;
a crystal's blue surface must not turn white wool cyan."
  (make-list 3 :initial-element (luvcraft::block-kind-light-emission kind)))

(defparameter *block-surfaces*
  ;; (relief strength roughness edge-wear . names); unnamed kinds are stone.
  '((:grass 0.8 0.9 0.5 :grass :moss :flowers)
    (:straw 0.7 0.95 0.3 :hay-bale :thatch)
    (:cactus 0.9 0.8 0.2 :cactus)
    (:grain 0.6 0.95 0.3 :gravel)
    (:grain 0.35 0.95 0.3 :dirt :mud)
    (:grain 0.12 0.95 0.3 :sand :clay :sandstone)
    (:grain 0.05 0.9 0.5 :plaster :concrete)
    (:stone 0.25 0.8 0.15 :stone :cobblestone :granite :limestone
     :mossy-stone-bricks :cracked-stone-bricks :stone-bricks :bricks
     :roof-tiles :slate-shingles :terracotta :chiseled-stone)
    (:planks 0.3 0.75 0.4 :slate)
    (:grain 0.55 0.98 0.3 :basalt)
    (:gloss 0.3 0.12 0.8 :obsidian)
    (:wood 0.4 0.85 0.15 :wood :timber :half-timber :barrel :bookshelf)
    (:leaves 0.6 0.9 0.2 :leaves)
    (:planks 0.25 0.7 0.15 :planks :dark-planks :birch-planks :parquet :wainscot
     :crate)
    (:snow 0.5 0.6 0.4 :snow)
    (:cloth 0.12 0.97 0.25 :red-wool :orange-wool :yellow-wool :lime-wool
     :light-blue-wool :blue-wool :purple-wool :pink-wool :white-wool
     :black-wool :tapestry)
    (:metal 0.15 0.28 0.9 :copper :gold :iron-plate :tape :film)
    (:grain 0.35 0.85 0.8 :verdigris)
    (:faceted 0.1 0.3 0.0 :crystal)
    (:gloss 0.05 0.3 0.15 :marble :marble-tiles :polished-stone
     :blue-tiles :mosaic :checker-floor
     :lantern :lit-window :terminal :urbit :orb-mote :fountain :lava-spring))
  "Which Luft relief, of what strength, roughness, and edge wear, cuts each
block kind (#HYBFRT).  The atlas tiles' painted relief is not carried over;
these choose the closest of the shader's world-space fields.")

(defparameter *block-patterns*
  ;; (pattern-arguments . names): laid in world space by the star shader.
  '(((:bond :courses 3 :aspect 1.5 :mortar-width 0.16
      :mortar-tone (0.0865 0.0595 0.0467) :variance 0.06)
     :bricks)
    ((:bond :courses 3 :aspect 1.5 :mortar-width 0.16
      :mortar-tone (0.0742 0.0782 0.0723) :variance 0.03)
     :stone-bricks)
    ((:bond :courses 2 :aspect 2 :mortar-width 0.10
      :mortar-tone (0.10 0.14 0.045) :variance 0.25
      :accent-tone (0.055 0.16 0.02))
     :mossy-stone-bricks)
    ((:bond :courses 2 :aspect 2 :mortar-width 0.18
      :mortar-tone (0.07 0.065 0.06) :variance 0.5
      :accent-tone (0.08 0.07 0.06))
     :cracked-stone-bricks)
    ((:figured :courses 2 :mortar-tone (0.12 0.12 0.11)
      :accent-tone (0.055 0.05 0.045))
     :chiseled-stone)
    ((:vein :courses 3 :accent-tone (0.28 0.27 0.25)) :marble)
    ((:fleck :courses 4 :accent-tone (0.025 0.02 0.02)) :granite)
    ((:fleck :courses 1 :accent-tone (0.85 0.45 0.75)) :flowers)
    ((:shingle :courses 4 :aspect 1 :mortar-width 0.08
      :mortar-tone (0.10 0.06 0.05) :variance 0.18)
     :roof-tiles)
    ((:shingle :courses 4 :aspect 1.5 :mortar-width 0.08
      :mortar-tone (0.06 0.07 0.08) :variance 0.15)
     :slate-shingles)
    ((:grid :courses 2 :aspect 1 :mortar-width 0.06
      :mortar-tone (0.55 0.55 0.52) :variance 0.06)
     :blue-tiles)
    ((:checker :courses 2 :mortar-width 0.06
      :mortar-tone (0.434 0.445 0.456) :variance 0.015)
     :marble-tiles)
    ((:grid :courses 6 :aspect 1 :mortar-width 0.10
      :mortar-tone (0.45 0.43 0.40) :variance 0.30
      :accent-tone (0.55 0.025 0.06))
     :mosaic)
    ((:checker :courses 2 :aspect 1 :mortar-width 0.02
      :mortar-tone (0.05 0.05 0.05) :variance 0.03)
     :checker-floor)
    ((:bond :courses 3 :aspect 7.5 :mortar-width 0.05
      :mortar-tone (0.12 0.08 0.05) :variance 0.15)
     :planks :dark-planks :birch-planks)
    ((:shelf :courses 2 :aspect 3.5 :mortar-width 0.125
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
    ((:panel :courses 1 :mortar-width 0.04
      :mortar-tone (0.075 0.08 0.085) :variance 0.0)
     :iron-plate)
    ((:figured :courses 2 :mortar-tone (0.48 0.27 0.055)
      :accent-tone (0.65 0.30 0.07))
     :tapestry)
    ((:bound :courses 2 :mortar-tone (0.12 0.065 0.02))
     :hay-bale)
    ((:grid :courses 4 :aspect 1 :mortar-width 0.05
      :mortar-tone (0.10 0.07 0.04) :variance 0.15)
     :parquet))
  "Which block kinds are laid in a pattern of bricks, tiles, shingles, or
boards, or a face-aware one of books, panes, timbers, or panels, and how.
The kind's tone is the elements'; mortar has its own.")

(defparameter *block-bodies*
  '((:barrel . :barrel) (:crate . :crate) (:tape . :reel) (:film . :reel)
    (:terminal . :screen) (:urbit . :screen)
    (:fountain . :fountain) (:lava-spring . :lava-spring))
  "Block kinds drawn as fixture bodies rather than cubical surfaces.")

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
   :top-tone (case (luvcraft:block-kind-name kind)
               (:terminal '(0.16 0.85 0.26))
               (:urbit '(0.65 0.40 0.9))
               (:fountain '(0.025 0.30 0.55))
               (:lava-spring '(1.0 0.18 0.015))
               (otherwise (block-face-tone kind :top)))
   :side-tone (block-face-tone kind :side)
   :bottom-tone (block-face-tone kind :bottom)
   :light-opacity (luvcraft::block-kind-light-opacity kind)
   :light-emission (block-light-emission-rgb kind)
   :surface-emission (case (luvcraft:block-kind-name kind)
                       ((:terminal :urbit) 0.8)
                       (otherwise (luvcraft::block-kind-surface-emission kind)))
   ;; A lantern is drawn as a lantern standing on, or hanging from, its
   ;; neighbour rather than as a cube.
   :attachment-form (and (eq (luvcraft:block-kind-name kind) :lantern) :lantern)
   :relief (first (block-surface (luvcraft:block-kind-name kind)))
   :relief-strength (second (block-surface (luvcraft:block-kind-name kind)))
   :roughness (third (block-surface (luvcraft:block-kind-name kind)))
   :edge-wear (fourth (block-surface (luvcraft:block-kind-name kind)))
   :pattern (block-pattern (luvcraft:block-kind-name kind))
   :body (or (cdr (assoc (luvcraft:block-kind-name kind) *block-bodies*)) :none)))

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
