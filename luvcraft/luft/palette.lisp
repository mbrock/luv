(in-package #:luvcraft.luft)

;;; Luvcraft's block kinds as one Luft palette.  Each kind becomes a material
;;; kind whose top, side, and bottom tones are the linear mean of the atlas
;;; tiles Luvcraft paints on those faces, so grass is green on top and dirt
;;; at the sides as it is in Luvcraft.  Light opacity, emission, and surface
;;; glow carry across; Luvcraft's single blocklight level becomes an RGB4
;;; emission tinted by the kind's display colour.  Relief and normals are
;;; not carried yet: that is the material protocol's next step (#Q3KI72).

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

(defun block-face-tone (kind face)
  "The tone of KIND's FACE, one of :TOP, :SIDE, or :BOTTOM."
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
   :surface-emission (luvcraft::block-kind-surface-emission kind)))

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
