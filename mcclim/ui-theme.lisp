;;; Shared workshop paint and small drawing primitives. Geometry stays with
;;; each widget so its hit-testing and layout remain one source of truth.
(in-package #:mcluv)

(defstruct (ui-theme (:constructor %make-ui-theme)) name inks)

(defun ui-srgb-channel (byte)
  "Decode an authored sRGB byte for our linear-light GPU inks."
  (let ((v (/ byte 255.0)))
    (if (<= v 0.04045) (/ v 12.92) (expt (/ (+ v 0.055) 1.055) 2.4))))

(defun ui-hex-ink (hex)
  (make-rgb-color (ui-srgb-channel (ldb (byte 8 16) hex))
                  (ui-srgb-channel (ldb (byte 8 8) hex))
                  (ui-srgb-channel (ldb (byte 8 0) hex))))

(defun make-field-atelier-theme ()
  "Field kit's warmth with the quiet atelier's restrained surfaces. #9Z486I"
  (%make-ui-theme
   :name :field-atelier
   :inks (loop for (role hex) in
                 '((:surface #xffedc5) (:raised #xfff7e3) (:inset #xf2dcad)
                   (:forest #x1f3d2e) (:border #xd5ac73) (:text #x392b23) (:muted #x514131)
                   (:accent #x5b704d) (:on-accent #xfffbef)
                   (:selection #xb95135) (:selection-soft #xe7a889)
                   (:focus #x314f3d) (:hover #xe0e6d5) (:track #xd4d6c7)
                   (:affirm #x3c772d) (:discuss #x216ca1) (:decline #xa63d38)
                   (:danger #xa52f2a) (:shadow #x26342b))
               append (list role (if (eq role :shadow)
                                     (compose-in (ui-hex-ink hex) (make-opacity 0.18))
                                     (ui-hex-ink hex))))))

(defparameter *ui-theme* (make-field-atelier-theme)
  "Replace this immutable theme at a frame boundary to restyle retained UI.")

(defun ui-ink (role)
  (or (getf (ui-theme-inks *ui-theme*) role)
      (error "Unknown UI ink role ~S." role)))

(defun ui-text-size (role)
  "The field kit's small, comfortable type scale in logical points."
  (ecase role (:key 12) (:caption 14) (:body 16) (:heading 20)))

(defun ui-metric (role)
  (ecase role (:panel-radius 15) (:control-radius 10) (:tile-radius 6)
         (:border-width 1) (:focus-width 2)))

(defvar *ui-theme-observers* (make-hash-table :test #'eq :weakness :key :synchronized t))

(defun ui-theme-changed-p (owner)
  "Observe theme identity once per retained owner; no redraw on idle frames."
  (unless (eq (gethash owner *ui-theme-observers*) *ui-theme*)
    (setf (gethash owner *ui-theme-observers*) *ui-theme*)
    t))

(defun draw-ui-surface (medium left top right bottom
                         &key (fill :surface) (edge :border)
                           (radius (ui-metric :panel-radius)) (rim (ui-metric :border-width)))
  "A shared opaque surface and analytic rim, in logical window coordinates."
  (draw-analytic-rounded-rectangle*
   medium left top right bottom :radius radius :ink (ui-ink edge))
  (draw-analytic-rounded-rectangle*
   medium (+ left rim) (+ top rim) (- right rim) (- bottom rim)
   :radius (max 0 (- radius rim)) :ink (ui-ink fill)))

(defun draw-ui-panel (medium left top right bottom)
  "Draw the shared paper panel and its shallow shadow."
  (draw-analytic-rounded-rectangle* medium (+ left 1) (+ top 3) right bottom
                                    :radius (ui-metric :panel-radius) :ink (ui-ink :shadow))
  (draw-ui-surface medium left top (- right 1) (- bottom 3)))

(defun draw-ui-tile (medium left top right bottom &key selected hovered focused)
  (draw-ui-surface
   medium left top right bottom :radius (ui-metric :tile-radius)
   :rim (ui-metric (if (or selected focused) :focus-width :border-width))
   :edge (cond (focused :focus) (selected :selection) (t :border))
   :fill (cond (selected :selection-soft) (hovered :hover) (t :raised))))

(defun draw-ui-keycap (medium label x y &key (width 18) (height 18))
  (draw-ui-surface medium x y (+ x width) (+ y height)
                   :fill :inset :radius 4)
  (draw-text* medium label (+ x (/ width 2)) (+ y (/ height 2))
              :align-x :center :align-y :center :text-size (ui-text-size :key)
              :ink (ui-ink :text)))

(defun draw-ui-action (medium left top right bottom label key role &key hovered)
  "Paint an action, its semantic mark, and its keyboard shortcut."
  (let* ((center (/ (+ top bottom) 2))
         (ink (ui-ink (if (eq role :inset) :text :on-accent)))
         (key-width (max 17 (+ 7 (text-size medium key :text-style (make-text-style :sans-serif :roman (ui-text-size :key))))))
         (key-x (- right key-width 6)))
    (draw-ui-surface medium left top right bottom :fill role :edge (if hovered :focus :border)
                     :rim (ui-metric (if hovered :focus-width :border-width))
                     :radius (ui-metric :control-radius))
    (let ((x (+ left 14)))
      (case role
        (:discuss
         (draw-analytic-rounded-rectangle* medium (- x 7) (- center 6) (+ x 7) (+ center 4)
                                           :radius 4 :ink ink)
         (draw-analytic-rounded-rectangle* medium (- x 5) (+ center 1) (- x 2) (+ center 7)
                                           :radius 1 :ink ink))
        (:affirm
         ;; A check drawn from two round-ended strokes works in every font.
         (flet ((stroke (cx cy length angle)
                  (draw-analytic-rounded-rectangle*
                   medium (- cx (/ length 2)) (- cy 1.5) (+ cx (/ length 2)) (+ cy 1.5)
                   :radius 1.5 :ink ink :transformation (make-rotation-transformation* angle cx cy))))
           (stroke (- x 3.8) (+ center 1.5) 8 (/ pi 4))
           (stroke (+ x 2.3) center 13 (- (/ pi 4)))))
        (:decline
         (draw-text* medium "×" x center :align-x :center :align-y :center
                     :text-size 18 :text-face :bold :ink ink))))
    (draw-text* medium label (/ (+ left (if (eq role :inset) 3 21) key-x) 2) center
                :align-x :center :align-y :center :text-size (ui-text-size :body)
                :text-face :bold :ink ink)
    (draw-ui-keycap medium key key-x (- center 9) :width key-width :height 18)))

(defun draw-ui-slider (medium left right y fraction)
  (let ((x (+ left (* (max 0 (min 1 fraction)) (- right left)))))
    (draw-analytic-rounded-rectangle* medium left (- y 3) right (+ y 3)
                                      :radius 3 :ink (ui-ink :track))
    (when (> x left)
      (draw-analytic-rounded-rectangle* medium left (- y 3) x (+ y 3)
                                        :radius 3 :ink (ui-ink :accent)))
    (draw-circle* medium x y 9 :ink (ui-ink :focus))
    (draw-circle* medium x y 7 :ink (ui-ink :raised))))

(defun draw-ui-switch (medium left right y on-p)
  (draw-analytic-rounded-rectangle* medium left (- y 12) right (+ y 12)
                                    :radius 12
                                    :ink (ui-ink (if on-p :accent :track)))
  (draw-circle* medium (if on-p (- right 12) (+ left 12)) y 8
                :ink (ui-ink :raised)))

(defparameter *ui-text-style* (make-text-style :sans-serif :roman (ui-text-size :body)))

(defmacro define-ui-themed-pane (class)
  `(defmethod handle-repaint :around ((pane ,class) region)
     (declare (ignore region))
     (with-text-style (pane *ui-text-style*)
       (call-next-method))))
