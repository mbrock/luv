;;; The palette is a shared, passive application instrument: a row of the
;;; things a player is holding, with the chosen one lit.  It reads a small
;;; semantic protocol and never changes application state itself; choosing
;;; remains the application's keys and wheel.  Nothing here knows about a
;;; LUFT viewer or its materials.

(in-package #:mcluv)

(defconstant +palette-slot-width+ 64)
(defconstant +palette-slot-height+ 60)
(defconstant +palette-gap+ 6)
(defconstant +palette-pad+ 12)
(defconstant +palette-hint-height+ 34
  "Room above the bar for one short hint pill.  It is reserved whether or not
a hint is showing, so a hint appearing never moves the bar.")
(defconstant +palette-bar-height+ 104)
(defconstant +palette-height+ (+ +palette-hint-height+ +palette-bar-height+))
(defconstant +palette-bottom-margin+ 18)
(defconstant +palette-maximum-items+ 10)
(defconstant +palette-caption-width+ 196)

(defun palette-alpha-ink (red green blue alpha)
  (compose-in (make-rgb-color red green blue) (make-opacity alpha)))

(defparameter *palette-shadow-ink* (palette-alpha-ink 0.0 0.0 0.0 0.30))
(defparameter *palette-edge-ink* (palette-alpha-ink 0.16 0.20 0.17 0.80))
(defparameter *palette-well-ink* (palette-alpha-ink 0.004 0.005 0.005 0.70))
(defparameter *palette-caption-ink* (palette-alpha-ink 0.004 0.005 0.005 0.82))
(defparameter *palette-text-ink* (make-rgb-color 0.93 0.92 0.87))
(defparameter *palette-muted-ink* (make-rgb-color 0.54 0.56 0.52))
(defparameter *palette-accent* '(0.95 0.86 0.55))

;;; ---------------------------------------------------------------------
;;; Application protocol.

(defgeneric palette-items-for (owner)
  (:documentation
   "Return OWNER's ordered palette items.  Items are the application's own
identities; NIL means OWNER has no palette."))

(defmethod palette-items-for (owner)
  (declare (ignore owner))
  nil)

(defgeneric palette-visible-p (owner)
  (:documentation "Whether OWNER's palette belongs on screen right now."))

(defmethod palette-visible-p (owner)
  (not (null (palette-items-for owner))))

(defgeneric palette-selected-item (owner)
  (:documentation "Return the member of PALETTE-ITEMS-FOR currently chosen."))

(defmethod palette-selected-item (owner)
  (declare (ignore owner))
  nil)

(defgeneric palette-item-label (owner item)
  (:documentation "Return ITEM's short display name."))

(defmethod palette-item-label (owner item)
  (declare (ignore owner))
  (string-capitalize (substitute #\Space #\- (princ-to-string item))))

(defgeneric palette-item-tones (owner item)
  (:documentation
   "Return ITEM's top, side, and underside display colors as three (R G B)
lists of linear 0..1 components, as every workbench ink is."))

(defgeneric palette-item-luminous-p (owner item)
  (:documentation "Whether ITEM gives off light and should look like it."))

(defmethod palette-item-luminous-p (owner item)
  (declare (ignore owner item))
  nil)

(defgeneric palette-item-key (owner item index)
  (:documentation "Return the key hint naming ITEM at zero-based INDEX."))

(defmethod palette-item-key (owner item index)
  (declare (ignore owner item))
  (format nil "~D" (mod (1+ index) 10)))

(defgeneric palette-hint (owner)
  (:documentation
   "Return one short line of advice or recent outcome, or NIL for none."))

(defmethod palette-hint (owner)
  (declare (ignore owner))
  nil)

;;; ---------------------------------------------------------------------
;;; A retained snapshot, sampled outside repaint.

(defstruct palette-slot label tones luminous-p key)

(defstruct palette-snapshot
  (visible-p nil)
  (slots nil)
  (selected nil)
  (hint nil))

(defclass palette-pane (transparent-gpu-application-pane) ())

(defclass palette-state ()
  ;; Slot names are prefixed: the workbench frame mixes several instrument
  ;; states together, and a shared name would be one shared slot.  OWNER is
  ;; deliberately shared, as every instrument's owner is the application.
  ((owner :initarg :owner :initform nil :reader palette-owner)
   (palette-snapshot :initform (make-palette-snapshot)
                     :accessor palette-snapshot)
   (palette-dirty-p :initform t :accessor palette-dirty-p)))

(defun sample-palette (owner)
  "Copy OWNER's palette into an immutable snapshot, or an empty one on error."
  (handler-case
      (if (not (palette-visible-p owner))
          (make-palette-snapshot)
          (let* ((items (subseq-at-most (palette-items-for owner)
                                        +palette-maximum-items+))
                 (selected (palette-selected-item owner)))
            (make-palette-snapshot
             :visible-p (not (null items))
             :slots
             (loop for item in items
                   for index from 0
                   collect
                   (make-palette-slot
                    :label (palette-item-label owner item)
                    :tones (multiple-value-list
                            (palette-item-tones owner item))
                    :luminous-p (palette-item-luminous-p owner item)
                    :key (palette-item-key owner item index)))
             :selected (position selected items :test #'eq)
             :hint (palette-hint owner))))
    (error () (make-palette-snapshot))))

(defun subseq-at-most (list count)
  (loop for item in list repeat count collect item))

(defun refresh-palette (frame)
  "Observe FRAME's owner and mark the palette dirty only when it changed."
  (let ((snapshot (sample-palette (palette-owner frame))))
    (unless (equalp snapshot (palette-snapshot frame))
      (setf (palette-snapshot frame) snapshot
            (palette-dirty-p frame) t)))
  frame)

(defun palette-logical-width (frame)
  (let ((count (max 1 (length (palette-snapshot-slots
                               (palette-snapshot frame))))))
    (max (+ +palette-caption-width+ (* 2 +palette-pad+))
         (+ (* 2 +palette-pad+)
            (* count +palette-slot-width+)
            (* (1- count) +palette-gap+)))))

;;; ---------------------------------------------------------------------
;;; Painting.  Only analytic rounded rectangles and text: the palette shares
;;; the workbench mirror, whose direct presentation admits no fallbacks.

(defun palette-color (components &optional (scale 1.0) (alpha 1.0))
  (destructuring-bind (red green blue) components
    (let ((color (make-rgb-color (min 1.0 (* red scale))
                                 (min 1.0 (* green scale))
                                 (min 1.0 (* blue scale)))))
      (if (< alpha 1.0)
          (compose-in color (make-opacity alpha))
          color))))

(defun draw-palette-block (medium left top size tones luminous-p)
  "Draw a beveled block seen from a little above: a lit top, a front face,
and the dark chamfer that runs round both, as the world draws its cells."
  (destructuring-bind (top-tone side-tone bottom-tone) tones
    (let* ((right (+ left size))
           (bottom (+ top size))
           (seam (+ top (* size 0.36))))
      (when luminous-p
        (draw-analytic-rounded-rectangle*
         medium (- left 3) (- top 3) (+ right 3) (+ bottom 3)
         :radius 10 :ink (palette-color side-tone 1.3 0.34)))
      ;; The chamfer is the block's darkest, least lit face.
      (draw-analytic-rounded-rectangle*
       medium left top right bottom
       :radius 7 :ink (palette-color bottom-tone 0.42))
      (draw-analytic-rounded-rectangle*
       medium (+ left 3) (+ top 2) (- right 3) (- seam 1)
       :radius 4
       :ink (make-linear-gradient
             0 top 0 seam
             (palette-color top-tone 1.25)
             (palette-color top-tone 0.95)))
      (draw-analytic-rounded-rectangle*
       medium (+ left 3) (+ seam 1.5) (- right 3) (- bottom 3)
       :radius 4
       :ink (make-linear-gradient
             0 seam 0 bottom
             (palette-color side-tone 1.0)
             (palette-color side-tone 0.66))))))

(defun draw-palette-pill (pane medium center-x top bottom text
                          &key (width +palette-caption-width+)
                            (ink *palette-text-ink*) (text-size 12))
  (draw-analytic-rounded-rectangle*
   medium (- center-x (/ width 2.0)) top (+ center-x (/ width 2.0)) bottom
   :radius (/ (- bottom top) 2.0) :ink *palette-caption-ink*)
  (draw-text* pane text center-x (/ (+ top bottom) 2.0)
              :align-x :center :align-y :center :text-size text-size
              :ink ink))

(defun palette-slot-left (index)
  (+ +palette-pad+ (* index (+ +palette-slot-width+ +palette-gap+))))

(defmethod handle-repaint ((pane palette-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (snapshot (palette-snapshot frame))
         (slots (palette-snapshot-slots snapshot))
         (selected (palette-snapshot-selected snapshot))
         (width (palette-logical-width frame))
         (center (/ width 2.0))
         (bar-top +palette-hint-height+)
         (bar-bottom (+ bar-top +palette-bar-height+))
         (row-left (/ (- width (+ (* 2 +palette-pad+)
                                  (* (length slots) +palette-slot-width+)
                                  (* (max 0 (1- (length slots)))
                                     +palette-gap+)))
                      2.0)))
    (when (palette-snapshot-visible-p snapshot)
      (with-sheet-medium (medium pane)
        (alexandria:when-let ((hint (palette-snapshot-hint snapshot)))
          (let ((hint-width
                  (+ 28 (text-size pane hint
                                   :text-style (make-text-style nil nil 13)))))
            (draw-palette-pill pane medium center 2 (- bar-top 8) hint
                               :width hint-width :text-size 13)))
        (draw-analytic-rounded-rectangle*
         medium 3 (+ bar-top 5) (+ width 1) (+ bar-bottom 2)
         :radius 16 :ink *palette-shadow-ink*)
        (draw-analytic-rounded-rectangle*
         medium 0 bar-top width bar-bottom
         :radius 16 :ink *palette-edge-ink*)
        (draw-analytic-rounded-rectangle*
         medium 1.5 (+ bar-top 1.5) (- width 1.5) (- bar-bottom 1.5)
         :radius 15
         :ink (make-linear-gradient
               0 bar-top 0 bar-bottom
               (palette-alpha-ink 0.030 0.034 0.031 0.86)
               (palette-alpha-ink 0.008 0.010 0.009 0.86)))
        (loop for slot in slots
              for index from 0
              for selected-p = (eql index selected)
              for left = (+ row-left (palette-slot-left index))
              for right = (+ left +palette-slot-width+)
              for top = (+ bar-top 10)
              for bottom = (+ top +palette-slot-height+)
              do (when selected-p
                   (draw-analytic-rounded-rectangle*
                    medium (- left 2) (- top 2) (+ right 2) (+ bottom 2)
                    :radius 10 :ink (palette-color *palette-accent*)))
                 (draw-analytic-rounded-rectangle*
                  medium left top right bottom :radius 8
                  :ink (if selected-p
                           (make-linear-gradient
                            0 top 0 bottom
                            (palette-color *palette-accent* 0.34)
                            (palette-color *palette-accent* 0.13))
                           *palette-well-ink*))
                 (draw-palette-block
                  medium (- (/ (+ left right) 2.0) 18) (+ top 13) 36
                  (palette-slot-tones slot) (palette-slot-luminous-p slot))
                 (draw-text* pane (palette-slot-key slot)
                             (+ left 7) (+ top 9)
                             :align-x :left :align-y :center :text-size 10
                             :ink (if selected-p
                                      (make-rgb-color 0.98 0.95 0.82)
                                      *palette-muted-ink*)))
        (when selected
          (draw-palette-pill
           pane medium center (- bar-bottom 28) (- bar-bottom 10)
           (palette-slot-label (nth selected slots))))))))

;;; ---------------------------------------------------------------------
;;; Placement.

(defun palette-placement (frame viewport-width viewport-height)
  "Return the palette's left, top, width, and height in the viewport."
  (let ((width (palette-logical-width frame)))
    (values (max 0 (floor (- viewport-width width) 2))
            (max 0 (- viewport-height +palette-height+
                      +palette-bottom-margin+))
            width
            +palette-height+)))
