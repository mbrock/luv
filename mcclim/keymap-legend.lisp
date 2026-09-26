;;; The keymap legend: what the keys do, read off the tables that decide it.
;;;
;;; Nothing here is a written-down list of bindings.  Rows are gathered by
;;; walking an application's own command tables, so rebinding a key or adding
;;; a command changes what the application says about itself.  Luvcraft's
;;; legend has the same idea; this one is the workbench instrument any
;;; application can open.

(in-package #:mcluv)

(defconstant +keymap-legend-width+ 640
  "The construction width; the realized width follows the column count.")
(defconstant +keymap-legend-column-width+ 420)
(defconstant +keymap-legend-column-gap+ 20)
(defconstant +keymap-legend-margin+ 26)
(defconstant +keymap-legend-row-height+ 27)
(defconstant +keymap-legend-section-gap+ 12)
(defconstant +keymap-legend-header-height+ 30)
(defconstant +keymap-legend-title-height+ 56)
(defconstant +keymap-legend-keys-right+ 172
  "Where a column's keys end, from its left; labels follow a gutter.")
(defconstant +keymap-legend-viewport-margin+ 24)

(defparameter *keymap-legend-shadow-ink*
  (compose-in (make-rgb-color 0.0 0.0 0.0) (make-opacity 0.42)))
(defparameter *keymap-legend-edge-ink*
  (compose-in (make-rgb-color 0.16 0.20 0.17) (make-opacity 0.90)))
(defparameter *keymap-legend-panel-ink*
  (compose-in (make-rgb-color 0.010 0.012 0.011) (make-opacity 0.90)))
(defparameter *keymap-legend-row-ink*
  (compose-in (make-rgb-color 0.048 0.054 0.050) (make-opacity 0.80)))
(defparameter *keymap-legend-key-ink* (make-rgb-color 0.58 0.78 0.54))
(defparameter *keymap-legend-text-ink* (make-rgb-color 0.93 0.92 0.87))
(defparameter *keymap-legend-muted-ink* (make-rgb-color 0.60 0.62 0.57))

;;; ---------------------------------------------------------------------
;;; Application protocol.

(defgeneric keymap-legend-sections-for (owner)
  (:documentation
   "Return OWNER's legend as (TITLE . SOURCE) pairs in reading order.

SOURCE is a command table designator, whose own keystrokes (not inherited
ones) become rows, or a list of (LABEL . KEYS) rows for input that no command
table describes, such as pointer buttons."))

(defmethod keymap-legend-sections-for (owner)
  (mapcar (lambda (table)
            (cons (string-capitalize
                   (substitute #\Space #\- (princ-to-string table)))
                  table))
          (command-menu-tables-for owner)))

(defgeneric keymap-legend-command-label (owner name arguments table)
  (:documentation
   "What OWNER's legend calls command NAME applied to ARGUMENTS, or NIL to
leave it out.  Rows merge by label, so this decides how coarse a legend is."))

(defmethod keymap-legend-command-label (owner name arguments table)
  (declare (ignore owner arguments))
  (string-downcase
   (or (command-line-name-for-command name table :errorp nil)
       (substitute #\Space #\- (symbol-name name)))))

;;; ---------------------------------------------------------------------
;;; Reading tables.

(defun keymap-legend-item-command (item gesture)
  "Return the command keystroke ITEM stands for at GESTURE, or NIL."
  (case (command-menu-item-type item)
    (:command (command-menu-item-value item))
    (:function (ignore-errors
                (funcall (command-menu-item-value item) gesture 1)))
    (t nil)))

(defun keymap-legend-table-rows (owner table)
  "Return TABLE's own keystrokes as (LABEL . KEYS) rows, one per label."
  (let ((rows nil))
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (declare (ignore menu-name))
       (alexandria:when-let*
           ((command (keymap-legend-item-command item gesture))
            (name (command-name command))
            (label (keymap-legend-command-label
                    owner name (command-arguments command) table)))
         (let ((row (assoc label rows :test #'string=)))
           (if row
               (pushnew (format-gesture gesture) (cdr row) :test #'string=)
               (push (cons label (list (format-gesture gesture))) rows)))))
     table :inherited nil)
    (mapcar (lambda (row) (cons (car row) (reverse (cdr row))))
            (nreverse rows))))

(defun keymap-legend-sections (owner)
  "Return (TITLE . ROWS) for every section of OWNER's legend with rows."
  (handler-case
      (loop for (title . source) in (keymap-legend-sections-for owner)
            for rows = (if (listp source)
                           source
                           (keymap-legend-table-rows owner source))
            when rows collect (cons title rows))
    (error (condition)
      (list (list "Legend unavailable"
                  (cons (princ-to-string condition) nil))))))

;;; Sections flow into at most two columns, split where the taller column is
;;; shortest, so a legend with a long table still fits a laptop screen.

(defun keymap-legend-section-height (section)
  (+ +keymap-legend-header-height+
     (* +keymap-legend-row-height+ (length (cdr section)))
     +keymap-legend-section-gap+))

(defun keymap-legend-columns (sections)
  "Split SECTIONS, in order, into one or two balanced columns."
  (if (< (length sections) 2)
      (list sections)
      (let ((heights (mapcar #'keymap-legend-section-height sections))
            (best nil)
            (best-height nil))
        (loop for split from 1 below (length sections)
              for left = (reduce #'+ (subseq heights 0 split))
              for right = (reduce #'+ (subseq heights split))
              for tallest = (max left right)
              when (or (null best-height) (< tallest best-height))
                do (setf best split best-height tallest))
        (list (subseq sections 0 best) (subseq sections best)))))

(defun keymap-legend-width-for (sections)
  (let ((count (length (keymap-legend-columns sections))))
    (+ (* 2 +keymap-legend-margin+)
       (* count +keymap-legend-column-width+)
       (* (max 0 (1- count)) +keymap-legend-column-gap+))))

(defun keymap-legend-height-for (sections)
  (+ +keymap-legend-title-height+
     (loop for column in (keymap-legend-columns sections)
           maximize (reduce #'+ (mapcar #'keymap-legend-section-height column)))
     +keymap-legend-margin+))

;;; ---------------------------------------------------------------------
;;; The frame state and pane.

(defclass keymap-legend-pane (transparent-gpu-application-pane) ())

(defvar *keymap-legend-construction-height* 480)

(defclass keymap-legend-state ()
  ((owner :initarg :owner :initform nil :reader keymap-legend-owner)
   (keymap-legend-sections :initform nil
                           :accessor keymap-legend-visible-sections)))

(defun refresh-keymap-legend (frame)
  "Re-read FRAME's owner's tables outside repaint."
  (setf (keymap-legend-visible-sections frame)
        (keymap-legend-sections (keymap-legend-owner frame)))
  frame)

(defun keymap-legend-placement (frame viewport-width viewport-height)
  "Return the legend's left, top, width, and height, centred in the viewport."
  (let* ((sections (keymap-legend-visible-sections frame))
         (width (keymap-legend-width-for sections))
         (height (keymap-legend-height-for sections)))
    (values (max 0 (floor (- viewport-width width) 2))
            (max +keymap-legend-viewport-margin+
                 (floor (- viewport-height height) 2))
            width
            height)))

(defun draw-keymap-legend-row (pane medium row left top)
  (destructuring-bind (label . keys) row
    (let ((middle (+ top (/ +keymap-legend-row-height+ 2)))
          (keys-right (+ left +keymap-legend-keys-right+)))
      (draw-analytic-rounded-rectangle*
       medium left (+ top 2)
       (+ left +keymap-legend-column-width+)
       (- (+ top +keymap-legend-row-height+) 1)
       :radius 6 :ink *keymap-legend-row-ink*)
      (when keys
        (draw-text* pane (format nil "~{~A~^  ~}" keys)
                    keys-right middle
                    :align-x :right :align-y :center
                    :text-size 14 :text-face :bold
                    :ink *keymap-legend-key-ink*))
      (draw-text* pane label (+ keys-right 18) middle
                  :align-y :center :text-size 14
                  :ink *keymap-legend-text-ink*))))

(defmethod handle-repaint ((pane keymap-legend-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (sections (keymap-legend-visible-sections frame))
         (width (keymap-legend-width-for sections))
         (height (keymap-legend-height-for sections))
         (margin +keymap-legend-margin+))
    (with-sheet-medium (medium pane)
      (draw-analytic-rounded-rectangle*
       medium 7 9 (- width 1) (- height 1)
       :radius 18 :ink *keymap-legend-shadow-ink*)
      (draw-analytic-rounded-rectangle*
       medium 0 0 width height :radius 18 :ink *keymap-legend-edge-ink*)
      (draw-analytic-rounded-rectangle*
       medium 2 2 (- width 2) (- height 2)
       :radius 16 :ink *keymap-legend-panel-ink*)
      (draw-text* pane "Keys" margin 32
                  :align-y :center :text-size 22 :text-face :bold
                  :ink *keymap-legend-text-ink*)
      (draw-text* pane "any key closes" (- width margin) 32
                  :align-x :right :align-y :center :text-size 14
                  :ink *keymap-legend-muted-ink*)
      (loop for column in (keymap-legend-columns sections)
            for left from margin
              by (+ +keymap-legend-column-width+ +keymap-legend-column-gap+)
            do (let ((y +keymap-legend-title-height+))
                 (dolist (section column)
                   (draw-text* pane (string-upcase (car section))
                               left (+ y 15)
                               :align-y :center :text-size 11
                               :text-face :bold
                               :ink *keymap-legend-muted-ink*)
                   (incf y +keymap-legend-header-height+)
                   (dolist (row (cdr section))
                     (draw-keymap-legend-row pane medium row left y)
                     (incf y +keymap-legend-row-height+))
                   (incf y +keymap-legend-section-gap+)))))))

(defun handle-keymap-legend-key-event (frame event)
  "Return :DISMISS when EVENT should put FRAME's legend away.

Any ordinary key does: a legend is read and then left, and the key a player
reaches for after reading is usually the one they just learned.  Modifiers
alone do not, so a chord can still be pressed to reach the release."
  (declare (ignore frame))
  (unless (member (luv:canvas-key-event-key-name event)
                  '(:lshift :rshift :lctrl :rctrl :lalt :ralt :lgui :rgui
                    :shift-left :shift-right))
    :dismiss))
