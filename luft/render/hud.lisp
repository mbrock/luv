;;; LUFT's vocabulary for the workbench palette and keymap legend.
;;;
;;; The palette shows the placements a player can build with, lit the way
;;; the world lights them; the legend reads the atelier's own command tables.
;;; Neither owns input: 1-4 and the wheel still choose, F1 or H explains.

(in-package #:luft.render)

(defun viewer-palette-vocabulary (viewer)
  (let ((source (viewer-source viewer)))
    (when (typep source 'scene)
      (coerce (domains:identity-vocabulary-members
               (scene-material-vocabulary source))
              'list))))

(defun display-tone (tone)
  "Lift one scene-linear RGB TONE into a panel color that reads like the lit
world.  The workbench's surface already encodes its linear inks to sRGB, so
the tone stays linear; a small gain stands in for the sun and exposure."
  (mapcar (lambda (value) (max 0.0 (min 1.0 (* 1.25 value)))) tone))

(defmethod mcluv:palette-items-for ((viewer viewer))
  (viewer-palette-vocabulary viewer))

(defmethod mcluv:palette-visible-p ((viewer viewer))
  (and (typep (viewer-mode viewer) '(or world-edit-mode first-person-mode))
       (not (null (viewer-palette-vocabulary viewer)))))

(defmethod mcluv:palette-selected-item ((viewer viewer))
  (viewer-edit-material viewer))

(defmethod mcluv:palette-item-label ((viewer viewer) (item material-placement))
  (declare (ignore viewer))
  (case (material-placement-name item)
    (:terrain "Earth")
    (:sanctuary-limestone "Limestone")
    (t (string-capitalize
        (substitute #\Space #\-
                    (symbol-name (material-placement-name item)))))))

(defmethod mcluv:palette-item-tones ((viewer viewer) (item material-placement))
  (declare (ignore viewer))
  (multiple-value-bind (top side bottom)
      (material-kind-oriented-tones (material-placement-kind item))
    (values (display-tone top) (display-tone side) (display-tone bottom))))

(defmethod mcluv:palette-item-luminous-p
    ((viewer viewer) (item material-placement))
  (declare (ignore viewer))
  (plusp (material-kind-surface-emission (material-placement-kind item))))

;;; A refusal is news for a moment, not a standing condition: the viewer's
;;; last edit status persists, so the hint remembers when it last changed.

(defparameter *viewer-edit-status-hint-seconds* 2.5)

(defvar *viewer-edit-status-seen*
  (make-hash-table :test #'eq :weakness :key :synchronized t))

(defun viewer-recent-edit-status (viewer)
  (let* ((status (viewer-last-edit-status viewer))
         (now (get-internal-real-time))
         (seen (gethash viewer *viewer-edit-status-seen*)))
    (unless (eq status (car seen))
      (setf seen (cons status now)
            (gethash viewer *viewer-edit-status-seen*) seen))
    (when (< (- now (cdr seen))
             (* *viewer-edit-status-hint-seconds*
                internal-time-units-per-second))
      status)))

(defmethod mcluv:palette-hint ((viewer viewer))
  (let ((mode (viewer-mode viewer)))
    (or (case (viewer-recent-edit-status viewer)
          (:outside-domain "Out of reach")
          (:occupied "Something is standing there")
          (:not-editable "This world cannot be edited"))
        (cond
          ((and (typep mode 'first-person-mode)
                (not (viewer-pointer-captured-p viewer)))
           "Click to look around  ·  F1 or H for keys")
          ((typep mode 'world-edit-mode)
           "Left smashes  ·  right builds  ·  middle picks  ·  Esc leaves")))))

;;; ---------------------------------------------------------------------
;;; The legend.

(defparameter *viewer-pointer-legend-rows*
  '(("look around" "Mouse")
    ("smash the block you face" "Left click")
    ("build against it" "Right click")
    ("pick up its material" "Middle click")
    ("choose material" "Wheel"))
  "Pointer input, which no command table describes.  First person captures
the pointer on the first click; every later click edits.")

(defmethod mcluv:keymap-legend-sections-for ((viewer viewer))
  (declare (ignore viewer))
  `(("In the world" . luft-atelier)
    ("Pointer" . ,*viewer-pointer-legend-rows*)
    ("Any time" . luft-window)))

(defmethod mcluv:keymap-legend-command-label
    ((viewer viewer) (name (eql 'com-start-moving)) arguments table)
  (declare (ignore viewer arguments table))
  "walk")

(defmethod mcluv:keymap-legend-command-label
    ((viewer viewer) (name (eql 'com-select-material)) arguments table)
  (declare (ignore viewer arguments table))
  "choose material")

(defun toggle-viewer-keymap (viewer)
  (luv.workbench:toggle-workbench-keymap
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(clim:define-command (com-show-keys :command-table luft-window
                                    :name "Show Keys"
                                    :keystroke (:f1))
    ()
  (toggle-viewer-keymap (viewer-command-viewer)))

;;; H is the key a laptop reaches without Fn.
(clim:add-keystroke-to-command-table
 'luft-window '(:h) :command '(com-show-keys) :errorp nil)
