;;; A real McCLIM hotbar, composited into luvcraft's existing game canvas.

(in-package #:mcluv)

(defclass hotbar-pane (transparent-gpu-application-pane) ())

(define-ui-themed-pane hotbar-pane)

(defun hotbar-material-color (block)
  (destructuring-bind (red green blue)
      (luvcraft:block-kind-display-color block)
    (make-rgb-color red green blue)))

(defun hotbar-scaled-color (components scale)
  (destructuring-bind (red green blue) components
    (make-rgb-color (min 1.0 (* red scale))
                    (min 1.0 (* green scale))
                    (min 1.0 (* blue scale)))))

(defun hotbar-material-ink (block top bottom)
  "Give BLOCK a restrained top-lit gradient from TOP to BOTTOM."
  (let ((components (luvcraft:block-kind-display-color block)))
    (make-linear-gradient
     0 top 0 bottom
     (hotbar-scaled-color components 1.18)
     (hotbar-scaled-color components 0.68))))

(defun hotbar-visible-state-for (frame)
  (let ((session (hotbar-session frame)))
    ;; The bar's own materials belong to its state: choosing a block in the
    ;; inventory can swap a new one onto it without changing the selection.
    (list *ui-theme* (luvcraft:player-selected-block session)
          (luvcraft:player-creative-p session)
          (luvcraft:block-inventory-quickbar-blocks
           (luvcraft:player-inventory session))
          (mapcar #'luvcraft:block-inventory-entry-quantity
                  (luvcraft:block-inventory-quickbar-entries
                   (luvcraft:player-inventory session))))))

;;;; Geometry
;;;;
;;;; One function decides where a slot is, so painting and hit-testing cannot
;;;; disagree about which one the player clicked.

(defconstant +hotbar-width+ 612)
(defconstant +hotbar-height+ 98)
(defconstant +hotbar-pad+ 12)
(defconstant +hotbar-gap+ 6)
(defconstant +hotbar-slot-height+ 54)
(defconstant +hotbar-slot-top+ 10)

(defun hotbar-slot-geometry (count index)
  "Left, top, right, and bottom of slot INDEX of COUNT, dividing the bar."
  (let* ((content (- +hotbar-width+ (* 2 +hotbar-pad+)))
         (slot-width
           (/ (- content (* +hotbar-gap+ (1- count))) (float count)))
         (left (+ +hotbar-pad+ (* index (+ slot-width +hotbar-gap+)))))
    (values left +hotbar-slot-top+ (+ left slot-width)
            (+ +hotbar-slot-top+ +hotbar-slot-height+))))

(defun hotbar-slot-at (count u)
  "The slot index at horizontal texture fraction U, or NIL between slots."
  (loop for index below count
        do (multiple-value-bind (left top right bottom)
               (hotbar-slot-geometry count index)
             (declare (ignore top bottom))
             (let ((x (* u +hotbar-width+)))
               (when (and (<= left x) (< x right))
                 (return index))))))

(defun draw-hotbar-shell (pane medium &key (width +hotbar-width+)
                                          (height +hotbar-height+))
  "Inventory and quickbar share the workshop's bone surface."
  (declare (ignore pane))
  (draw-ui-surface medium 0 0 width height))

(defun draw-hotbar-slot (pane medium left top right bottom
                         &key selected-p hovered-p accent)
  (declare (ignore pane accent))
  (draw-ui-tile medium left top right bottom
                :selected selected-p :hovered hovered-p))

(defun draw-hotbar-caption (pane text &key ink)
  "The one label the bar carries: what is currently chosen."
  (draw-text* pane text (/ +hotbar-width+ 2.0) (- +hotbar-height+ 17)
              :align-x :center :align-y :center :text-size 12
              :ink (if ink (apply #'make-rgb-color ink) (ui-ink :text))))

(defun hotbar-creative-button-at-p (u v)
  (and (<= 12 (* u +hotbar-width+) 146)
       (<= 70 (* v +hotbar-height+) 92)))

(defun draw-hotbar-creative-button (pane medium session)
  (let ((active (luvcraft:player-creative-p session)))
    (draw-analytic-rounded-rectangle*
     medium 12 70 146 92 :radius 5
     :ink (if active (ui-ink :accent)
              (ui-ink :inset)))
    (draw-text* pane (if active "Creative ON · F6" "Creative · F6") 79 81
                :align-x :center :align-y :center :text-size 12
                :ink (if active (ui-ink :on-accent)
                         (ui-ink :muted)))))

(defmethod handle-repaint ((pane hotbar-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (session (hotbar-session frame))
         (selected (luvcraft:player-selected-block session))
         (blocks
           (luvcraft:block-inventory-quickbar-blocks
            (luvcraft:player-inventory session))))
    (with-sheet-medium (medium pane)
      (draw-hotbar-shell pane medium)
      (draw-line* pane 12 68 (- +hotbar-width+ 12) 68
                  :ink (ui-ink :border))
      (draw-text* pane "Inventory · I" (- +hotbar-width+ 16) 81
                  :align-x :right :align-y :center :text-size 12
                  :ink (ui-ink :muted))
      (draw-hotbar-creative-button pane medium session)
      (let ((count (max 1 (length blocks))))
        (loop for block in blocks
              for index from 0
              for selected-p = (eq block selected)
              do (multiple-value-bind (left top right bottom)
                     (hotbar-slot-geometry count index)
                   (draw-hotbar-slot pane medium left top right bottom
                                     :selected-p selected-p)
                   ;; The cube is the material: the same procedural faces the
                   ;; world is built from, not a swatch of its average colour.
                   (draw-block-icon pane block
                                    (- (/ (+ left right) 2.0) 20)
                                    (- (/ (+ top bottom) 2.0) 20)
                                    40)
                   (let ((quantity
                           (luvcraft:block-inventory-entry-quantity
                            (luvcraft:block-inventory-entry-for
                             (luvcraft:player-inventory session) block))))
                     (when quantity
                       (draw-text*
                        pane
                        (if (and (luvcraft:player-creative-p session)
                                 (not (luvcraft:block-kind-carried-p block)))
                            "∞" (format nil "~D" quantity))
                        (- right 5) (- bottom 4)
                        :align-x :right :align-y :bottom :text-size 10
                        :ink (ui-ink :muted))))
                   ;; The tenth slot's key is 0, at the end of the number row.
                   (draw-ui-keycap medium (format nil "~D" (mod (1+ index) 10))
                                   (+ left 3) (+ top 3) :width 14 :height 14))))
      ;; Nine labels is nine things to read; the chosen one is the only name
      ;; that answers a question the player is actually asking.
      (draw-hotbar-caption
       pane
       (string-capitalize
        (substitute #\Space #\- (symbol-name
                                  (luvcraft:block-kind-name selected))))))))

(define-application-frame luvcraft-hotbar ()
  ((session :initarg :session :reader hotbar-session)
   (visible-selection :initform nil :accessor hotbar-visible-selection))
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'hotbar-pane
                   :width +hotbar-width+ :height +hotbar-height+)))
  (:layouts (default bar)))

(defun repaint-hotbar (frame)
  "Repaint and publish one complete material palette."
  (make-gpu-frame-background-transparent frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (check-type mirror luv-gpu-mirror)
    (repaint-gpu-mirror mirror))
  (setf (hotbar-visible-selection frame)
        (hotbar-visible-state-for frame))
  frame)

(defclass luvcraft-hotbar-overlay (luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-hotbar-overlay))
  (declare (ignore overlay))
  :hud)

(defun hotbar-screen-state (overlay)
  (let* ((source-size (widget-overlay-logical-size overlay))
         (viewport-size
           (multiple-value-list
            (luv:canvas-logical-size
             (instrument-canvas
              (widget-overlay-session overlay)))))
         (source-width (first source-size))
         (source-height (second source-size))
         (viewport-width (first viewport-size))
         (viewport-height (second viewport-size))
         (scale (min 1.0 (/ (- viewport-width 24.0) source-width)))
         (half-width (/ (* source-width scale) viewport-width))
         (half-height (/ (* source-height scale) viewport-height))
         (bottom-margin (/ 28.0 viewport-height))
         (center-y (- 1.0 bottom-margin half-height)))
    (make-array
     12 :element-type 'single-float
     :initial-contents
     (mapcar (lambda (value) (coerce value 'single-float))
             (list 0.0 center-y 0.0 1.0
                   half-width 0.0 0.0 0.0
                   0.0 half-height 0.0 0.0)))))

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay :zone :hotbar/encode)
    ((overlay luvcraft-hotbar-overlay) session pass surface-texture)
  (declare (ignore pass))
  (prepare-direct-widget-overlay
   overlay session surface-texture (hotbar-screen-state overlay))
  overlay)

(luv:zdefmethod (luvcraft:refresh-luvcraft-overlay :zone :hotbar/refresh)
    ((overlay luvcraft-hotbar-overlay) session)
  (declare (ignore session))
  (let* ((frame (widget-overlay-frame overlay))
         (state (hotbar-visible-state-for frame)))
    (unless (equal state (hotbar-visible-selection frame))
      (repaint-hotbar frame)))
  overlay)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-hotbar-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore canvas))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (when (and (typep event 'luv:canvas-pointer-button-press-event)
               (eq :left (luv:canvas-pointer-event-button event)))
      (let* ((frame (widget-overlay-frame overlay))
             (count (length (luvcraft:block-inventory-quickbar-blocks
                             (luvcraft:player-inventory session)))))
        ;; HOTBAR-SLOT-AT reads the same geometry the painter does, so a click
        ;; in the gap between two slots chooses neither rather than guessing.
        (cond
          ((hotbar-creative-button-at-p (first uv) (second uv))
           (toggle-instrument-creative-mode session))
          ((<= +hotbar-slot-top+ (* (second uv) +hotbar-height+)
               (+ +hotbar-slot-top+ +hotbar-slot-height+))
           (alexandria:when-let ((slot (hotbar-slot-at count (first uv))))
             (luvcraft:select-luvcraft-block session (1+ slot)))))
        (repaint-hotbar frame)))
    t))

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-hotbar-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft:luvcraft-overlay-focus-insets
    ((overlay luvcraft-hotbar-overlay) session)
  (declare (ignore session))
  (let* ((state (hotbar-screen-state overlay))
         (height
           (second
            (multiple-value-list
             (luv:canvas-logical-size
              (instrument-canvas
               (widget-overlay-session overlay))))))
         (center-y (aref state 1))
         (half-height (abs (aref state 9)))
         (bottom-inset
           (* 0.5 height (+ (- 1.0 center-y) half-height))))
    (values 0.0 0.0 0.0 bottom-inset)))

(defun open-luvcraft-hotbar (session &key (title "luvcraft block palette"))
  "Create and attach the screen-space McCLIM block palette for SESSION."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target*
                   (instrument-canvas session))
                 (*embedded-mirror-context*
                   (instrument-context session))
                 (*embedded-mirror-device*
                   (instrument-device session)))
             (make-application-frame
              'luvcraft-hotbar :frame-manager manager :enable t
              :session session))))
    (make-gpu-frame-background-transparent frame)
    (setf (frame-pretty-name frame) title
          (hotbar-visible-selection frame)
          (hotbar-visible-state-for frame))
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-hotbar-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mirror-compositor mirror) overlay)
      (add-instrument session overlay)
      (when (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror))
      overlay)))

(defun close-luvcraft-hotbar (overlay)
  "Remove and release an OPEN-LUVCRAFT-HOTBAR overlay."
  (check-type overlay luvcraft-hotbar-overlay)
  (remove-instrument
   (widget-overlay-session overlay) overlay)
  nil)

(defmethod luvcraft:attach-luvcraft-hud
    ((session luvcraft:luvcraft-session))
  (open-luvcraft-hotbar session))
