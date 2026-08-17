;;; A real McCLIM hotbar, composited into luvcraft's existing game canvas.

(in-package #:mcluv)

(defclass hotbar-pane (application-pane) ())

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

(defun hotbar-selection-ink (top bottom)
  "A quiet neutral edge for the currently selected material."
  (make-linear-gradient
   0 top 0 bottom
   (make-rgb-color 0.78 0.82 0.80)
   (make-rgb-color 0.40 0.46 0.48)))

(defun hotbar-terminal-display (frame)
  (let ((focus
          (luvcraft:luvcraft-session-modal-focus (hotbar-session frame))))
    (and (typep focus 'luvcraft:terminal-display) focus)))

(defun hotbar-visible-state-for (frame)
  (alexandria:if-let ((display (hotbar-terminal-display frame)))
    (list :terminal display (luvcraft:terminal-display-mode display))
    (list :blocks
          (luvcraft:luvcraft-session-selected-block (hotbar-session frame)))))

(defparameter *terminal-display-modes* '(:shell :film)
  "The wall modes the hotbar offers, in slot order.

A presentation extension which teaches CHANGE-TERMINAL-DISPLAY-MODE a new
mode appends it here and gets a numbered slot; nothing else has to change.")

(defparameter *terminal-display-mode-colors*
  '((:shell 0.12 0.44 0.30)
    (:film 0.47 0.24 0.58)
    (:telegram 0.16 0.42 0.62)))

(defun terminal-display-mode-color (mode)
  (or (cdr (assoc mode *terminal-display-mode-colors*))
      '(0.35 0.35 0.35)))

;;;; Geometry
;;;;
;;;; One function decides where a slot is, so painting and hit-testing cannot
;;;; disagree about which one the player clicked.

(defconstant +hotbar-width+ 612)
(defconstant +hotbar-height+ 104)
(defconstant +hotbar-pad+ 12)
(defconstant +hotbar-gap+ 6)
(defconstant +hotbar-slot-height+ 60)
(defconstant +hotbar-slot-top+ 10)

(defun hotbar-slot-geometry (count index)
  "Left, top, right, and bottom of slot INDEX of COUNT."
  (let* ((content (- +hotbar-width+ (* 2 +hotbar-pad+)))
         (width (/ (- content (* +hotbar-gap+ (1- count))) (float count)))
         (left (+ +hotbar-pad+ (* index (+ width +hotbar-gap+)))))
    (values left +hotbar-slot-top+ (+ left width)
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

(defun draw-hotbar-shell (pane medium)
  "The instrument the slots sit in: one raised chassis, quietly lit."
  ;; Cover the whole sheet first.  The chassis is drawn at the bar's declared
  ;; size, and a sheet that is larger than that -- a frame outliving a resize
  ;; -- would otherwise show through as bare white.
  (with-bounding-rectangle* (left top right bottom) pane
    (draw-rectangle* pane left top right bottom
                     :ink (make-rgb-color 0.012 0.014 0.018)))
  (draw-analytic-rounded-rectangle*
   medium 0 0 +hotbar-width+ +hotbar-height+ :radius 14
   :ink (make-linear-gradient
         0 0 0 +hotbar-height+
         (make-rgb-color 0.20 0.21 0.23)
         (make-rgb-color 0.055 0.060 0.070)))
  (draw-analytic-rounded-rectangle*
   medium 2 2 (- +hotbar-width+ 2) (- +hotbar-height+ 2) :radius 12
   :ink (make-linear-gradient
         0 0 0 +hotbar-height+
         (make-rgb-color 0.085 0.092 0.105)
         (make-rgb-color 0.030 0.034 0.040)))
  ;; A single bright hairline along the top edge is what reads as "made of
  ;; something" without drawing a border around every slot.
  (draw-line* pane 12 1.5 (- +hotbar-width+ 12) 1.5
              :ink (make-rgb-color 0.42 0.45 0.50) :line-thickness 1))

(defun draw-hotbar-slot (pane medium left top right bottom
                         &key selected-p (accent '(0.95 0.86 0.55)))
  "One recessed well, lit from inside when it is the chosen one."
  (draw-analytic-rounded-rectangle*
   medium left top right bottom :radius 8
   :ink (if selected-p
            (make-linear-gradient
             0 top 0 bottom
             (hotbar-scaled-color accent 0.34)
             (hotbar-scaled-color accent 0.13))
            (make-linear-gradient
             0 top 0 bottom
             (make-rgb-color 0.028 0.032 0.038)
             (make-rgb-color 0.055 0.060 0.068))))
  (when selected-p
    (draw-analytic-rounded-rectangle*
     medium (- left 2) (- top 2) (+ right 2) (+ bottom 2) :radius 10
     :filled nil :ink (hotbar-scaled-color accent 1.0))
    (draw-analytic-rounded-rectangle*
     medium (- left 1) (- top 1) (+ right 1) (+ bottom 1) :radius 9
     :filled nil :ink (hotbar-scaled-color accent 1.0)))
  ;; The well's own top edge, darker than the chassis, is the whole recess.
  (draw-line* pane (+ left 3) (+ top 0.5) (- right 3) (+ top 0.5)
              :ink (make-rgb-color 0.014 0.016 0.020)))

(defun draw-hotbar-caption (pane medium text &key (ink '(0.93 0.93 0.90)))
  "The one label the bar carries: what is currently chosen."
  (let ((top (- +hotbar-height+ 28))
        (bottom (- +hotbar-height+ 10)))
    (draw-analytic-rounded-rectangle*
     medium (- (/ +hotbar-width+ 2.0) 96) top
     (+ (/ +hotbar-width+ 2.0) 96) bottom
     :radius 7 :ink (make-rgb-color 0.020 0.023 0.028))
    (draw-text* pane text (/ +hotbar-width+ 2.0) (/ (+ top bottom) 2.0)
                :align-x :center :align-y :center :text-size 12
                :ink (apply #'make-rgb-color ink))))

(defparameter *terminal-display-mode-glyphs*
  '((:shell . "❯") (:film . "▶") (:telegram . "✈"))
  "One mark per mode.  A chooser of three words all the same size makes the
player read; a chooser of three shapes lets them recognize.")

(defun paint-terminal-mode-hotbar (pane display)
  "Paint the focused terminal's semantic modes into PANE."
  (with-sheet-medium (medium pane)
    (when (typep medium 'luv-raster-medium)
      (clear-raster-medium-reliefs medium))
    (draw-hotbar-shell pane medium)
    (let ((count (length *terminal-display-modes*))
          (current (luvcraft:terminal-display-mode display)))
      (loop for mode in *terminal-display-modes*
            for index from 0
            for selected-p = (eq mode current)
            for accent = (terminal-display-mode-color mode)
            do (multiple-value-bind (left top right bottom)
                   (hotbar-slot-geometry count index)
                 (draw-hotbar-slot pane medium left top right bottom
                                   :selected-p selected-p :accent accent)
                 ;; A mode slot is wide and short, so the mark and the word sit
                 ;; beside each other on one line rather than stacked into
                 ;; each other's space.
                 (let ((middle (/ (+ top bottom) 2.0)))
                   (draw-text* pane
                               (or (cdr (assoc mode
                                               *terminal-display-mode-glyphs*))
                                   "?")
                               (+ left 34) middle
                               :align-x :center :align-y :center :text-size 20
                               :ink (hotbar-scaled-color
                                     accent (if selected-p 2.2 1.5)))
                   (draw-text* pane (string-capitalize (symbol-name mode))
                               (+ left 56) middle
                               :align-x :left :align-y :center :text-size 15
                               :ink (if selected-p
                                        (make-rgb-color 0.98 0.98 0.96)
                                        (make-rgb-color 0.62 0.64 0.66)))
                   (draw-text* pane (format nil "~D" (1+ index))
                               (+ left 10) (+ top 11)
                               :align-x :left :align-y :center :text-size 10
                               :ink (make-rgb-color 0.46 0.48 0.52)))))
      (draw-hotbar-caption
       pane medium
       (format nil "~A wall" (string-capitalize (symbol-name current)))))))

(defmethod handle-repaint ((pane hotbar-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (session (hotbar-session frame))
         (selected (luvcraft:luvcraft-session-selected-block session))
         (blocks
           (luvcraft:block-inventory-quickbar-blocks
            (luvcraft:luvcraft-session-inventory session))))
    (alexandria:when-let ((display (hotbar-terminal-display frame)))
      (paint-terminal-mode-hotbar pane display)
      (return-from handle-repaint nil))
    (with-sheet-medium (medium pane)
      (when (typep medium 'luv-raster-medium)
        (clear-raster-medium-reliefs medium))
      (draw-hotbar-shell pane medium)
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
                                    (- (/ (+ top bottom) 2.0) 21)
                                    40)
                   (draw-text* pane (format nil "~D" (1+ index))
                               (+ left 7) (+ top 10)
                               :align-x :left :align-y :center :text-size 10
                               :ink (if selected-p
                                        (make-rgb-color 0.98 0.95 0.82)
                                        (make-rgb-color 0.46 0.48 0.52))))))
      ;; Nine labels is nine things to read; the chosen one is the only name
      ;; that answers a question the player is actually asking.
      (draw-hotbar-caption
       pane medium
       (string-capitalize
        (substitute #\Space #\- (symbol-name
                                  (luvcraft:block-kind-name selected))))))))

(define-application-frame luvcraft-hotbar ()
  ((session :initarg :session :reader hotbar-session)
   (visible-selection :initform nil :accessor hotbar-visible-selection))
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'hotbar-pane)))
  (:layouts
   (default
    (horizontally (:width +hotbar-width+ :height +hotbar-height+) bar))))

(defun repaint-hotbar (frame)
  "Repaint and publish one complete material palette."
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  (setf (hotbar-visible-selection frame)
        (hotbar-visible-state-for frame))
  frame)

(defclass luvcraft-hotbar-overlay (luvcraft-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-hotbar-overlay))
  (declare (ignore overlay))
  :hud)

(defun hotbar-screen-state (overlay)
  (let* ((source-size
           (luv:gpu-texture-size
            (mirror-texture (widget-overlay-mirror overlay))))
         (viewport-size
           (luv:canvas-extent
            (luvcraft::luvcraft-session-context
             (widget-overlay-session overlay))))
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

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-hotbar-overlay) session pass surface-texture)
  (declare (ignore session))
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when source
      ;; No depth state: the hotbar is a HUD and must remain visible over the
      ;; scene regardless of the block depth already in the shared pass.
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source
       :target-format (luv:gpu-texture-format surface-texture))
      (let* ((state (hotbar-screen-state overlay))
             (frame-state
               (ensure-spinning-compositor-frame-state
                overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group
         pass 0 (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
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
             (display (hotbar-terminal-display frame)))
        ;; HOTBAR-SLOT-AT reads the same geometry the painter does, so a click
        ;; in the gap between two slots chooses neither rather than guessing.
        (if display
            (alexandria:when-let
                ((slot (hotbar-slot-at (length *terminal-display-modes*)
                                       (first uv))))
              (luvcraft:change-terminal-display-mode
               display session (nth slot *terminal-display-modes*)))
            (let ((count (length (luvcraft:block-inventory-quickbar-blocks
                                  (luvcraft:luvcraft-session-inventory
                                   session)))))
              (alexandria:when-let ((slot (hotbar-slot-at count (first uv))))
                (luvcraft:select-luvcraft-block session (1+ slot)))))
        (repaint-hotbar frame)))
    t))

(defmethod luvcraft:handle-luvcraft-focus-control-event
    ((display luvcraft:terminal-display) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore display))
  (some (lambda (overlay)
          (and (typep overlay 'luvcraft-hotbar-overlay)
               (luvcraft:handle-luvcraft-overlay-event
                overlay session canvas event)))
        (luvcraft:luvcraft-session-overlays session)))

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
            (luv:canvas-extent
             (luvcraft::luvcraft-session-context
              (widget-overlay-session overlay)))))
         (center-y (aref state 1))
         (half-height (abs (aref state 9)))
         (bottom-inset
           (* 0.5 height (+ (- 1.0 center-y) half-height))))
    (values 0.0 0.0 0.0 bottom-inset)))

(defun open-luvcraft-hotbar (session &key (title "luvcraft block palette"))
  "Create and attach the screen-space McCLIM block palette for SESSION."
  (let* ((port (find-port :server-path '(:luv)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (*embedded-mirror-context*
                   (luvcraft::luvcraft-session-context session))
                 (*embedded-mirror-device*
                   (luvcraft::luvcraft-session-device session)))
             (make-application-frame
              'luvcraft-hotbar :frame-manager manager :enable t
              :session session))))
    (setf (frame-pretty-name frame) title
          (hotbar-visible-selection frame)
          (hotbar-visible-state-for frame))
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-hotbar-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (when (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror))
      overlay)))

(defun close-luvcraft-hotbar (overlay)
  "Remove and release an OPEN-LUVCRAFT-HOTBAR overlay."
  (check-type overlay luvcraft-hotbar-overlay)
  (luvcraft:remove-luvcraft-overlay
   (widget-overlay-session overlay) overlay)
  nil)

(defmethod luvcraft:attach-luvcraft-hud
    ((session luvcraft:luvcraft-session))
  (open-luvcraft-hotbar session))
