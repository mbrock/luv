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

(defun paint-terminal-mode-hotbar (pane display)
  "Paint the focused terminal's two semantic modes into PANE."
  (with-bounding-rectangle* (left top right bottom) pane
    (with-sheet-medium (medium pane)
      (when (typep medium 'luv-raster-medium)
        (clear-raster-medium-reliefs medium))
      (draw-analytic-rounded-rectangle*
       medium left top right bottom :radius 15
       :ink (make-linear-gradient
             0 top 0 bottom
             (make-rgb-color 0.13 0.15 0.18)
             (make-rgb-color 0.025 0.03 0.04)))
      (let* ((content-left (+ left 6))
             (content-right (- right 6))
             (content-top (+ top 6))
             (content-bottom (- bottom 6))
             (slot-width (/ (- content-right content-left) 2.0)))
        (loop for mode in '(:shell :film)
              for number from 1
              for slot-left = (+ content-left (* (1- number) slot-width))
              for slot-right = (+ content-left (* number slot-width))
              for selected-p = (eq mode (luvcraft:terminal-display-mode display))
              for colors = (ecase mode
                             (:shell '(0.12 0.44 0.30))
                             (:film '(0.47 0.24 0.58)))
              do (draw-rectangle*
                  pane slot-left content-top slot-right content-bottom
                  :ink (hotbar-scaled-color colors
                                            (if selected-p 1.45 0.72)))
                 (when (= number 2)
                   (draw-rectangle*
                    pane slot-left content-top (+ slot-left 1) content-bottom
                    :ink (make-rgb-color 0.08 0.09 0.10)))
                 (when selected-p
                   (draw-rectangle*
                    pane (+ slot-left 3) (+ content-top 3)
                    (- slot-right 3) (- content-bottom 3)
                    :filled nil :line-thickness 3
                    :ink (hotbar-selection-ink content-top content-bottom)))
                 (draw-text* pane (format nil "~D" number)
                             (+ slot-left 24) (/ (+ content-top content-bottom) 2)
                             :align-x :center :align-y :center :text-size 18
                             :ink +white+)
                 (draw-text* pane (string-upcase (symbol-name mode))
                             (/ (+ slot-left slot-right) 2)
                             (/ (+ content-top content-bottom) 2)
                             :align-x :center :align-y :center :text-size 22
                             :ink +white+))))))

(defmethod handle-repaint ((pane hotbar-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (selected
           (luvcraft:luvcraft-session-selected-block (hotbar-session frame)))
         (blocks
           (luvcraft:block-inventory-quickbar-blocks
            (luvcraft:luvcraft-session-inventory (hotbar-session frame)))))
    (alexandria:when-let ((display (hotbar-terminal-display frame)))
      (paint-terminal-mode-hotbar pane display)
      (return-from handle-repaint nil))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'luv-raster-medium)
          (clear-raster-medium-reliefs medium))
        ;; One raised shell and one continuous color field make this a single
        ;; instrument. Slots meet exactly; fine rules identify positions
        ;; without opening dark cracks between nine independent gadgets.
        (draw-rectangle*
         pane left top right bottom
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.035 0.042 0.048)
               (make-rgb-color 0.012 0.016 0.020)))
        (draw-analytic-rounded-rectangle*
         medium (+ left 1) (+ top 1) (- right 1) (- bottom 1)
         :radius 15
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.18 0.20 0.22)
               (make-rgb-color 0.055 0.065 0.075)))
        (draw-analytic-rounded-rectangle*
         medium (+ left 3) (+ top 3) (- right 3) (- bottom 3)
         :radius 13
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.075 0.085 0.095)
               (make-rgb-color 0.025 0.030 0.036)))
        (let* ((content-left (+ left 6))
               (content-right (- right 6))
               (content-top (+ top 6))
               (content-bottom (- bottom 6))
               (slot-width (/ (- content-right content-left)
                              (max 1.0 (length blocks)))))
          (loop for block in blocks
                for number from 1
                for slot-left = (+ content-left (* (1- number) slot-width))
                for slot-right = (+ content-left (* number slot-width))
                for selected-p = (eq block selected)
                do (draw-rectangle*
                    pane slot-left content-top slot-right content-bottom
                    :ink (hotbar-material-ink
                          block content-top content-bottom))
                   (when (> number 1)
                     (draw-rectangle*
                      pane slot-left content-top (+ slot-left 1) content-bottom
                      :ink (make-rgb-color 0.08 0.09 0.095)))
                   (when selected-p
                     (draw-rectangle*
                      pane (+ slot-left 2) (+ content-top 2)
                      (- slot-right 2) (- content-bottom 2)
                      :filled nil :line-thickness 2
                      :ink (hotbar-selection-ink
                            content-top content-bottom)))
                   ;; Stable dark plates keep every label readable without
                   ;; visually separating the underlying material strip.
                   (draw-analytic-rounded-rectangle*
                    medium (+ slot-left 6) (+ content-top 5)
                    (+ slot-left 27) (+ content-top 26)
                    :radius 7 :ink (make-rgb-color 0.045 0.052 0.058))
                   (draw-analytic-rounded-rectangle*
                    medium (+ slot-left 5) (- content-bottom 25)
                    (- slot-right 5) (- content-bottom 5)
                    :radius 6 :ink (make-linear-gradient
                                    0 (- content-bottom 25) 0 content-bottom
                                    (make-rgb-color 0.12 0.14 0.15)
                                    (make-rgb-color 0.03 0.038 0.044)))
                   (draw-text* pane (format nil "~D" number)
                               (+ slot-left 16.5) (+ content-top 15.5)
                               :align-x :center :align-y :center :text-size 12
                               :ink (make-rgb-color 0.98 0.98 0.96))
                   (draw-text* pane
                               (string-upcase
                                (symbol-name (luvcraft:block-kind-name block)))
                               (/ (+ slot-left slot-right) 2)
                               (- content-bottom 14)
                               :align-x :center :align-y :center :text-size 10
                               :ink (make-rgb-color 0.98 0.98 0.96))))))))

(define-application-frame luvcraft-hotbar ()
  ((session :initarg :session :reader hotbar-session)
   (visible-selection :initform nil :accessor hotbar-visible-selection))
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'hotbar-pane)))
  (:layouts
   (default
    (horizontally (:width 846 :height 100) bar))))

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
        (if display
            (let ((slot (min 2 (1+ (floor (* (first uv) 2))))))
              (luvcraft:change-terminal-display-mode
               display session (nth (1- slot) '(:shell :film))))
            (let ((slot (min 9 (1+ (floor (* (first uv) 9))))))
              (luvcraft:select-luvcraft-block session slot)))
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
