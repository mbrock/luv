;;; A modal McCLIM inventory presented over the live luvcraft canvas.

(in-package #:mcluv)

(defconstant +inventory-view-width+ 696)
(defconstant +inventory-view-height+ 430)
(defconstant +inventory-grid-left+ 24)
(defconstant +inventory-grid-top+ 78)
(defconstant +inventory-grid-right+ 672)
(defconstant +inventory-grid-bottom+ 406)
(defconstant +inventory-grid-columns+ 3)

(defclass inventory-pane (application-pane) ())

(defun inventory-entry-label (entry)
  (string-upcase
   (symbol-name
    (luvcraft:block-kind-name
     (luvcraft:block-inventory-entry-block entry)))))

(defun inventory-entry-quantity-label (entry)
  (alexandria:if-let
      ((quantity (luvcraft:block-inventory-entry-quantity entry)))
    (format nil "~D" quantity)
    "UNLIMITED"))

(defun inventory-material-number (block)
  (1+ (or (position block (luvcraft:placeable-block-kinds) :test #'eq)
          0)))

(defun inventory-visible-state-for (frame)
  (let ((session (inventory-session frame)))
    (list
     (luvcraft:luvcraft-session-selected-block session)
     (loop for entry in
             (luvcraft:block-inventory-entries
              (luvcraft:luvcraft-session-inventory session))
           collect (list entry
                         (luvcraft:block-inventory-entry-quantity entry))))))

(defmethod handle-repaint ((pane inventory-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (session (inventory-session frame))
         (inventory (luvcraft:luvcraft-session-inventory session))
         (entries (luvcraft:block-inventory-entries inventory))
         (selected (luvcraft:luvcraft-session-selected-block session))
         (rows (max 1 (ceiling (length entries) +inventory-grid-columns+)))
         (cell-width (/ (- +inventory-grid-right+ +inventory-grid-left+)
                        +inventory-grid-columns+))
         (cell-height (/ (- +inventory-grid-bottom+ +inventory-grid-top+)
                         rows)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'luv-raster-medium)
          (clear-raster-medium-reliefs medium))
        (draw-analytic-rounded-rectangle*
         medium left top right bottom :radius 24
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.15 0.17 0.18)
               (make-rgb-color 0.025 0.03 0.035)))
        (draw-text* pane "INVENTORY" 24 34
                    :align-y :center :text-size 24 :ink +white+)
        (draw-text* pane "click a material  ·  I or Esc to return"
                    (- right 24) 35 :align-x :right :align-y :center
                    :text-size 13 :ink (make-rgb-color 0.68 0.72 0.71))
        (loop for entry in entries
              for index from 0
              for column = (mod index +inventory-grid-columns+)
              for row = (floor index +inventory-grid-columns+)
              for block = (luvcraft:block-inventory-entry-block entry)
              for chosen-p = (eq block selected)
              for x1 = (+ +inventory-grid-left+ (* column cell-width) 5)
              for y1 = (+ +inventory-grid-top+ (* row cell-height) 5)
              for x2 = (+ +inventory-grid-left+ (* (1+ column) cell-width) -5)
              for y2 = (+ +inventory-grid-top+ (* (1+ row) cell-height) -5)
              for number = (inventory-material-number block)
              do (draw-analytic-rounded-rectangle*
                  medium x1 y1 x2 y2 :radius 13
                  :ink (make-linear-gradient
                        0 y1 0 y2
                        (hotbar-scaled-color
                         (nth (1- number) *hotbar-material-colors*)
                         (if chosen-p 1.22 0.82))
                        (make-rgb-color 0.035 0.043 0.047)))
                 (when chosen-p
                   (draw-rectangle*
                    pane (+ x1 3) (+ y1 3) (- x2 3) (- y2 3)
                    :filled nil :line-thickness 3
                    :ink (hotbar-selection-ink y1 y2)))
                 (draw-analytic-rounded-rectangle*
                  medium (+ x1 12) (+ y1 12) (+ x1 55) (+ y1 55)
                  :radius 10 :ink (hotbar-material-ink number y1 y2))
                 (draw-text* pane (format nil "~D" (1+ index))
                             (+ x1 33.5) (+ y1 33.5)
                             :align-x :center :align-y :center :text-size 15
                             :ink +white+)
                 (draw-text* pane (inventory-entry-label entry)
                             (+ x1 68) (+ y1 27)
                             :align-y :center :text-size 17 :ink +white+)
                 (draw-text* pane (inventory-entry-quantity-label entry)
                             (+ x1 68) (+ y1 51)
                             :align-y :center :text-size 11
                             :ink (make-rgb-color 0.72 0.77 0.75)))))))

(define-application-frame luvcraft-inventory ()
  ((session :initarg :session :reader inventory-session)
   (visible-state :initform nil :accessor inventory-visible-state))
  (:menu-bar nil)
  (:panes
   (inventory (make-pane 'inventory-pane)))
  (:layouts
   (default
    (horizontally (:width +inventory-view-width+
                   :height +inventory-view-height+)
      inventory))))

(defun repaint-inventory (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  (setf (inventory-visible-state frame) (inventory-visible-state-for frame))
  frame)

(defclass luvcraft-inventory-overlay (luvcraft-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-inventory-overlay))
  (declare (ignore overlay))
  :hud)

(defun inventory-screen-state (overlay)
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
         (scale (min 1.0
                     (/ (- viewport-width 36.0) source-width)
                     (/ (- viewport-height 36.0) source-height)))
         (half-width (/ (* source-width scale) viewport-width))
         (half-height (/ (* source-height scale) viewport-height)))
    (make-array
     12 :element-type 'single-float
     :initial-contents
     (mapcar (lambda (value) (coerce value 'single-float))
             (list 0.0 0.0 0.0 1.0
                   half-width 0.0 0.0 0.0
                   0.0 half-height 0.0 0.0)))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-inventory-overlay) session pass surface-texture)
  (declare (ignore session))
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when source
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source
       :target-format (luv:gpu-texture-format surface-texture))
      (let* ((state (inventory-screen-state overlay))
             (frame-state
               (ensure-spinning-compositor-frame-state overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group pass 0 (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-inventory-overlay) session)
  (declare (ignore session))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (equal (inventory-visible-state-for frame)
                   (inventory-visible-state frame))
      (repaint-inventory frame)))
  overlay)

(defun inventory-slot-at (u v entry-count)
  "Return the zero-based inventory slot at normalized texture U,V."
  (let* ((x (* u +inventory-view-width+))
         (y (* v +inventory-view-height+))
         (rows (max 1 (ceiling entry-count +inventory-grid-columns+))))
    (when (and (<= +inventory-grid-left+ x)
               (< x +inventory-grid-right+)
               (<= +inventory-grid-top+ y)
               (< y +inventory-grid-bottom+))
      (let* ((column
               (floor (* (- x +inventory-grid-left+)
                         +inventory-grid-columns+)
                      (- +inventory-grid-right+ +inventory-grid-left+)))
             (row
               (floor (* (- y +inventory-grid-top+) rows)
                      (- +inventory-grid-bottom+ +inventory-grid-top+)))
             (slot (+ column (* row +inventory-grid-columns+))))
        (and (< slot entry-count) slot)))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-inventory-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore canvas))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (when (and (typep event 'luv:canvas-pointer-button-press-event)
               (eq :left (luv:canvas-pointer-event-button event)))
      (let* ((entries
               (luvcraft:block-inventory-entries
                (luvcraft:luvcraft-session-inventory session)))
             (slot (inventory-slot-at (first uv) (second uv)
                                      (length entries))))
        (when slot
          (luvcraft:select-luvcraft-block session (1+ slot))
          (repaint-inventory (widget-overlay-frame overlay)))))
    t))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-inventory-overlay) session canvas
     (event luv:canvas-key-press-event))
  (if (eq :escape (luv:canvas-key-event-key-name event))
      (progn
        (luvcraft:unfocus-luvcraft-session session)
        t)
      (call-next-method)))

(defmethod luvcraft:luvcraft-focus-left
    ((overlay luvcraft-inventory-overlay) session)
  (when (member overlay (luvcraft:luvcraft-session-overlays session)
                :test #'eq)
    (luvcraft:remove-luvcraft-overlay session overlay))
  overlay)

(defun open-luvcraft-inventory (session &key (title "luvcraft inventory"))
  "Create, attach, and focus SESSION's modal McCLIM inventory view."
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
              'luvcraft-inventory :frame-manager manager :enable t
              :session session))))
    (setf (frame-pretty-name frame) title
          (inventory-visible-state frame) (inventory-visible-state-for frame))
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-inventory-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (when (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror))
      (luvcraft:focus-luvcraft-session session overlay)
      overlay)))

(defun close-luvcraft-inventory (overlay)
  "Close an OPEN-LUVCRAFT-INVENTORY overlay."
  (check-type overlay luvcraft-inventory-overlay)
  (let ((session (widget-overlay-session overlay)))
    (if (eq overlay (luvcraft:luvcraft-session-modal-focus session))
        (luvcraft:unfocus-luvcraft-session session)
        (when (member overlay (luvcraft:luvcraft-session-overlays session)
                      :test #'eq)
          (luvcraft:remove-luvcraft-overlay session overlay))))
  nil)

(defmethod luvcraft:toggle-luvcraft-inventory
    ((session luvcraft:luvcraft-session))
  (alexandria:if-let
      ((overlay
         (find-if (lambda (candidate)
                    (typep candidate 'luvcraft-inventory-overlay))
                  (luvcraft:luvcraft-session-overlays session))))
    (progn (close-luvcraft-inventory overlay) t)
    (progn (open-luvcraft-inventory session) t)))
