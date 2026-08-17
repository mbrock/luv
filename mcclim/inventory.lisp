;;; A modal McCLIM inventory presented over the live luvcraft canvas.

(in-package #:mcluv)

(defconstant +inventory-view-width+ 840)
(defconstant +inventory-view-height+ 500)
(defconstant +inventory-sidebar-left+ 8)
(defconstant +inventory-sidebar-right+ 158)
(defconstant +inventory-grid-left+ 166)
(defconstant +inventory-grid-top+ 50)
(defconstant +inventory-grid-right+ 650)
(defconstant +inventory-grid-bottom+ 228)
(defconstant +inventory-grid-columns+ 5)
(defconstant +inventory-details-left+ 658)
(defconstant +inventory-details-right+ 832)
(defconstant +inventory-quickbar-top+ 266)
(defconstant +inventory-quickbar-bottom+ 326)

(defparameter *inventory-categories*
  '((:all "All blocks")
    (:natural "Natural")
    (:building "Building")
    (:luminous "Luminous")))

(defparameter *inventory-panel-ink* (make-rgb-color 0.20 0.20 0.18))
(defparameter *inventory-well-ink* (make-rgb-color 0.105 0.105 0.095))
(defparameter *inventory-face-ink* (make-rgb-color 0.31 0.30 0.27))
(defparameter *inventory-light-edge* (make-rgb-color 0.52 0.50 0.44))
(defparameter *inventory-dark-edge* (make-rgb-color 0.055 0.055 0.05))
(defparameter *inventory-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *inventory-muted-ink* (make-rgb-color 0.65 0.65 0.59))
(defparameter *inventory-accent-ink* (make-rgb-color 0.58 0.78 0.54))

(defclass inventory-pane (application-pane) ())

(define-application-frame luvcraft-inventory ()
  ((session :initarg :session :reader inventory-session)
   (visible-state :initform nil :accessor inventory-visible-state)
   (category :initform :all :accessor inventory-category)
   (atlas :initform (luvcraft:make-block-texture-atlas)
          :reader inventory-atlas))
  (:menu-bar nil)
  (:panes
   (inventory
    (make-pane 'inventory-pane
               :default-text-style (make-text-style :serif nil :normal))))
  (:layouts
   (default
    (horizontally (:width +inventory-view-width+
                   :height +inventory-view-height+)
      inventory))))

(defun draw-inventory-bevel
    (stream left top right bottom &key (ink *inventory-face-ink*) recessed-p)
  "Draw one deliberately old-fashioned raised or recessed CLIM panel."
  (draw-rectangle* stream left top right bottom :ink ink)
  (let ((top-left (if recessed-p *inventory-dark-edge* *inventory-light-edge*))
        (bottom-right
          (if recessed-p *inventory-light-edge* *inventory-dark-edge*)))
    (draw-line* stream left top right top :ink top-left :line-thickness 2)
    (draw-line* stream left top left bottom :ink top-left :line-thickness 2)
    (draw-line* stream left bottom right bottom
                :ink bottom-right :line-thickness 2)
    (draw-line* stream right top right bottom
                :ink bottom-right :line-thickness 2)))

(defun inventory-block-preview-tile (block)
  (let ((tiles (luvcraft:block-kind-face-tiles block)))
    (or (getf tiles :top) (getf tiles :all) (getf tiles :side))))

(defun packed-inventory-ink (word)
  (make-rgb-color (/ (ldb (byte 8 0) word) 255.0)
                  (/ (ldb (byte 8 8) word) 255.0)
                  (/ (ldb (byte 8 16) word) 255.0)))

(defun draw-inventory-block-tile
    (stream atlas block left top &key (scale 3))
  "Paint BLOCK's real generated top tile as a crisp nearest-neighbour icon."
  (let ((tile (inventory-block-preview-tile block)))
    (dotimes (y 16)
      (dotimes (x 16)
        (draw-rectangle*
         stream
         (+ left (* x scale)) (+ top (* y scale))
         (+ left (* (1+ x) scale)) (+ top (* (1+ y) scale))
         :ink (packed-inventory-ink
               (aref atlas y (+ x (* tile 16)))))))))

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

(defun inventory-category-block-p (category block)
  (or (eq category :all)
      (member (luvcraft:block-kind-name block)
              (ecase category
                (:natural '(:grass :dirt :leaves :sand :snow))
                (:building '(:stone :wood :terminal))
                (:luminous '(:crystal :terminal))))))

(defun inventory-visible-entries (frame)
  (remove-if-not
   (lambda (entry)
     (inventory-category-block-p
      (inventory-category frame)
      (luvcraft:block-inventory-entry-block entry)))
   (luvcraft:block-inventory-entries
    (luvcraft:luvcraft-session-inventory (inventory-session frame)))))

(defun inventory-visible-state-for (frame)
  (let ((session (inventory-session frame)))
    (list
     (inventory-category frame)
     (luvcraft:luvcraft-session-selected-block session)
     (loop for entry in
             (luvcraft:block-inventory-entries
              (luvcraft:luvcraft-session-inventory session))
           collect (list entry
                         (luvcraft:block-inventory-entry-quantity entry))))))

(defun draw-inventory-category (frame pane category label index)
  (let* ((top (+ 50 (* index 39)))
         (selected-p (eq category (inventory-category frame))))
    (progn
      (draw-inventory-bevel
       pane 16 top 150 (+ top 34)
       :ink (if selected-p
                (make-rgb-color 0.36 0.35 0.30)
                *inventory-panel-ink*)
       :recessed-p selected-p)
      (draw-rectangle* pane 25 (+ top 9) 40 (+ top 24)
                       :ink (ecase category
                              (:all (make-rgb-color 0.72 0.56 0.24))
                              (:natural (make-rgb-color 0.20 0.50 0.18))
                              (:building (make-rgb-color 0.49 0.47 0.42))
                              (:luminous (make-rgb-color 0.18 0.72 0.74))))
      (draw-text* pane label 49 (+ top 17)
                  :align-y :center :text-size 14
                  :ink (if selected-p +white+ *inventory-text-ink*)))))

(defun draw-inventory-entry
    (frame pane entry index left top width height)
  (let* ((block (luvcraft:block-inventory-entry-block entry))
         (selected-p
           (eq block
               (luvcraft:luvcraft-session-selected-block
                (inventory-session frame)))))
    (progn
      (draw-inventory-bevel pane left top (+ left width) (+ top height)
                            :ink (if selected-p
                                     (make-rgb-color 0.39 0.39 0.34)
                                     *inventory-face-ink*)
                            :recessed-p t)
      (when selected-p
        (draw-rectangle* pane (+ left 3) (+ top 3)
                         (- (+ left width) 3) (- (+ top height) 3)
                         :ink *inventory-accent-ink*
                         :filled nil :line-thickness 2))
      (draw-inventory-block-tile
       pane (inventory-atlas frame) block (+ left 22) (+ top 9) :scale 3)
      (draw-text* pane (format nil "~D" (1+ index))
                  (+ left 9) (+ top 10) :text-size 11
                  :ink *inventory-text-ink*)
      (draw-text* pane (inventory-entry-quantity-label entry)
                  (- (+ left width) 7) (- (+ top height) 8)
                  :align-x :right :align-y :bottom :text-size 10
                  :ink +white+))))

(defun draw-inventory-quickbar (frame pane entries)
  (draw-text* pane "Quickbar" +inventory-grid-left+ 242
              :align-y :center :text-size 14 :ink *inventory-text-ink*)
  (let ((slot-width
          (/ (- +inventory-grid-right+ +inventory-grid-left+)
             (max 1 (length entries)))))
    (loop for entry in entries
          for index from 0
          for block = (luvcraft:block-inventory-entry-block entry)
          for left = (+ +inventory-grid-left+ (* index slot-width))
          for selected-p =
            (eq block
                (luvcraft:luvcraft-session-selected-block
                 (inventory-session frame)))
          do (draw-inventory-bevel
              pane left +inventory-quickbar-top+
              (+ left slot-width) +inventory-quickbar-bottom+
              :ink (if selected-p
                       (make-rgb-color 0.40 0.40 0.35)
                       *inventory-face-ink*)
              :recessed-p t)
             (when selected-p
               (draw-rectangle*
                pane (+ left 2) (+ +inventory-quickbar-top+ 2)
                (- (+ left slot-width) 2) (- +inventory-quickbar-bottom+ 2)
                :filled nil :line-thickness 2 :ink *inventory-accent-ink*))
             (draw-inventory-block-tile
              pane (inventory-atlas frame) block
              (+ left (/ (- slot-width 32) 2))
              (+ +inventory-quickbar-top+ 8) :scale 2)
             (draw-text* pane (format nil "~D" (1+ index))
                         (+ left 5) (+ +inventory-quickbar-top+ 5)
                         :text-size 9 :ink +white+))))

(defun draw-inventory-details (frame pane selected entry)
  (draw-text* pane "Item Inspector" 668 31
              :align-y :center :text-size 14 :ink *inventory-text-ink*)
  (draw-inventory-bevel pane +inventory-details-left+ 44
                        +inventory-details-right+ 356
                        :ink *inventory-panel-ink* :recessed-p t)
  (draw-inventory-block-tile pane (inventory-atlas frame) selected 674 58
                             :scale 3)
  (draw-text* pane (inventory-entry-label entry) 730 70
              :align-y :center :text-size 15 :ink +white+)
  (draw-text* pane
              (format nil "luvcraft:~(~A~)"
                      (luvcraft:block-kind-name selected))
              730 92 :align-y :center :text-size 10
              :ink *inventory-muted-ink*)
  (draw-line* pane 668 119 822 119 :ink *inventory-dark-edge*)
  (draw-text* pane "Details" 668 138 :align-y :center
              :text-size 13 :ink *inventory-text-ink*)
  (loop for label in '("Type" "Light opacity" "Light emission"
                       "Surface emission" "Stack")
        for value in (list "Block"
                           (luvcraft:block-light-opacity selected)
                           (luvcraft:block-light-emission selected)
                           (format nil "~,2F"
                                   (luvcraft:block-surface-emission selected))
                           (inventory-entry-quantity-label entry))
        for y from 162 by 24
        do (draw-text* pane label 668 y :align-y :center :text-size 10
                       :ink *inventory-muted-ink*)
           (draw-text* pane (format nil "~A" value) 820 y
                       :align-x :right :align-y :center :text-size 10
                       :ink *inventory-text-ink*))
  (draw-line* pane 668 294 822 294 :ink *inventory-dark-edge*)
  (draw-text* pane "Presentation" 668 313 :align-y :center
              :text-size 12 :ink *inventory-text-ink*)
  (draw-text* pane
              (format nil "#<BLOCK-KIND ~(~A~)>"
                      (luvcraft:block-kind-name selected))
              668 336 :align-y :center :text-size 10
              :ink *inventory-accent-ink*))

(defmethod handle-repaint ((pane inventory-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (session (inventory-session frame))
         (inventory (luvcraft:luvcraft-session-inventory session))
         (entries (inventory-visible-entries frame))
         (all-entries (luvcraft:block-inventory-entries inventory))
         (selected (luvcraft:luvcraft-session-selected-block session))
         (selected-entry (luvcraft:block-inventory-entry-for inventory selected))
         (cell-width (/ (- +inventory-grid-right+ +inventory-grid-left+)
                        +inventory-grid-columns+))
         (cell-height (/ (- +inventory-grid-bottom+ +inventory-grid-top+) 2)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'luv-raster-medium)
          (clear-raster-medium-reliefs medium))
        (draw-analytic-rounded-rectangle*
         medium left top right bottom :radius 7
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.25 0.25 0.23)
               (make-rgb-color 0.12 0.12 0.11)))
        (draw-rectangle* pane 3 3 (- right 3) (- bottom 3)
                         :filled nil :line-thickness 2
                         :ink *inventory-light-edge*)
        (draw-text* pane "Categories" 16 30
                    :align-y :center :text-size 13
                    :ink *inventory-text-ink*)
        (draw-text* pane "Inventory" 408 27
                    :align-x :center :align-y :center :text-size 17
                    :ink *inventory-text-ink*)
        (loop for (category label) in *inventory-categories*
              for index from 0
              do (draw-inventory-category frame pane category label index))
        (draw-inventory-bevel pane +inventory-grid-left+ +inventory-grid-top+
                              +inventory-grid-right+ +inventory-grid-bottom+
                              :ink *inventory-panel-ink* :recessed-p t)
        (loop for entry in entries
              for index from 0
              for column = (mod index +inventory-grid-columns+)
              for row = (floor index +inventory-grid-columns+)
              for number = (position entry all-entries :test #'eq)
              do (draw-inventory-entry
                  frame pane entry number
                  (+ +inventory-grid-left+ (* column cell-width) 4)
                  (+ +inventory-grid-top+ (* row cell-height) 4)
                  (- cell-width 8) (- cell-height 8)))
        (draw-inventory-quickbar frame pane all-entries)
        (when selected-entry
          (draw-inventory-details frame pane selected selected-entry))
        (draw-inventory-bevel pane 8 366 832 404
                              :ink *inventory-panel-ink* :recessed-p t)
        (draw-text* pane "Mode: Creative" 18 385 :align-y :center
                    :text-size 11 :ink *inventory-text-ink*)
        (draw-text* pane (format nil "Slots: ~D" (length all-entries))
                    300 385 :align-y :center :text-size 11
                    :ink *inventory-text-ink*)
        (draw-text* pane
                    (format nil "Selected: ~(~A~)"
                            (luvcraft:block-kind-name selected))
                    520 385 :align-y :center :text-size 11
                    :ink *inventory-text-ink*)
        (draw-inventory-bevel pane 8 410 832 458
                              :ink *inventory-well-ink* :recessed-p t)
        (draw-text* pane "Presentation>" 18 434 :align-y :center
                    :text-size 11 :ink *inventory-accent-ink*)
        (draw-text* pane
                    (format nil "#<BLOCK-INVENTORY-ENTRY ~(~A~)>"
                            (luvcraft:block-kind-name selected))
                    150 434 :align-y :center :text-size 11
                    :ink *inventory-text-ink*)
        (draw-text* pane "Click: select     Category: filter     I / Esc: close"
                    18 480 :align-y :center :text-size 11
                    :ink *inventory-text-ink*)))))

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

(defmethod luvcraft:encode-luvcraft-overlay :around
    ((hotbar luvcraft-hotbar-overlay) session pass surface-texture)
  "Hide the redundant HUD bar while the full inventory owns modal focus."
  (if (typep (luvcraft:luvcraft-session-modal-focus session)
             'luvcraft-inventory-overlay)
      hotbar
      (call-next-method)))

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

(defun inventory-category-at (u v)
  (let ((x (* u +inventory-view-width+))
        (y (* v +inventory-view-height+)))
    (when (and (<= 16 x) (< x 150) (<= 50 y) (< y 206))
      (let ((index (floor (- y 50) 39)))
        (first (nth index *inventory-categories*))))))

(defun inventory-slot-at (u v entry-count)
  "Return the zero-based visible inventory slot at texture U,V."
  (let ((x (* u +inventory-view-width+))
        (y (* v +inventory-view-height+)))
    (when (and (<= +inventory-grid-left+ x)
               (< x +inventory-grid-right+)
               (<= +inventory-grid-top+ y)
               (< y +inventory-grid-bottom+))
      (let* ((column
               (floor (* (- x +inventory-grid-left+)
                         +inventory-grid-columns+)
                      (- +inventory-grid-right+ +inventory-grid-left+)))
             (row
               (floor (* (- y +inventory-grid-top+) 2)
                      (- +inventory-grid-bottom+ +inventory-grid-top+)))
             (slot (+ column (* row +inventory-grid-columns+))))
        (and (< slot entry-count) slot)))))

(defun inventory-quickbar-slot-at (u v entry-count)
  (let ((x (* u +inventory-view-width+))
        (y (* v +inventory-view-height+)))
    (when (and (plusp entry-count)
               (<= +inventory-grid-left+ x)
               (< x +inventory-grid-right+)
               (<= +inventory-quickbar-top+ y)
               (< y +inventory-quickbar-bottom+))
      (min (1- entry-count)
           (floor (* (- x +inventory-grid-left+) entry-count)
                  (- +inventory-grid-right+ +inventory-grid-left+))))))

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
      (let* ((frame (widget-overlay-frame overlay))
             (inventory (luvcraft:luvcraft-session-inventory session))
             (all-entries (luvcraft:block-inventory-entries inventory))
             (category (inventory-category-at (first uv) (second uv))))
        (cond
          (category
           (setf (inventory-category frame) category)
           (repaint-inventory frame))
          (t
           (let* ((visible (inventory-visible-entries frame))
                  (slot (or (inventory-slot-at
                             (first uv) (second uv) (length visible))
                            (inventory-quickbar-slot-at
                             (first uv) (second uv) (length all-entries))))
                  (entries
                    (if (inventory-slot-at
                         (first uv) (second uv) (length visible))
                        visible all-entries))
                  (entry (and slot (nth slot entries))))
             (when entry
               (let ((number
                       (position entry all-entries :test #'eq)))
                 (luvcraft:select-luvcraft-block session (1+ number))
                 (repaint-inventory frame))))))))
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
