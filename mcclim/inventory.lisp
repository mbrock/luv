;;; A modal McCLIM inventory presented over the live luvcraft canvas.

(in-package #:mcluv)

(defconstant +inventory-view-width+ 840)
(defconstant +inventory-view-height+ 500)
(defconstant +inventory-sidebar-left+ 8)
(defconstant +inventory-sidebar-right+ 158)
(defconstant +inventory-grid-left+ 166)
(defconstant +inventory-grid-top+ 50)
(defconstant +inventory-grid-right+ 650)
(defconstant +inventory-grid-bottom+ 262)
(defconstant +inventory-grid-columns+ 8)
(defconstant +inventory-grid-rows+ 3)
(defconstant +inventory-page-size+
  (* +inventory-grid-columns+ +inventory-grid-rows+))
(defconstant +inventory-details-left+ 658)
(defconstant +inventory-details-right+ 832)
(defconstant +inventory-quickbar-top+ 290)
(defconstant +inventory-quickbar-bottom+ 350)
(defconstant +inventory-category-top+ 50)
(defconstant +inventory-category-pitch+ 34)

(defparameter *inventory-categories*
  '((:all "All blocks")
    (:natural "Natural")
    (:building "Building")
    (:masonry "Masonry")
    (:woodwork "Woodwork")
    (:roofing "Roofing")
    (:decor "Decor")
    (:luminous "Luminous")
    (:colors "Colours")))

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
   (page :initform 0 :accessor inventory-page)
   ;; The block under the pointer: what the inspector shows and what a
   ;; number key puts on the bar.  NIL when the pointer is not over a cell.
   (hovered :initform nil :accessor inventory-hovered-block)
   (simple-focus :initform :blocks :accessor inventory-simple-focus)
   (simple-cursor :initform 0 :accessor inventory-simple-cursor)
   (pending-block :initform nil :accessor inventory-pending-block)
   (added-block :initform nil :accessor inventory-added-block)
   (added-slot :initform nil :accessor inventory-added-slot)
   (added-time :initform nil :accessor inventory-added-time)
   (preferences :initform nil :accessor inventory-preferences)
   (preferences-writer :initform nil :accessor inventory-preferences-writer)
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
  "Paint BLOCK as an isometric cube, sized as SCALE sixteenths used to be.

The flat top tile said what a material looked like; the cube says what it
is.  ATLAS is no longer read here -- BLOCK-ICON-PATTERN keeps its own -- but
it stays in the lambda list because the inventory frame owns one and the
callers all have it to hand."
  (declare (ignore atlas))
  (draw-block-icon stream block left top (* scale 16)))

(defun inventory-entry-label (entry)
  (string-upcase
   (symbol-name
    (luvcraft:block-kind-name
     (luvcraft:block-inventory-entry-block entry)))))

(defun inventory-entry-quantity-label (entry)
  "How many there are, as a count or as the mark for no limit.

A creative inventory is mostly unlimited stacks, and spelling that out in
every cell makes the word louder than the block it is about."
  (alexandria:if-let
      ((quantity (luvcraft:block-inventory-entry-quantity entry)))
    (format nil "~D" quantity)
    "∞"))

(defun inventory-category-block-p (category block)
  (or (eq category :all)
      (member category (luvcraft:block-kind-categories block))))

(defun inventory-filtered-entries (frame)
  (remove-if-not
   (lambda (entry)
     (inventory-category-block-p
      (inventory-category frame)
      (luvcraft:block-inventory-entry-block entry)))
   (luvcraft:block-inventory-entries
    (luvcraft:luvcraft-session-inventory (inventory-session frame)))))

(defun inventory-page-count (frame)
  (max 1 (ceiling (length (inventory-filtered-entries frame))
                  +inventory-page-size+)))

(defun inventory-visible-entries (frame)
  (let* ((entries (inventory-filtered-entries frame))
         (page (min (inventory-page frame)
                    (1- (inventory-page-count frame))))
         (start (* page +inventory-page-size+)))
    (subseq entries start (min (length entries)
                               (+ start +inventory-page-size+)))))

(defun inventory-visible-state-for (frame)
  (let ((session (inventory-session frame)))
    (list
     (inventory-simple-state frame)
     (inventory-category frame)
     (inventory-page frame)
     (inventory-hovered-block frame)
     (luvcraft:luvcraft-session-selected-block session)
     (loop for entry in
             (luvcraft:block-inventory-entries
              (luvcraft:luvcraft-session-inventory session))
           collect (list entry
                         (luvcraft:block-inventory-entry-quantity entry))))))

(defun draw-inventory-category (frame pane category label index)
  (let* ((top (+ +inventory-category-top+
                (* index +inventory-category-pitch+)))
         (selected-p (eq category (inventory-category frame))))
    (progn
      (draw-inventory-bevel
       pane 16 top 150 (+ top (- +inventory-category-pitch+ 4))
       :ink (if selected-p
                (make-rgb-color 0.36 0.35 0.30)
                *inventory-panel-ink*)
       :recessed-p selected-p)
      (draw-rectangle* pane 25 (+ top 8) 39 (+ top 22)
                       :ink (ecase category
                              (:all (make-rgb-color 0.72 0.56 0.24))
                              (:natural (make-rgb-color 0.20 0.50 0.18))
                              (:building (make-rgb-color 0.49 0.47 0.42))
                              (:masonry (make-rgb-color 0.82 0.80 0.74))
                              (:woodwork (make-rgb-color 0.60 0.38 0.18))
                              (:roofing (make-rgb-color 0.66 0.27 0.18))
                              (:decor (make-rgb-color 0.93 0.74 0.27))
                              (:luminous (make-rgb-color 0.18 0.72 0.74))
                              (:colors (make-rgb-color 0.86 0.30 0.52))))
      (draw-text* pane label 49 (+ top 15)
                  :align-y :center :text-size 14
                  :ink (if selected-p +white+ *inventory-text-ink*)))))

(defun draw-inventory-page-controls (frame pane)
  (let ((page (inventory-page frame))
        (count (inventory-page-count frame)))
    (draw-inventory-bevel pane 166 12 200 43
                          :ink *inventory-panel-ink* :recessed-p t)
    (draw-inventory-bevel pane 616 12 650 43
                          :ink *inventory-panel-ink* :recessed-p t)
    (draw-text* pane "‹" 183 27 :align-x :center :align-y :center
                :text-size 20
                :ink (if (plusp page) +white+ *inventory-muted-ink*))
    (draw-text* pane "›" 633 27 :align-x :center :align-y :center
                :text-size 20
                :ink (if (< page (1- count)) +white+ *inventory-muted-ink*))
    (draw-text* pane (format nil "Inventory  ·  ~D / ~D" (1+ page) count)
                408 27 :align-x :center :align-y :center :text-size 17
                :ink *inventory-text-ink*)))

(defun draw-inventory-entry
    (frame pane entry index left top width height)
  "Draw ENTRY's cell.  INDEX is its place in the whole inventory, so a block
on the bar wears the number key that selects it."
  (let* ((block (luvcraft:block-inventory-entry-block entry))
         (selected-p
           (eq block
               (luvcraft:luvcraft-session-selected-block
                (inventory-session frame))))
         (hovered-p (eq block (inventory-hovered-block frame))))
    (progn
      (draw-inventory-bevel pane left top (+ left width) (+ top height)
                            :ink (cond (selected-p
                                        (make-rgb-color 0.39 0.39 0.34))
                                       (hovered-p
                                        (make-rgb-color 0.36 0.35 0.31))
                                       (t *inventory-face-ink*))
                            :recessed-p (not hovered-p))
      (when selected-p
        (draw-rectangle* pane (+ left 3) (+ top 3)
                         (- (+ left width) 3) (- (+ top height) 3)
                         :ink *inventory-accent-ink*
                         :filled nil :line-thickness 2))
      (draw-inventory-block-tile
       pane (inventory-atlas frame) block
       (+ left (/ (- width 40) 2.0)) (+ top 9) :scale 5/2)
      (when (< index luvcraft::+block-inventory-quickbar-size+)
        (draw-text* pane (format nil "~D" (mod (1+ index) 10))
                    (+ left 6) (+ top 5) :text-size 10
                    :ink *inventory-accent-ink*))
      (draw-text* pane (inventory-entry-quantity-label entry)
                  (- (+ left width) 8) (- (+ top height) 7)
                  :align-x :right :align-y :bottom :text-size 12
                  :ink *inventory-muted-ink*))))

(defun draw-inventory-quickbar (frame pane entries)
  (draw-text* pane "Quickbar" +inventory-grid-left+
              (- +inventory-quickbar-top+ 14)
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
             (draw-text* pane (format nil "~D" (mod (1+ index) 10))
                         (+ left 5) (+ +inventory-quickbar-top+ 5)
                         :text-size 9 :ink +white+))))

(defgeneric inventory-detail-rows (block entry)
  (:documentation
   "The (LABEL . VALUE) rows the inspector prints for BLOCK held as ENTRY.")
  (:method ((block luvcraft:block-kind) entry)
    (list (cons "Type" "Block")
          (cons "Light opacity" (luvcraft:block-light-opacity block))
          (cons "Light emission" (luvcraft:block-light-emission block))
          (cons "Surface emission"
                (format nil "~,2F" (luvcraft:block-surface-emission block)))
          (cons "Stack" (inventory-entry-quantity-label entry)))))

(defun inventory-clipped-label (text limit)
  (if (and text (> (length text) limit))
      (concatenate 'string (subseq text 0 (1- limit)) "…")
      (or text "—")))

(defmethod inventory-detail-rows ((film luvcraft:film-block-kind) entry)
  "A film is its video: what it is called, who put it up, how long it runs."
  (let ((duration (luvcraft:film-duration film)))
    (list (cons "Type" "Film")
          (cons "Title" (inventory-clipped-label (luvcraft:film-title film) 17))
          (cons "By" (inventory-clipped-label (luvcraft:film-uploader film) 22))
          (cons "Runs"
                (if duration
                    (format nil "~D:~2,'0D" (floor duration 60) (mod duration 60))
                    "—"))
          (cons "Stack" (inventory-entry-quantity-label entry)))))

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
  (loop for (label . value) in (inventory-detail-rows selected entry)
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
         (quickbar-entries (luvcraft:block-inventory-quickbar-entries inventory))
         (selected (luvcraft:luvcraft-session-selected-block session))
         (inspected (or (inventory-hovered-block frame) selected))
         (inspected-entry
           (luvcraft:block-inventory-entry-for inventory inspected))
         (cell-width (/ (- +inventory-grid-right+ +inventory-grid-left+)
                        +inventory-grid-columns+))
         (cell-height (/ (- +inventory-grid-bottom+ +inventory-grid-top+)
                         +inventory-grid-rows+)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
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
        (draw-inventory-page-controls frame pane)
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
                  (+ +inventory-grid-left+ (* column cell-width) 3)
                  (+ +inventory-grid-top+ (* row cell-height) 3)
                  (- cell-width 6) (- cell-height 6)))
        (draw-inventory-quickbar frame pane quickbar-entries)
        (when inspected-entry
          (draw-inventory-details frame pane inspected inspected-entry))
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
        (draw-text* pane "Click: take in hand     1–9, 0 over a block: put it on that key     Wheel / ‹ ›: page     I / Esc: close"
                    18 480 :align-y :center :text-size 11
                    :ink *inventory-text-ink*)))))

(defun repaint-inventory (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (check-type mirror luv-gpu-mirror)
    (repaint-gpu-mirror mirror))
  (setf (inventory-visible-state frame) (inventory-visible-state-for frame))
  frame)

(defclass luvcraft-inventory-overlay (luvcraft-hud-widget-overlay)
  ((visible-p :initform t :accessor inventory-overlay-visible-p)))

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
  (let* ((source-size (widget-overlay-logical-size overlay))
         (viewport-size
           (multiple-value-list
            (luv:canvas-logical-size
             (luvcraft:luvcraft-session-canvas
              (widget-overlay-session overlay)))))
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

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay :zone :inventory/encode)
    ((overlay luvcraft-inventory-overlay) session pass surface-texture)
  (declare (ignore pass))
  (when (inventory-overlay-visible-p overlay)
    (prepare-direct-widget-overlay
     overlay session surface-texture (inventory-screen-state overlay)))
  overlay)

(luv:zdefmethod (luvcraft:refresh-luvcraft-overlay :zone :inventory/refresh)
    ((overlay luvcraft-inventory-overlay) session)
  (declare (ignore session))
  (when (inventory-overlay-visible-p overlay)
    (let ((frame (widget-overlay-frame overlay)))
      (unless (equal (inventory-visible-state-for frame)
                     (inventory-visible-state frame))
        (repaint-inventory frame))))
  overlay)

(defun inventory-category-at (u v)
  (let ((x (* u +inventory-view-width+))
        (y (* v +inventory-view-height+)))
    (when (and (<= 16 x) (< x 150) (<= +inventory-category-top+ y)
               (< y (+ +inventory-category-top+
                       (* +inventory-category-pitch+
                          (length *inventory-categories*)))))
      (let ((index (floor (- y +inventory-category-top+)
                          +inventory-category-pitch+)))
        (first (nth index *inventory-categories*))))))

(defun inventory-page-direction-at (u v)
  (let ((x (* u +inventory-view-width+))
        (y (* v +inventory-view-height+)))
    (when (and (<= 12 y) (< y 43))
      (cond ((and (<= 166 x) (< x 200)) -1)
            ((and (<= 616 x) (< x 650)) 1)))))

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
               (floor (* (- y +inventory-grid-top+) +inventory-grid-rows+)
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

(defun inventory-block-at (frame u v)
  "The block in the grid cell or quickbar slot at texture U,V, or NIL."
  (let* ((inventory (luvcraft:luvcraft-session-inventory
                     (inventory-session frame)))
         (visible (inventory-visible-entries frame))
         (quickbar (luvcraft:block-inventory-quickbar-entries inventory))
         (entry
           (alexandria:if-let ((slot (inventory-slot-at u v (length visible))))
             (nth slot visible)
             (alexandria:when-let
                 ((slot (inventory-quickbar-slot-at u v (length quickbar))))
               (nth slot quickbar)))))
    (and entry (luvcraft:block-inventory-entry-block entry))))

(defun turn-inventory-page (frame direction)
  (setf (inventory-page frame)
        (max 0 (min (1- (inventory-page-count frame))
                    (+ (inventory-page frame) direction))))
  (repaint-inventory frame))

(defun hover-inventory-block (frame block)
  "Note BLOCK as under the pointer, repainting only when that changes."
  (unless (eq block (inventory-hovered-block frame))
    (setf (inventory-hovered-block frame) block)
    (repaint-inventory frame)))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-inventory-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore canvas))
  (when (inventory-overlay-visible-p overlay)
    (let ((frame (widget-overlay-frame overlay))
          (uv (luvcraft-widget-texture-coordinate
               overlay
               (luv:canvas-pointer-event-x event)
               (luv:canvas-pointer-event-y event))))
      (if (null uv)
          ;; Off the panel: nothing is under the pointer any more, and the
          ;; event belongs to whatever is.
          (progn (hover-inventory-block frame nil) nil)
          (destructuring-bind (u v) uv
            (typecase event
              (luv:canvas-pointer-motion-event
               (hover-inventory-block frame (inventory-block-at frame u v)))
              (luv:canvas-pointer-wheel-event
               (let ((notches (luv:canvas-pointer-event-scroll-y event)))
                 (unless (zerop notches)
                   ;; Wheel down is a negative notch and the next page.
                   (turn-inventory-page frame (if (minusp notches) 1 -1)))))
              (luv:canvas-pointer-button-press-event
               (when (eq :left (luv:canvas-pointer-event-button event))
                 (let ((category (inventory-category-at u v))
                       (page-direction (inventory-page-direction-at u v))
                       (block (inventory-block-at frame u v)))
                   (cond
                     (category
                      (setf (inventory-category frame) category
                            (inventory-page frame) 0)
                      (repaint-inventory frame))
                     (page-direction
                      (turn-inventory-page frame page-direction))
                     (block
                      (luvcraft:put-luvcraft-block-on-quickbar session block)
                      (repaint-inventory frame)))))))
            t)))))

(defun inventory-key-slot (event)
  "The one-based quickbar slot a number key names: 1-9, and 0 as ten."
  (let ((character (or (luv:canvas-key-event-unshifted-character event)
                       (luv:canvas-key-event-character event))))
    (alexandria:when-let
        ((digit (and (characterp character) (digit-char-p character))))
      (if (zerop digit) 10 digit))))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-inventory-overlay) session canvas
     (event luv:canvas-key-press-event))
  ;; Escape leaves, and so does another I: the key which opened the view is
  ;; the key which puts it away, since while it is open it owns every key.
  ;; A number key puts the block under the pointer on that key's slot, or,
  ;; with nothing hovered, selects the slot as it would in the world.
  (let ((frame (widget-overlay-frame overlay))
        (slot (inventory-key-slot event)))
    (cond
      ((or (eq :escape (luv:canvas-key-event-key-name event))
           (and (eq :i (luv:canvas-key-event-key-name event))
                (not (luv:canvas-key-event-repeat-p event))))
       (luvcraft:unfocus-luvcraft-session session)
       t)
      (slot
       (alexandria:if-let ((block (inventory-hovered-block frame)))
         (luvcraft:assign-luvcraft-quickbar-slot session block slot)
         (when (<= slot (length (luvcraft:block-inventory-quickbar-entries
                                 (luvcraft:luvcraft-session-inventory
                                  session))))
           (luvcraft:select-luvcraft-block session slot)))
       (repaint-inventory frame)
       t)
      ((member (luv:canvas-key-event-key-name event) '(:page-down :right))
       (turn-inventory-page frame 1)
       t)
      ((member (luv:canvas-key-event-key-name event) '(:page-up :left))
       (turn-inventory-page frame -1)
       t)
      (t (call-next-method)))))

(defmethod luvcraft:luvcraft-focus-left
    ((overlay luvcraft-inventory-overlay) session)
  (declare (ignore session))
  (setf (inventory-overlay-visible-p overlay) nil)
  overlay)

(defun open-luvcraft-inventory (session &key (title "luvcraft inventory"))
  "Create, attach, and focus SESSION's modal McCLIM inventory view."
  (alexandria:if-let
      ((overlay
         (find-if (lambda (candidate)
                    (typep candidate 'luvcraft-inventory-overlay))
                  (luvcraft:luvcraft-session-overlays session))))
    (progn
      (setf (inventory-overlay-visible-p overlay) t)
      (luvcraft:focus-luvcraft-session session overlay)
      overlay)
    (let* ((port (find-port :server-path '(:luv-gpu)))
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
      (initialize-inventory-preferences frame)
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
        overlay))))

(defun close-luvcraft-inventory (overlay)
  "Close an OPEN-LUVCRAFT-INVENTORY overlay."
  (check-type overlay luvcraft-inventory-overlay)
  (let ((session (widget-overlay-session overlay)))
    (setf (inventory-overlay-visible-p overlay) nil)
    (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
      (luvcraft:unfocus-luvcraft-session session)))
  nil)

(defmethod luvcraft:toggle-luvcraft-inventory
    ((session luvcraft:luvcraft-session))
  (alexandria:if-let
      ((overlay
         (find-if (lambda (candidate)
                    (typep candidate 'luvcraft-inventory-overlay))
                  (luvcraft:luvcraft-session-overlays session))))
    (if (inventory-overlay-visible-p overlay)
        (progn (close-luvcraft-inventory overlay) t)
        (progn (open-luvcraft-inventory session) t))
    (progn (open-luvcraft-inventory session) t)))

(defmethod luvcraft:attach-luvcraft-hud :around
    ((session luvcraft:luvcraft-session))
  ;; Build the durable inventory pane before the game window is published.
  ;; Opening it later is then only focus/visibility state, with no shader or
  ;; glyph-atlas compilation on the frame thread.  This is an AROUND method so
  ;; application adapters can independently own :AFTER attachment hooks; CLOS
  ;; replaces, rather than accumulates, identical qualified coordinates.
  (multiple-value-prog1 (call-next-method)
    (open-luvcraft-inventory session)
    (luvcraft:unfocus-luvcraft-session session)))
