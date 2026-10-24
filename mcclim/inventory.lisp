;;; A modal McCLIM inventory presented over the live luvcraft canvas.

(in-package #:mcluv)

(defconstant +inventory-view-width+ 840)
(defparameter *inventory-view-height* 440)
(defconstant +inventory-sidebar-left+ 8)
(defparameter *inventory-sidebar-right* 70)
(defparameter *inventory-grid-left* 80)
(defparameter *inventory-grid-top* 64)
(defparameter *inventory-grid-right* 650)
(defparameter *inventory-grid-bottom* 350)
(defconstant +inventory-grid-columns+ 8)
(defconstant +inventory-grid-rows+ 5)
(defconstant +inventory-page-size+
  (* +inventory-grid-columns+ +inventory-grid-rows+))
(defconstant +inventory-details-left+ 658)
(defconstant +inventory-details-right+ 832)
(defparameter *inventory-quickbar-left* 16)
(defconstant +inventory-quickbar-right+ 824)
(defparameter *inventory-quickbar-top* 368)
(defparameter *inventory-quickbar-bottom* 428)
(defconstant +inventory-category-top+ 64)
(defconstant +inventory-category-pitch+ 31)

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

(defclass inventory-pane (transparent-gpu-application-pane) ())

(define-application-frame luvcraft-inventory ()
  ((session :initarg :session :reader inventory-session)
   (visible-state :initform nil :accessor inventory-visible-state)
   (category :initform :all :accessor inventory-category)
   (page :initform 0 :accessor inventory-page)
   ;; The block under the pointer: what the inspector shows and what a
   ;; number key puts on the bar.  NIL when the pointer is not over a cell.
   (hovered-category :initform nil :accessor inventory-hovered-category)
   (hovered :initform nil :accessor inventory-hovered-block)
   (preferences :initform nil :accessor inventory-preferences)
   (preferences-writer :initform nil :accessor inventory-preferences-writer)
   (atlas :initform (luvcraft:make-block-texture-atlas)
          :reader inventory-atlas))
  (:menu-bar nil)
  (:panes
   (inventory
    (make-pane 'inventory-pane
               :default-text-style (make-text-style :sans-serif nil :normal)
               :width +inventory-view-width+ :height *inventory-view-height*)))
  (:layouts (default inventory)))

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
  (string-capitalize
   (substitute #\Space #\-
               (symbol-name
                (luvcraft:block-kind-name
                 (luvcraft:block-inventory-entry-block entry))))))

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
    (luvcraft:player-inventory (inventory-session frame)))))

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
     *ui-theme*
     (inventory-simple-state frame)
     (inventory-category frame)
     (inventory-hovered-category frame)
     (inventory-page frame)
     (inventory-hovered-block frame)
     (luvcraft:player-selected-block session)
     (loop for entry in
             (luvcraft:block-inventory-entries
              (luvcraft:player-inventory session))
           collect (list entry
                         (luvcraft:block-inventory-entry-quantity entry))))))

(defparameter *inventory-category-icons*
  '((:all . :bricks) (:natural . :grass) (:building . :planks)
    (:masonry . :stone) (:woodwork . :wood) (:roofing . :thatch)
    (:decor . :bookshelf) (:luminous . :lantern) (:colors . :red-wool)))

(defun draw-inventory-category (frame pane category label index)
  (declare (ignore label))
  (let* ((top (+ +inventory-category-top+ (* index +inventory-category-pitch+)))
         (selected-p (eq category (inventory-category frame)))
         (block (luvcraft:block-kind-named
                 (cdr (assoc category *inventory-category-icons*)) nil)))
    (with-sheet-medium (medium pane)
      (when (or selected-p (eq category (inventory-hovered-category frame)))
        (draw-ui-surface medium 22 top 62 (+ top 29)
                         :fill (if selected-p :selection-soft :accent)
                         :edge (if selected-p :selection :accent) :radius 5)))
    (when block (draw-block-icon pane block 29 (+ top 1) 27))))

(defun draw-inventory-page-controls (frame pane)
  (let ((page (inventory-page frame)) (count (inventory-page-count frame)))
    (with-sheet-medium (medium pane)
      (dolist (left '(706 784))
        (draw-ui-surface medium left 17 (+ left 28) 43 :fill :inset :radius 5)))
    (draw-text* pane "‹" 720 30 :align-x :center :align-y :center
                :text-family :fix :text-size 20 :ink (ui-ink :text))
    (draw-text* pane "›" 798 30 :align-x :center :align-y :center
                :text-family :fix :text-size 20 :ink (ui-ink :text))
    (draw-text* pane (format nil "~D / ~D" (1+ page) count) 759 30
                :align-x :center :align-y :center :text-size (ui-text-size :body)
                :ink (ui-ink :text))))

(defun draw-inventory-entry
    (frame pane entry index left top width height)
  "Draw ENTRY's cell.  INDEX is its place in the whole inventory, so a block
on the bar wears the number key that selects it."
  (let* ((block (luvcraft:block-inventory-entry-block entry))
         (selected-p
           (eq block
               (luvcraft:player-selected-block
                (inventory-session frame))))
         (hovered-p (eq block (inventory-hovered-block frame))))
    (with-sheet-medium (medium pane)
      (draw-hotbar-slot pane medium left top (+ left width) (+ top height)
                        :selected-p selected-p :hovered-p hovered-p)
      (draw-inventory-block-tile
       pane (inventory-atlas frame) block
       (+ left (/ (- width 40) 2.0))
       (+ top (/ (- height 40) 2.0)) :scale 5/2)
      (when (< index luvcraft::+block-inventory-quickbar-size+)
        (draw-text* pane (format nil "~D" (mod (1+ index) 10))
                    (+ left 6) (+ top 10) :align-y :center :text-size (ui-text-size :caption)
                    :ink (ui-ink :selection)))
      (when (plusp (or (luvcraft:block-inventory-entry-quantity entry) 0))
        (draw-text* pane (inventory-entry-quantity-label entry)
                    (- (+ left width) 6) (- (+ top height) 5)
                    :align-x :right :align-y :bottom :text-size (ui-text-size :caption)
                    :ink (ui-ink :muted))))))

(defun draw-inventory-quickbar (frame pane entries)
  (let ((slot-width
          (/ (- +inventory-quickbar-right+ *inventory-quickbar-left*)
             (max 1 (length entries)))))
    (loop for entry in entries
          for index from 0
          for block = (luvcraft:block-inventory-entry-block entry)
          for left = (+ *inventory-quickbar-left* (* index slot-width))
          for selected-p =
            (eq block
                (luvcraft:player-selected-block
                 (inventory-session frame)))
          do (with-sheet-medium (medium pane)
               (draw-hotbar-slot
                pane medium (+ left 3) *inventory-quickbar-top*
                (- (+ left slot-width) 3) *inventory-quickbar-bottom*
                :selected-p selected-p))
             (draw-inventory-block-tile
              pane (inventory-atlas frame) block
              (+ left (/ (- slot-width 40) 2))
              (+ *inventory-quickbar-top* 10) :scale 5/2)
             (with-sheet-medium (medium pane)
               (draw-ui-keycap medium (format nil "~D" (mod (1+ index) 10))
                                (+ left 6) (+ *inventory-quickbar-top* 3)
                                :width 16 :height 16)))))

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
  "A material portrait and one useful action, without renderer properties."
  (declare (ignore frame))
  (draw-block-icon pane selected 695 85 94)
  (draw-text* pane (inventory-clipped-label (inventory-entry-label entry) 19)
              744 210 :align-x :center :align-y :center
              :text-face :bold :text-size (ui-text-size :body) :ink (ui-ink :text))
  (draw-line* pane 680 232 808 232 :ink (ui-ink :border))
  (draw-text* pane "Click to equip" 744 258 :align-x :center :align-y :center
              :text-size (ui-text-size :caption) :ink (ui-ink :text))
  (draw-text* pane "Hover + 1–9, 0" 744 284 :align-x :center :align-y :center
              :text-size (ui-text-size :caption) :ink (ui-ink :muted))
  (draw-text* pane "to choose a slot" 744 304 :align-x :center :align-y :center
              :text-size (ui-text-size :caption) :ink (ui-ink :muted)))

(defmethod handle-repaint ((pane inventory-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (session (inventory-session frame))
         (inventory (luvcraft:player-inventory session))
         (entries (inventory-visible-entries frame))
         (inspected (or (inventory-hovered-block frame)
                        (luvcraft:player-selected-block session)))
         (inspected-entry (luvcraft:block-inventory-entry-for inventory inspected))
         (cell-width (/ (- *inventory-grid-right* *inventory-grid-left*)
                        +inventory-grid-columns+))
         (cell-height (/ (- *inventory-grid-bottom* *inventory-grid-top*)
                         +inventory-grid-rows+)))
    (with-sheet-medium (medium pane)
      (draw-ui-surface medium 0 0 +inventory-view-width+ *inventory-view-height*
                       :edge :forest :rim 3)
      (draw-text* pane "Materials" 28 30 :align-y :center :text-face :bold
                  :text-size (ui-text-size :heading) :ink (ui-ink :text))
      (let ((category (or (inventory-hovered-category frame) (inventory-category frame))))
        (draw-text* pane (second (assoc category *inventory-categories*)) 176 30
                    :align-y :center :text-size (ui-text-size :body) :ink (ui-ink :muted)))
      (when (inventory-preferences frame)
        (draw-text* pane (format nil "Player ~D · F2" (1+ (inventory-active-player frame)))
                    680 30 :align-x :right :align-y :center
                    :text-size (ui-text-size :caption) :ink (ui-ink :muted)))
      (draw-ui-surface medium 16 58 68 350 :fill :forest :edge :forest :radius 10)
      (draw-ui-surface medium 76 58 654 350 :fill :raised :radius 8)
      (draw-ui-surface medium 662 58 824 350 :fill :raised :radius 8)
      (draw-ui-surface medium 12 362 828 432 :fill :inset :radius 10)
      (draw-inventory-page-controls frame pane)
      (loop for (category label) in *inventory-categories* for index from 0
            do (draw-inventory-category frame pane category label index))
      (loop for entry in entries for index from 0
            for column = (mod index +inventory-grid-columns+)
            for row = (floor index +inventory-grid-columns+)
            do (draw-inventory-entry frame pane entry 10
                                     (+ *inventory-grid-left* (* column cell-width) 3)
                                     (+ *inventory-grid-top* (* row cell-height) 3)
                                     (- cell-width 6) (- cell-height 6)))
      (draw-inventory-quickbar frame pane (luvcraft:block-inventory-quickbar-entries inventory))
      (when inspected-entry (draw-inventory-details frame pane inspected inspected-entry)))))

(defun repaint-inventory (frame)
  (make-gpu-frame-background-transparent frame)
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
  (if (typep (instrument-focus session)
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
             (instrument-canvas
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
        (y (* v *inventory-view-height*)))
    (when (and (<= 16 x) (< x 68) (<= +inventory-category-top+ y)
               (< y (+ +inventory-category-top+
                       (* +inventory-category-pitch+
                          (length *inventory-categories*)))))
      (let ((index (floor (- y +inventory-category-top+)
                          +inventory-category-pitch+)))
        (first (nth index *inventory-categories*))))))

(defun inventory-page-direction-at (u v)
  (let ((x (* u +inventory-view-width+))
        (y (* v *inventory-view-height*)))
    (when (and (<= 17 y) (< y 43))
      (cond ((and (<= 706 x) (< x 734)) -1)
            ((and (<= 784 x) (< x 812)) 1)))))

(defun inventory-slot-at (u v entry-count)
  "Return the zero-based visible inventory slot at texture U,V."
  (let ((x (* u +inventory-view-width+))
        (y (* v *inventory-view-height*)))
    (when (and (<= *inventory-grid-left* x)
               (< x *inventory-grid-right*)
               (<= *inventory-grid-top* y)
               (< y *inventory-grid-bottom*))
      (let* ((column
               (floor (* (- x *inventory-grid-left*)
                         +inventory-grid-columns+)
                      (- *inventory-grid-right* *inventory-grid-left*)))
             (row
               (floor (* (- y *inventory-grid-top*) +inventory-grid-rows+)
                      (- *inventory-grid-bottom* *inventory-grid-top*)))
             (slot (+ column (* row +inventory-grid-columns+))))
        (and (< slot entry-count) slot)))))

(defun inventory-quickbar-slot-at (u v entry-count)
  (let ((x (* u +inventory-view-width+))
        (y (* v *inventory-view-height*)))
    (when (and (plusp entry-count)
               (<= *inventory-quickbar-left* x)
               (< x +inventory-quickbar-right+)
               (<= *inventory-quickbar-top* y)
               (< y *inventory-quickbar-bottom*))
      (min (1- entry-count)
           (floor (* (- x *inventory-quickbar-left*) entry-count)
                  (- +inventory-quickbar-right+ *inventory-quickbar-left*))))))

(defun inventory-block-at (frame u v)
  "The block in the grid cell or quickbar slot at texture U,V, or NIL."
  (let* ((inventory (luvcraft:player-inventory
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
  (setf (inventory-hovered-block frame) nil)
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
          (progn
            (setf (inventory-hovered-category frame) nil)
            (hover-inventory-block frame nil)
            nil)
          (destructuring-bind (u v) uv
            (typecase event
              (luv:canvas-pointer-motion-event
               (setf (inventory-hovered-category frame) (inventory-category-at u v))
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
                            (inventory-page frame) 0
                            (inventory-hovered-block frame) nil)
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

(defun move-inventory-cursor (frame delta)
  "Move keyboard focus through this page, keeping the preview in sync."
  (setf (inventory-hovered-category frame) nil)
  (let* ((entries (inventory-visible-entries frame))
         (current (position (inventory-hovered-block frame) entries
                            :key #'luvcraft:block-inventory-entry-block))
         (index (max 0 (min (1- (length entries)) (+ (or current 0) delta)))))
    (when entries
      (hover-inventory-block frame (luvcraft:block-inventory-entry-block (nth index entries))))))

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
       (unfocus-instrument session)
       t)
      (slot
       (alexandria:if-let ((block (inventory-hovered-block frame)))
         (luvcraft:assign-luvcraft-quickbar-slot session block slot)
         (when (<= slot (length (luvcraft:block-inventory-quickbar-entries
                                 (luvcraft:player-inventory
                                  session))))
           (luvcraft:select-luvcraft-block session slot)))
       (repaint-inventory frame)
       t)
      ((eq (luv:canvas-key-event-key-name event) :page-down)
       (turn-inventory-page frame 1)
       t)
      ((eq (luv:canvas-key-event-key-name event) :page-up)
       (turn-inventory-page frame -1)
       t)
      ((member (luv:canvas-key-event-key-name event) '(:left :right :up :down))
       (move-inventory-cursor frame
                              (case (luv:canvas-key-event-key-name event)
                                (:left -1) (:right 1)
                                (:up (- +inventory-grid-columns+))
                                (:down +inventory-grid-columns+)))
       t)
      ((member (luv:canvas-key-event-key-name event) '(:return :enter :space))
       (when (inventory-hovered-block frame)
         (luvcraft:put-luvcraft-block-on-quickbar session (inventory-hovered-block frame))
         (repaint-inventory frame))
       t)
      ((eq (luv:canvas-key-event-key-name event) :tab)
       (let* ((index (position (inventory-category frame) *inventory-categories* :key #'first))
              (next (mod (1+ (or index -1)) (length *inventory-categories*))))
         (setf (inventory-category frame) (first (nth next *inventory-categories*))
               (inventory-page frame) 0
               (inventory-hovered-block frame) nil
               (inventory-hovered-category frame) nil)
         (repaint-inventory frame))
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
                  (instrument-overlays session))))
    (progn
      (setf (inventory-overlay-visible-p overlay) t)
      (focus-instrument session overlay)
      overlay)
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
                'luvcraft-inventory :frame-manager manager :enable t
                :session session))))
      (make-gpu-frame-background-transparent frame)
      (initialize-inventory-preferences frame)
      (setf (frame-pretty-name frame) title
            (inventory-visible-state frame) (inventory-visible-state-for frame))
      (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
             (overlay
               (make-instance 'luvcraft-inventory-overlay
                              :session session :frame frame :mirror mirror)))
        (setf (mirror-compositor mirror) overlay)
        (add-instrument session overlay)
        (when (typep mirror 'luv-gpu-mirror)
          (repaint-gpu-mirror mirror))
        (focus-instrument session overlay)
        overlay))))

(defun close-luvcraft-inventory (overlay)
  "Close an OPEN-LUVCRAFT-INVENTORY overlay."
  (check-type overlay luvcraft-inventory-overlay)
  (let ((session (widget-overlay-session overlay)))
    (setf (inventory-overlay-visible-p overlay) nil)
    (when (eq overlay (instrument-focus session))
      (unfocus-instrument session)))
  nil)

(defun toggle-player-inventory (session)
  (alexandria:if-let
      ((overlay
         (find-if (lambda (candidate)
                    (typep candidate 'luvcraft-inventory-overlay))
                  (instrument-overlays session))))
    (if (inventory-overlay-visible-p overlay)
        (progn (close-luvcraft-inventory overlay) t)
        (progn (open-luvcraft-inventory session) t))
    (progn (open-luvcraft-inventory session) t)))

(defmethod luvcraft:toggle-luvcraft-inventory
    ((session luvcraft:luvcraft-session))
  (toggle-player-inventory session))

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
