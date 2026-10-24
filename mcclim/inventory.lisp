;;; A modal McCLIM inventory presented over the live luvcraft canvas.

(in-package #:mcluv)

(defconstant +inventory-view-width+ 840)
(defparameter *inventory-view-height* 364)
(defparameter *inventory-grid-left* 20)
(defparameter *inventory-grid-top* 64)
(defparameter *inventory-grid-right* 650)
(defparameter *inventory-grid-bottom* 350)
(defconstant +inventory-grid-columns+ 8)
(defconstant +inventory-grid-rows+ 5)
(defconstant +inventory-page-size+
  (* +inventory-grid-columns+ +inventory-grid-rows+))

(defclass inventory-pane (transparent-gpu-application-pane) ())

(define-application-frame luvcraft-inventory ()
  ((session :initarg :session :reader inventory-session)
   (visible-state :initform nil :accessor inventory-visible-state)
   (scroll :initform 0.0 :accessor inventory-scroll)
   ;; The block under the pointer: what the inspector shows and what a
   ;; number key puts on the bar.  NIL when the pointer is not over a cell.
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

(defun inventory-filtered-entries (frame)
  "One complete material catalog, independent of the speedbar's order."
  (sort (copy-list (luvcraft:block-inventory-entries
                   (luvcraft:player-inventory (inventory-session frame))))
        #'string< :key #'inventory-entry-label))

(defun inventory-row-height ()
  (/ (- *inventory-grid-bottom* *inventory-grid-top*) +inventory-grid-rows+))

(defun inventory-max-scroll (count)
  (max 0 (- (* (ceiling count +inventory-grid-columns+) (inventory-row-height))
            (- *inventory-grid-bottom* *inventory-grid-top*))))

(defun inventory-visible-entries (frame)
  (inventory-filtered-entries frame))

(defun scroll-inventory (frame amount)
  (setf (inventory-scroll frame)
        (max 0 (min (inventory-max-scroll (length (inventory-filtered-entries frame)))
                    (+ (inventory-scroll frame) amount)))
        (inventory-hovered-block frame) nil)
  (repaint-inventory frame))

(defun inventory-visible-state-for (frame)
  (let ((session (inventory-session frame)))
    (list
     *ui-theme*
     (inventory-simple-state frame)
     (inventory-scroll frame)
     (inventory-hovered-block frame)
     (luvcraft:player-selected-block session)
     (loop for entry in
             (luvcraft:block-inventory-entries
              (luvcraft:player-inventory session))
           collect (list entry
                         (luvcraft:block-inventory-entry-quantity entry))))))

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

(defgeneric inventory-material-description (block)
  (:documentation "Short, player-facing lines beneath a material's name."))

(defmethod inventory-material-description ((block luvcraft:block-kind))
  (or (cdr (assoc (luvcraft:block-kind-name block)
                  '((:marble "Pale stone with" "flowing veins.")
                    (:marble-tiles "Veined stone," "cut into tiles.")
                    (:lantern "A warm light in" "an iron frame.")
                    (:crystal "A bright crystal" "with a cool glow.")
                    (:lit-window "A window glowing" "with warm light.")
                    (:grass "Soft green grass" "over earthy soil.")
                    (:snow "A soft blanket" "of white snow.")
                    (:leaves "A little piece" "of the canopy.")
                    (:sand "Pale grains from" "a sunny shore.")
                    (:dirt "Earth for paths" "and landscapes."))))
      (let ((categories (luvcraft:block-kind-categories block)))
        (cond ((member :masonry categories) '("Made for walls" "and buildings."))
              ((member :woodwork categories) '("A wood material" "for building."))
              ((member :roofing categories) '("A finishing touch" "for rooftops."))
              ((member :colors categories) '("A splash of colour" "for your world."))
              ((member :natural categories) '("A piece of the" "natural world."))
              ((member :decor categories) '("A small detail" "to make it yours."))
              (t '("A material for" "your next idea."))))))

(defgeneric inventory-material-traits (block)
  (:documentation "Rows of (ICON LABEL VALUE), using actual block properties."))

(defmethod inventory-material-traits ((block luvcraft:block-kind))
  (append
   (list (list (if (plusp (luvcraft:block-light-emission block)) :light :light-off)
               "Light" (format nil "~D / 15" (luvcraft:block-light-emission block))))
   (when (plusp (luvcraft:block-surface-emission block))
     (list (list :glow "Glow" (format nil "~,2F" (luvcraft:block-surface-emission block)))))
   (list (list :solid (if (luvcraft:block-solid-p block) "Solid block" "Non-solid") nil))))

(defun draw-material-trait-icon (medium kind x y)
  "Small native vector icons; no dependence on the font's symbol coverage."
  (let ((ink (ui-ink (if (member kind '(:light :glow)) :selection :muted))))
    (case kind
      ((:light :light-off)
       (draw-analytic-rounded-rectangle* medium (+ x 2) (- y 8) (+ x 14) (+ y 4)
                                         :radius 6 :ink ink)
       (draw-analytic-rounded-rectangle* medium (+ x 5) (+ y 3) (+ x 11) (+ y 8)
                                         :radius 1 :ink ink))
      (:glow
       (draw-analytic-rounded-rectangle* medium (+ x 7) (- y 8) (+ x 9) (+ y 8)
                                         :radius 1 :ink ink)
       (draw-analytic-rounded-rectangle* medium x (- y 1) (+ x 16) (+ y 1)
                                         :radius 1 :ink ink))
      (t
       (draw-analytic-rounded-rectangle* medium (+ x 2) (- y 6) (+ x 14) (+ y 6)
                                         :radius 2 :ink ink)))))

(defun draw-inventory-details (frame pane selected entry)
  (declare (ignore frame))
  (draw-block-icon pane selected 704 78 80)
  (draw-text* pane (inventory-clipped-label (inventory-entry-label entry) 19)
              744 187 :align-x :center :align-y :center
              :text-face :bold :text-size (ui-text-size :body) :ink (ui-ink :text))
  (loop for line in (inventory-material-description selected) for y from 215 by 20
        do (draw-text* pane line 744 y :align-x :center :align-y :center
                       :text-size (ui-text-size :caption) :ink (ui-ink :muted)))
  (with-sheet-medium (medium pane)
    (draw-analytic-rounded-rectangle* medium 678 255 808 256 :radius 0 :ink (ui-ink :border))
    (loop for (icon label value) in (inventory-material-traits selected)
          for y from 275 by 27
          do (draw-material-trait-icon medium icon 678 y)
             (draw-text* pane label 702 y :align-y :center
                         :text-size (ui-text-size :caption) :ink (ui-ink :text))
             (when value
               (draw-text* pane value 810 y :align-x :right :align-y :center
                           :text-size (ui-text-size :caption) :ink (ui-ink :text))))))

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
      (draw-ui-surface medium 16 58 654 350 :fill :raised :radius 8)
      (draw-ui-surface medium 662 58 824 350 :fill :raised :radius 8)
      (with-drawing-options (pane :clipping-region
                                 (make-rectangle* *inventory-grid-left* *inventory-grid-top*
                                                  *inventory-grid-right* *inventory-grid-bottom*))
        (loop for entry in entries for index from 0
              for column = (mod index +inventory-grid-columns+)
              for row = (floor index +inventory-grid-columns+)
              for top = (- (+ *inventory-grid-top* (* row cell-height)) (inventory-scroll frame))
              when (and (< top *inventory-grid-bottom*) (> (+ top cell-height) *inventory-grid-top*))
                do (draw-inventory-entry frame pane entry 10
                                         (+ *inventory-grid-left* (* column cell-width) 3)
                                         (+ top 3) (- cell-width 6) (- cell-height 6))))
      (let ((maximum (inventory-max-scroll (length entries))))
        (when (plusp maximum)
          (let* ((height (- *inventory-grid-bottom* *inventory-grid-top*))
                 (thumb (* height (/ height (+ height maximum))))
                 (top (+ *inventory-grid-top* (* (- height thumb) (/ (inventory-scroll frame) maximum)))))
            (draw-analytic-rounded-rectangle* medium 646 top 650 (+ top thumb)
                                              :radius 2 :ink (ui-ink :accent)))))
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

(defun inventory-slot-at (u v entry-count &optional (scroll 0))
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
               (floor (* (+ scroll (- y *inventory-grid-top*)) +inventory-grid-rows+)
                      (- *inventory-grid-bottom* *inventory-grid-top*)))
             (slot (+ column (* row +inventory-grid-columns+))))
        (and (< slot entry-count) slot)))))

(defun inventory-block-at (frame u v)
  (let* ((entries (inventory-filtered-entries frame))
         (slot (inventory-slot-at u v (length entries) (inventory-scroll frame))))
    (when slot (luvcraft:block-inventory-entry-block (nth slot entries)))))

(defun choose-inventory-material (overlay session block)
  "Equip directly without changing the player's saved speedbar."
  (setf (luvcraft:player-selected-block session) block)
  (luvcraft:update-player-instruments session)
  (close-luvcraft-inventory overlay))

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
            (hover-inventory-block frame nil)
            nil)
          (destructuring-bind (u v) uv
            (typecase event
              (luv:canvas-pointer-motion-event
               (hover-inventory-block frame (inventory-block-at frame u v)))
              (luv:canvas-pointer-wheel-event
               (let ((notches (luv:canvas-pointer-event-scroll-y event)))
                 (unless (zerop notches)
                   ;; Preserve fractional trackpad deltas; clip only at the tray edges.
                   (scroll-inventory frame (* -32 notches)))))
              (luv:canvas-pointer-button-press-event
               (when (eq :left (luv:canvas-pointer-event-button event))
                 (let ((block (inventory-block-at frame u v)))
                   (when block (choose-inventory-material overlay session block))))))
            t)))))

(defun inventory-key-slot (event)
  "The one-based quickbar slot a number key names: 1-9, and 0 as ten."
  (let ((character (or (luv:canvas-key-event-unshifted-character event)
                       (luv:canvas-key-event-character event))))
    (alexandria:when-let
        ((digit (and (characterp character) (digit-char-p character))))
      (if (zerop digit) 10 digit))))

(defun move-inventory-cursor (frame delta)
  "Move through the catalog and scroll the focused row into view."
  (let* ((entries (inventory-filtered-entries frame))
         (current (position (or (inventory-hovered-block frame)
                                (luvcraft:player-selected-block (inventory-session frame)))
                            entries :key #'luvcraft:block-inventory-entry-block))
         (index (max 0 (min (1- (length entries)) (+ (or current 0) delta)))))
    (when entries
      (let* ((top (* (floor index +inventory-grid-columns+) (inventory-row-height)))
             (bottom (+ top (inventory-row-height)))
             (height (- *inventory-grid-bottom* *inventory-grid-top*)))
        (setf (inventory-scroll frame)
              (max 0 (min (inventory-max-scroll (length entries))
                          (cond ((< top (inventory-scroll frame)) top)
                                ((> bottom (+ (inventory-scroll frame) height)) (- bottom height))
                                (t (inventory-scroll frame)))))))
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
       (move-inventory-cursor frame +inventory-page-size+)
       t)
      ((eq (luv:canvas-key-event-key-name event) :page-up)
       (move-inventory-cursor frame (- +inventory-page-size+))
       t)
      ((member (luv:canvas-key-event-key-name event) '(:left :right :up :down))
       (move-inventory-cursor frame
                              (case (luv:canvas-key-event-key-name event)
                                (:left -1) (:right 1)
                                (:up (- +inventory-grid-columns+))
                                (:down +inventory-grid-columns+)))
       t)
      ((member (luv:canvas-key-event-key-name event) '(:return :enter :space))
       (choose-inventory-material overlay session
                                  (or (inventory-hovered-block frame)
                                      (luvcraft:player-selected-block session)))
       t)
      ((eq (luv:canvas-key-event-key-name event) :home)
       (move-inventory-cursor frame (- (length (inventory-filtered-entries frame)))) t)
      ((eq (luv:canvas-key-event-key-name event) :end)
       (move-inventory-cursor frame (length (inventory-filtered-entries frame))) t)
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
