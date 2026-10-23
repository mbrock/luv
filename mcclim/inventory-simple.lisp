;;; The picture-first inventory and two independently remembered player kits.
(in-package #:mcluv)

(defvar *inventory-preferences-pathname* nil
  "Optional preferences file override for captures and isolated tests.")

(defparameter *simple-inventory-categories*
  '((:natural "Nature" :grass) (:masonry "Stone" :stone)
    (:woodwork "Wood" :planks) (:roofing "Roofs" :roof-tiles)
    (:luminous "Lights" :lantern) (:all "Everything" :red-wool)))

(defun inventory-preferences-pathname ()
  (or *inventory-preferences-pathname*
      (merge-pathnames
       #P"../inventory-players.sexp"
       (uiop:pathname-directory-pathname
        (luvcraft::default-luvcraft-world-pathname)))))

(defun default-inventory-preferences ()
  (list :version 1 :active 0
        :players (loop repeat 2 collect
                   (list :full nil :kit '(:grass :planks :bricks)
                         :bar (copy-list luvcraft::*luvcraft-starting-quickbar*)))))

(defun validate-inventory-preferences (data)
  "Reject a damaged preferences file before it can replace either player's kit."
  (unless (and (listp data) (eql 1 (getf data :version))
               (typep (getf data :active) '(integer 0 1))
               (listp (getf data :players)) (= 2 (length (getf data :players)))
               (every (lambda (player)
                        (and (listp player)
                             (member (getf player :full) '(nil t))
                             (every (lambda (key)
                                      (let ((names (getf player key)))
                                        (and (listp names)
                                             (<= (length names)
                                                 (if (eq key :kit) 6 10))
                                             (every #'keywordp names)
                                             (= (length names)
                                                (length (remove-duplicates names))))))
                                    '(:kit :bar))))
                      (getf data :players)))
    (error "Invalid inventory player preferences: ~S" data))
  data)

(defun read-inventory-preferences (path)
  (if (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (let ((*read-eval* nil) (*readtable* (copy-readtable nil)))
          (let ((data (read in nil :eof)))
            (unless (eq :eof (read in nil :eof))
              (error "Extra data in inventory preferences ~A" path))
            (validate-inventory-preferences data))))
      (default-inventory-preferences)))

(defun inventory-data (frame)
  (luvcraft:player-inventory (inventory-session frame)))

(defun inventory-simple-p (frame)
  (luvcraft::block-inventory-simple-p (inventory-data frame)))

(defun inventory-active-player (frame)
  (getf (inventory-preferences frame) :active))

(defun inventory-profile-description (frame)
  (let ((inventory (inventory-data frame)))
    (list :full (not (inventory-simple-p frame))
          :kit (mapcar #'luvcraft:block-kind-name
                       (luvcraft::block-inventory-kit inventory))
          :bar (mapcar (lambda (entry)
                         (luvcraft:block-kind-name
                          (luvcraft:block-inventory-entry-block entry)))
                       (subseq (luvcraft:block-inventory-entries inventory)
                               0 (min 10 (length (luvcraft:block-inventory-entries
                                                 inventory))))))))

(defun remember-inventory-profile (frame)
  (when (inventory-preferences frame)
    (let* ((data (inventory-preferences frame))
           (players (getf data :players))
           (index (inventory-active-player frame))
           (description (inventory-profile-description frame)))
      (unless (equal description (nth index players))
        (setf (nth index players) description)
        (luvcraft::request-world-checkpoint
         (inventory-preferences-writer frame) (copy-tree data))))))

(defun apply-inventory-profile (frame)
  (let* ((data (inventory-preferences frame))
         (profile (nth (getf data :active) (getf data :players)))
         (inventory (inventory-data frame))
         (session (inventory-session frame)))
    (flet ((blocks (names)
             (loop for name in names
                   for block = (luvcraft:block-kind-named name nil)
                   when (and block (luvcraft:block-inventory-entry-for inventory block))
                     collect block)))
      (setf (luvcraft::block-inventory-simple-p inventory) (not (getf profile :full))
            (luvcraft::block-inventory-kit inventory)
            (or (blocks (getf profile :kit))
                (list (luvcraft:block-kind-named :grass))))
      (luvcraft::arrange-luvcraft-quickbar session (getf profile :bar)))
    (when (and (inventory-simple-p frame)
               (luvcraft::block-inventory-kit inventory))
      (setf (luvcraft:player-selected-block session)
            (first (luvcraft::block-inventory-kit inventory))))
    (setf (inventory-category frame) :natural
          (inventory-page frame) 0
          (inventory-simple-focus frame) :blocks
          (inventory-simple-cursor frame) 0
          (inventory-pending-block frame) nil
          (inventory-added-time frame) nil
          (inventory-hovered-block frame) nil)
    (luvcraft:update-player-instruments session)))

(defun initialize-inventory-preferences (frame)
  (unless (inventory-preferences frame)
    (let* ((path (inventory-preferences-pathname))
           (data (read-inventory-preferences path)))
      (setf (inventory-preferences frame) data
            (inventory-preferences-writer frame)
            (luvcraft::make-world-checkpoint-writer path))
      (apply-inventory-profile frame)))
  frame)

(defun switch-inventory-player (frame)
  (remember-inventory-profile frame)
  (let ((data (inventory-preferences frame)))
    (setf (getf data :active) (- 1 (getf data :active)))
    (apply-inventory-profile frame)
    (luvcraft::request-world-checkpoint
     (inventory-preferences-writer frame) (copy-tree data)))
  (repaint-inventory frame))

(defun toggle-full-inventory (frame)
  (setf (luvcraft::block-inventory-simple-p (inventory-data frame))
        (not (inventory-simple-p frame))
        (inventory-page frame) 0
        (inventory-simple-cursor frame) 0
        (inventory-pending-block frame) nil
        (inventory-hovered-block frame) nil)
  (unless (assoc (inventory-category frame) *simple-inventory-categories*)
    (setf (inventory-category frame) :all))
  (when (and (inventory-simple-p frame)
             (not (member (luvcraft:player-selected-block (inventory-session frame))
                          (luvcraft::block-inventory-kit (inventory-data frame)))))
    (setf (luvcraft:player-selected-block (inventory-session frame))
          (first (luvcraft::block-inventory-kit (inventory-data frame)))))
  (luvcraft:update-player-instruments (inventory-session frame))
  (remember-inventory-profile frame)
  (repaint-inventory frame))

(defmethod luvcraft:release-luvcraft-overlay :before
    ((overlay luvcraft-inventory-overlay))
  (let* ((frame (widget-overlay-frame overlay))
         (writer (inventory-preferences-writer frame)))
    (when writer
      (remember-inventory-profile frame)
      (luvcraft::stop-world-checkpoint-writer writer)
      (setf (inventory-preferences-writer frame) nil))))

(defmethod luvcraft:refresh-luvcraft-overlay :after
    ((overlay luvcraft-inventory-overlay) session)
  (declare (ignore session))
  (remember-inventory-profile (widget-overlay-frame overlay)))

(defun simple-inventory-animation (frame)
  (when (inventory-added-time frame)
    (min 1.0 (/ (- (get-internal-real-time) (inventory-added-time frame))
                (* internal-time-units-per-second 0.35)))))

(defun inventory-simple-state (frame)
  (list (inventory-simple-p frame)
        (inventory-simple-focus frame) (inventory-simple-cursor frame)
        (inventory-pending-block frame)
        (luvcraft::block-inventory-kit (inventory-data frame))
        (and (inventory-preferences frame) (inventory-active-player frame))
        (let ((progress (simple-inventory-animation frame)))
          (and progress (floor (* 12 progress))))))

(defun simple-inventory-entries (frame)
  ;; Stable catalog order: adding a block to the kit must not move the cursor.
  (let* ((entries (sort (copy-list (inventory-filtered-entries frame))
                        #'string< :key #'inventory-entry-label))
         (start (* 6 (inventory-page frame))))
    (subseq entries (min start (length entries))
            (min (+ start 6) (length entries)))))

(defun simple-inventory-pages (frame)
  (max 1 (ceiling (length (inventory-filtered-entries frame)) 6)))

(defun simple-inventory-category (frame index)
  (setf (inventory-category frame)
        (first (nth (mod index (length *simple-inventory-categories*))
                    *simple-inventory-categories*))
        (inventory-page frame) 0
        (inventory-simple-cursor frame) 0)
  (repaint-inventory frame))

(defun simple-inventory-page (frame direction)
  (setf (inventory-page frame)
        (mod (+ (inventory-page frame) direction) (simple-inventory-pages frame))
        (inventory-simple-cursor frame) 0)
  (repaint-inventory frame))

(defun choose-simple-inventory-block (frame block &optional replacement)
  (multiple-value-bind (slot status)
      (luvcraft::add-building-kit-block (inventory-data frame) block replacement)
    (if (eq status :full)
        (setf (inventory-pending-block frame) block
              (inventory-simple-focus frame) :kit
              (inventory-simple-cursor frame) 0)
        (progn
          (setf (luvcraft:player-selected-block (inventory-session frame)) block
                (inventory-pending-block frame) nil
                (inventory-added-block frame) block
                (inventory-added-slot frame) slot
                (inventory-added-time frame) (get-internal-real-time)
                (inventory-simple-focus frame) :blocks)
          (luvcraft:update-player-instruments (inventory-session frame))
          (remember-inventory-profile frame))))
  (repaint-inventory frame))

(defun simple-inventory-card (index)
  (values (+ 20 (* 268 (mod index 3)))
          (+ 132 (* 112 (floor index 3))) 260 104))

(defun simple-inventory-slot (index)
  (values (+ 20 (* 134 index)) 390 126 64))

(defun simple-inventory-hit (x y)
  "Return one semantic target and its index, shared by mouse and keyboard."
  (cond ((and (<= 18 x 155) (<= 8 y 44)) (values :profile 0))
        ((and (<= 651 x 822) (<= 8 y 44)) (values :mode 0))
        ((and (<= 20 x 820) (<= 58 y 119))
         (values :categories (min 5 (floor (- x 20) 134))))
        ((and (<= 20 x 816) (<= 132 y 348))
         (let* ((column (floor (- x 20) 268))
                (row (floor (- y 132) 112)))
           (when (and (< column 3) (< row 2)
                      (< (mod (- x 20) 268) 260)
                      (< (mod (- y 132) 112) 104))
             (values :blocks (+ column (* row 3))))))
        ((and (<= 20 x 816) (<= 390 y 454))
         (values :kit (min 5 (floor (- x 20) 134))))
        ((and (<= 620 x 817) (<= 466 y 496)) (values :play 0))
        ((and (<= 20 x 57) (<= 351 y 382)) (values :previous 0))
        ((and (<= 784 x 821) (<= 351 y 382)) (values :next 0))))

(defun simple-inventory-activate (frame zone index)
  (case zone
    (:profile (switch-inventory-player frame))
    (:mode (toggle-full-inventory frame))
    (:play (unfocus-instrument (inventory-session frame)))
    (:categories (simple-inventory-category frame index)
                 (setf (inventory-simple-focus frame) :blocks))
    (:previous (simple-inventory-page frame -1))
    (:next (simple-inventory-page frame 1))
    (:blocks
     (alexandria:when-let ((entry (nth index (simple-inventory-entries frame))))
       (choose-simple-inventory-block
        frame (luvcraft:block-inventory-entry-block entry))))
    (:kit
     (let ((kit (luvcraft::block-inventory-kit (inventory-data frame))))
       (cond ((inventory-pending-block frame)
              (choose-simple-inventory-block frame (inventory-pending-block frame) index))
             ((nth index kit)
              (setf (luvcraft:player-selected-block (inventory-session frame))
                    (nth index kit))
              (luvcraft:update-player-instruments (inventory-session frame)))
             (t (setf (inventory-simple-focus frame) :blocks
                      (inventory-simple-cursor frame) 0)))))))

(defun simple-inventory-key (frame event)
  (let* ((key (luv:canvas-key-event-key-name event))
         (zone (inventory-simple-focus frame))
         (cursor (inventory-simple-cursor frame))
         (category (or (position (inventory-category frame)
                                 *simple-inventory-categories* :key #'first) 0))
         (count (length (simple-inventory-entries frame))))
    (when (and (inventory-pending-block frame)
               (member key '(:up :down :left :right :tab :page-up :page-down)))
      (setf (inventory-simple-focus frame) :kit
            (inventory-simple-cursor frame)
            (mod (+ cursor (if (member key '(:left :up :page-up)) -1 1)) 6))
      (repaint-inventory frame)
      (return-from simple-inventory-key t))
    (case key
      (:escape
       (if (inventory-pending-block frame)
           (setf (inventory-pending-block frame) nil
                 (inventory-simple-focus frame) :blocks
                 (inventory-simple-cursor frame) 0)
           (unfocus-instrument (inventory-session frame))))
      (:return (simple-inventory-activate frame zone
                 (if (eq zone :categories) category cursor)))
      (:tab
       (setf (inventory-simple-focus frame)
             (if (eq zone :blocks) :kit :blocks)
             (inventory-simple-cursor frame) 0))
      ((:page-up :page-down)
       (simple-inventory-page frame (if (eq key :page-up) -1 1)))
      ((:left :right)
       (let ((step (if (eq key :left) -1 1)))
         (case zone
           (:categories (simple-inventory-category frame (+ category step)))
           (:kit (setf (inventory-simple-cursor frame) (mod (+ cursor step) 6)))
           (:blocks
            (let ((next (+ cursor step)))
              (cond ((< next 0)
                     (simple-inventory-page frame -1)
                     (setf (inventory-simple-cursor frame)
                           (1- (length (simple-inventory-entries frame)))))
                    ((>= next count) (simple-inventory-page frame 1))
                    (t (setf (inventory-simple-cursor frame) next))))))))
      (:up
       (case zone
         (:kit (setf (inventory-simple-focus frame) :blocks
                     (inventory-simple-cursor frame) (max 0 (1- count))))
         (:blocks (if (< cursor 3)
                      (setf (inventory-simple-focus frame) :categories)
                      (decf (inventory-simple-cursor frame) 3)))))
      (:down
       (case zone
         (:categories (setf (inventory-simple-focus frame) :blocks
                            (inventory-simple-cursor frame) 0))
         (:blocks (if (< (+ cursor 3) count)
                      (incf (inventory-simple-cursor frame) 3)
                      (setf (inventory-simple-focus frame) :kit
                            (inventory-simple-cursor frame) 0)))))
      ((:backspace :delete)
       (when (and (eq zone :kit) (not (inventory-pending-block frame)))
         (let ((inventory (inventory-data frame)))
           (when (> (length (luvcraft::block-inventory-kit inventory)) 1)
             (setf (luvcraft::block-inventory-kit inventory)
                   (loop for block in (luvcraft::block-inventory-kit inventory)
                         for i from 0 unless (= i cursor) collect block))
             (setf (luvcraft:player-selected-block (inventory-session frame))
                   (first (luvcraft::block-inventory-kit inventory)))
             (luvcraft:update-player-instruments (inventory-session frame))
             (remember-inventory-profile frame)))))))
    (repaint-inventory frame))

(defmethod luvcraft:handle-luvcraft-focus-event :around
    ((overlay luvcraft-inventory-overlay) session canvas
     (event luv:canvas-key-press-event))
  (let* ((frame (widget-overlay-frame overlay))
         (key (luv:canvas-key-event-key-name event)))
    (cond ((and (member key '(:f2 :f3))
                (not (luv:canvas-key-event-repeat-p event)))
           (if (eq key :f2) (switch-inventory-player frame)
               (toggle-full-inventory frame)) t)
          ((inventory-simple-p frame)
           (if (eq key :i)
               (unfocus-instrument session)
               (simple-inventory-key frame event)) t)
          (t (call-next-method)))))

(defmethod luvcraft:handle-luvcraft-overlay-event :around
    ((overlay luvcraft-inventory-overlay) session canvas
     (event luv:canvas-pointer-event))
  (let* ((frame (widget-overlay-frame overlay))
         (uv (and (inventory-overlay-visible-p overlay)
                  (luvcraft-widget-texture-coordinate
                   overlay (luv:canvas-pointer-event-x event)
                   (luv:canvas-pointer-event-y event)))))
    (if (null uv)
        (call-next-method)
        (multiple-value-bind (zone index)
            (simple-inventory-hit (* 840 (first uv)) (* 500 (second uv)))
          (cond
            ((and (member zone '(:profile :mode))
                  (typep event 'luv:canvas-pointer-button-press-event)
                  (eq :left (luv:canvas-pointer-event-button event)))
             (simple-inventory-activate frame zone index) t)
            ((inventory-simple-p frame)
             (typecase event
               (luv:canvas-pointer-motion-event
                (when (and (member zone '(:blocks :kit :categories))
                           (or (not (inventory-pending-block frame))
                               (eq zone :kit)))
                  (setf (inventory-simple-focus frame) zone
                        (inventory-simple-cursor frame) index)))
               (luv:canvas-pointer-wheel-event
                (unless (zerop (luv:canvas-pointer-event-scroll-y event))
                  (simple-inventory-page frame
                    (if (minusp (luv:canvas-pointer-event-scroll-y event)) 1 -1))))
               (luv:canvas-pointer-button-press-event
                (when (eq :left (luv:canvas-pointer-event-button event))
                  (when (or (null (inventory-pending-block frame))
                            (member zone '(:kit :play :mode :profile)))
                    (simple-inventory-activate frame zone index)))))
             (repaint-inventory frame) t)
            (t (call-next-method)))))))

(defparameter *simple-paper* (make-rgb-color 0.84 0.78 0.64))
(defparameter *simple-ink* (make-rgb-color 0.025 0.055 0.035))
(defparameter *simple-focus* (make-rgb-color 0.98 0.70 0.16))

(defun simple-panel (pane x y width height &key selected dark)
  (with-sheet-medium (medium pane)
    (draw-analytic-rounded-rectangle*
     medium x y (+ x width) (+ y height) :radius 9
     :ink (if selected *simple-focus*
              (if dark *simple-ink* (make-rgb-color 0.70 0.65 0.53))))
    (when selected
      (draw-analytic-rounded-rectangle*
       medium (+ x 2) (+ y 2) (- (+ x width) 2) (- (+ y height) 2)
       :radius 7
       :ink (if dark *simple-ink* (make-rgb-color 0.70 0.65 0.53))))))

(defun simple-label (pane text x y &key (size 15) light)
  (draw-text* pane text x y :align-x :center :align-y :center
              :text-size size :ink (if light *simple-paper* *simple-ink*)))

(defun draw-inventory-header (frame pane)
  (let ((simple (inventory-simple-p frame)))
    (if simple
        (progn
          (draw-rectangle* pane 0 0 840 47 :ink *simple-paper*)
          (simple-panel pane 18 8 137 34 :dark t)
          (simple-label pane "Choose your building blocks" 408 27 :size 20))
        (with-sheet-medium (medium pane)
          (draw-hotbar-slot pane medium 18 10 155 42)
          (draw-text* pane "Material library" 174 27 :align-y :center
                      :text-size 16 :ink *inventory-text-ink*)))
    (draw-text* pane (format nil "Player ~D  · F2"
                            (1+ (inventory-active-player frame)))
                86 25 :align-x :center :align-y :center :text-size 13
                :ink (if simple *simple-paper* *inventory-text-ink*))
    (let ((ink (if simple *simple-ink* *inventory-text-ink*)))
      (draw-rectangle* pane 654 15 674 35 :filled nil :ink ink :line-thickness 2)
      (unless simple
        (draw-line* pane 657 25 662 31 :ink ink :line-thickness 2)
        (draw-line* pane 662 31 671 18 :ink ink :line-thickness 2))
      (draw-text* pane "Full inventory · F3" 683 25 :align-y :center
                  :text-size 13 :ink ink))))

(defun draw-simple-inventory (frame pane)
  (let* ((inventory (inventory-data frame))
         (kit (luvcraft::block-inventory-kit inventory))
         (entries (simple-inventory-entries frame))
         (zone (inventory-simple-focus frame))
         (cursor (inventory-simple-cursor frame)))
    (draw-rectangle* pane 0 0 840 500 :ink *simple-paper*)
    (simple-label pane "Choose your building blocks" 408 27 :size 21)
    (loop for (category label name) in *simple-inventory-categories*
          for i from 0 for x = (+ 20 (* i 134)) do
            (simple-panel pane x 58 126 61
                          :selected (and (eq zone :categories)
                                         (eq category (inventory-category frame)))
                          :dark (eq category (inventory-category frame)))
            (draw-block-icon pane (luvcraft:block-kind-named name) (+ x 47) 62 31)
            (simple-label pane label (+ x 63) 105 :size 13
                          :light (eq category (inventory-category frame))))
    (loop for entry in entries for i from 0
          for block = (luvcraft:block-inventory-entry-block entry) do
            (multiple-value-bind (x y width height) (simple-inventory-card i)
              (simple-panel pane x y width height
                            :selected (and (eq zone :blocks) (= cursor i)))
              (draw-block-icon pane block (+ x 100) (+ y 5) 64)
              (simple-label pane
                            (string-capitalize
                             (substitute #\Space #\-
                                         (symbol-name (luvcraft:block-kind-name block))))
                            (+ x 130) (+ y 86) :size 15)
              (when (member block kit)
                (simple-label pane "✓" (+ x 238) (+ y 19) :size 20))))
    (simple-label pane "‹" 38 366 :size 25)
    (simple-label pane "›" 802 366 :size 25)
    (simple-label pane
                  (if (inventory-pending-block frame)
                      "Kit full — choose a block to replace"
                      (format nil "My blocks     ·     Page ~D of ~D"
                              (1+ (inventory-page frame)) (simple-inventory-pages frame)))
                  420 367 :size 15)
    (loop for i below 6 for block = (nth i kit) do
      (multiple-value-bind (x y width height) (simple-inventory-slot i)
        (simple-panel pane x y width height :dark t
                      :selected (or (and (eq zone :kit) (= cursor i))
                                    (eq block (luvcraft:player-selected-block
                                               (inventory-session frame)))))
        (if block
            (draw-block-icon pane block (+ x 41) (+ y 7) 46)
            (simple-label pane "+" (+ x 63) (+ y 31) :size 27 :light t))))
    (simple-label pane
                  (if (inventory-pending-block frame)
                      "← ↑ ↓ →  Move     Enter  Replace     Esc  Cancel"
                      "← ↑ ↓ →  Move     Enter  Add     Tab  My blocks")
                  305 483 :size 14)
    (simple-panel pane 620 466 197 30 :dark t)
    (simple-label pane "▶  Let's build  · Esc" 718 481 :size 14 :light t)
    (alexandria:when-let ((progress (simple-inventory-animation frame)))
      (when (< progress 1)
        (let* ((entry (position (inventory-added-block frame) entries
                               :key #'luvcraft:block-inventory-entry-block))
               (from-x (+ 150 (* 268 (mod (or entry 0) 3))))
               (from-y (+ 180 (* 112 (floor (or entry 0) 3))))
               (to-x (+ 83 (* 134 (inventory-added-slot frame))))
               (x (+ from-x (* progress (- to-x from-x))))
               (y (+ from-y (* progress (- 422 from-y)))))
          (draw-block-icon pane (inventory-added-block frame) (- x 24) (- y 24) 48))))))

(defmethod handle-repaint :around ((pane inventory-pane) region)
  (let ((frame (pane-frame pane)))
    (with-text-style (pane (make-text-style :sans-serif :roman :normal))
      (if (and (inventory-preferences frame) (inventory-simple-p frame))
          (draw-simple-inventory frame pane)
          (call-next-method))
      (when (inventory-preferences frame)
        (draw-inventory-header frame pane)))))
