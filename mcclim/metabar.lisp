;;; The metabar: a drawer of live knobs, slid in from the left with RET.
;;;
;;; The game's knobs (LUVCRAFT/KNOBS.LISP) -- grading, the sun, the sky's
;;; clock, the terminal's emissions, the player's stride -- each name their
;;; place, their quantity and so their unit, their range, and, by their
;;; class, what must happen after a change.  Its verbs -- focus what the
;;; crosshair is on, quit -- are ACTIONS from the same file.  The metabar is
;;; the gadget that offers both: one McCLIM pane, composited as a HUD strip
;;; down the left edge.  The knobs come in groups, one open at a time, each
;;; a header row that opens on RET; under the open one, a row per knob -- a
;;; track, a value with its unit, and a pair of nudge buttons for a scalar,
;;; a switch for a switch -- and a button per action beneath them all.  It
;;; is a tool, not a thing in the world; it does not care where the player
;;; is standing.
;;;
;;; RET slides it out and takes the focus so the keys drive it: up and down
;;; choose a row, left and right nudge (shift for ten steps), flip, or press
;;; the chosen thing; RET on a group opens it, RET elsewhere or escape slides
;;; the bar back.  The mouse works too, on the headers, tracks, switches, and
;;; buttons, and the wheel over a knob nudges it.

(in-package #:mcluv)

(defparameter *metabar-width* 460)
(defparameter *metabar-row-height* 74)
(defparameter *metabar-switch-height* 46)
(defparameter *metabar-header-height* 36)
(defparameter *metabar-top-pad* 8)
(defparameter *metabar-bottom-pad* 8)
(defparameter *metabar-action-height* 44)
(defparameter *metabar-pad* 18)
(defparameter *metabar-track-left* 58)
(defparameter *metabar-track-right* 400)
(defparameter *metabar-button-width* 26)

(defparameter *metabar-panel-ink* (make-rgb-color 0.11 0.11 0.105))
(defparameter *metabar-row-ink* (make-rgb-color 0.16 0.16 0.15))
(defparameter *metabar-selected-row-ink* (make-rgb-color 0.22 0.23 0.21))
(defparameter *metabar-header-ink* (make-rgb-color 0.13 0.13 0.125))
(defparameter *metabar-track-ink* (make-rgb-color 0.30 0.30 0.28))
(defparameter *metabar-fill-ink* (make-rgb-color 0.58 0.78 0.54))
(defparameter *metabar-rebuild-ink* (make-rgb-color 0.85 0.68 0.40))
(defparameter *metabar-knob-ink* (make-rgb-color 0.93 0.93 0.90))
(defparameter *metabar-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *metabar-muted-ink* (make-rgb-color 0.60 0.60 0.55))
(defparameter *metabar-edge-ink* (make-rgb-color 0.42 0.42 0.38))

(defclass metabar-pane (application-pane) ())

(define-application-frame luvcraft-metabar ()
  ((session :initarg :session :reader metabar-session)
   (selected :initform 0 :accessor metabar-selected)
   ;; The one group whose knobs are shown.
   (open-group :initform (first (luvcraft:knob-groups))
               :accessor metabar-open-group)
   ;; The values as last painted, so a change made from a SLY buffer or by
   ;; another control shows up without anyone asking.
   (visible-state :initform nil :accessor metabar-visible-state))
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'metabar-pane)))
  (:layouts
   (default
    (horizontally (:width *metabar-width* :height (metabar-height)) bar))))

;;; ---------------------------------------------------------------------
;;; Rows.
;;;
;;; A row is a header for a group, a knob of the open group, or an action;
;;; the frame's selection is an index into this list.

(defun metabar-rows (frame)
  (append
   (loop for group in (luvcraft:knob-groups)
         collect (list :group group)
         when (eq group (metabar-open-group frame))
           append (mapcar (lambda (knob) (list :knob knob))
                          (luvcraft:knobs-in-group group)))
   (mapcar (lambda (action) (list :action action)) luvcraft:*actions*)))

(defun metabar-row-count (frame)
  (length (metabar-rows frame)))

(defgeneric metabar-knob-row-height (knob)
  (:method ((knob luvcraft:scalar-knob)) *metabar-row-height*)
  (:method ((knob luvcraft:switch-knob)) *metabar-switch-height*))

(defun metabar-row-extent (row)
  (ecase (first row)
    (:group *metabar-header-height*)
    (:knob (metabar-knob-row-height (second row)))
    (:action *metabar-action-height*)))

(defun metabar-height ()
  "Room for every header, the tallest group's knobs, and the actions: fixed
whichever group is open, so the pane need never be resized."
  (+ *metabar-top-pad*
     (* *metabar-header-height* (length (luvcraft:knob-groups)))
     (reduce #'max (luvcraft:knob-groups)
             :key (lambda (group)
                    (reduce #'+ (luvcraft:knobs-in-group group)
                            :key #'metabar-knob-row-height :initial-value 0))
             :initial-value 0)
     (* *metabar-action-height* (length luvcraft:*actions*))
     *metabar-bottom-pad*))

(defun metabar-row-top (frame index)
  "The top of row INDEX."
  (+ *metabar-top-pad*
     (loop for row in (metabar-rows frame)
           repeat index
           sum (metabar-row-extent row))))

(defun metabar-row-at (frame v)
  "The row index at texture-space V, or NIL."
  (let ((y (* v (metabar-height)))
        (top *metabar-top-pad*))
    (loop for row in (metabar-rows frame)
          for index from 0
          for bottom = (+ top (metabar-row-extent row))
          when (and (<= top y) (< y bottom))
            return index
          do (setf top bottom))))

(defun metabar-selected-row (frame)
  (nth (metabar-selected frame) (metabar-rows frame)))

(defun metabar-selected-knob (frame)
  (let ((row (metabar-selected-row frame)))
    (and (eq (first row) :knob) (second row))))

(defun metabar-selected-action (frame)
  (let ((row (metabar-selected-row frame)))
    (and (eq (first row) :action) (second row))))

(defun metabar-selected-group (frame)
  (let ((row (metabar-selected-row frame)))
    (and (eq (first row) :group) (second row))))

(defun metabar-visible-state-for (frame)
  (list* (metabar-selected frame)
         (metabar-open-group frame)
         (mapcar (lambda (knob)
                   (luvcraft:knob-value knob (metabar-session frame)))
                 luvcraft:*knobs*)))

;;; ---------------------------------------------------------------------
;;; Painting.

(defun draw-metabar-header (frame pane group top selected-p)
  (let ((bottom (+ top *metabar-header-height*))
        (open-p (eq group (metabar-open-group frame))))
    (draw-rectangle* pane 6 (+ top 2) (- *metabar-width* 6) (- bottom 2)
                     :ink (if selected-p
                              *metabar-selected-row-ink*
                              *metabar-header-ink*))
    (when selected-p
      (draw-rectangle* pane 6 (+ top 2) 9 (- bottom 2)
                       :ink *metabar-fill-ink*))
    (draw-text* pane (if open-p "▾" "▸")
                *metabar-pad* (/ (+ top bottom) 2)
                :align-y :center :text-size 15
                :ink (if open-p *metabar-fill-ink* *metabar-muted-ink*))
    (draw-text* pane (string-downcase group)
                (+ *metabar-pad* 22) (/ (+ top bottom) 2)
                :align-y :center :text-size 15 :text-face :bold
                :ink (if (or open-p selected-p) +white+ *metabar-muted-ink*))
    (draw-text* pane (format nil "~D" (length (luvcraft:knobs-in-group group)))
                (- *metabar-width* *metabar-pad*) (/ (+ top bottom) 2)
                :align-x :right :align-y :center :text-size 13
                :ink *metabar-muted-ink*)))

(defun draw-metabar-action (frame pane action top selected-p)
  (declare (ignore frame))
  (let ((bottom (+ top *metabar-action-height*)))
    (draw-analytic-rounded-rectangle*
     pane 10 (+ top 4) (- *metabar-width* 10) (- bottom 4) :radius 6
     :ink (if selected-p *metabar-fill-ink* *metabar-selected-row-ink*))
    (draw-text* pane (luvcraft:action-label action)
                (/ *metabar-width* 2) (/ (+ top bottom) 2)
                :align-x :center :align-y :center :text-size 16
                :ink (if selected-p *metabar-panel-ink* *metabar-text-ink*))))

(defun draw-metabar-knob-title (frame pane knob top selected-p)
  "The row's label, its value with its unit, and, for a knob folded into a
shader, the mark that turning it rebuilds one."
  (let ((session (metabar-session frame)))
    (draw-text* pane (luvcraft:knob-label knob)
                *metabar-pad* (+ top 24)
                :align-y :center :text-size 17
                :ink (if selected-p +white+ *metabar-text-ink*))
    (when (luvcraft:shader-knob-p knob)
      ;; A dot after the label, in the rebuild colour: turning this one
      ;; recompiles a shader, so it answers in tens of milliseconds, not
      ;; on the next frame.
      (draw-circle* pane
                    (+ *metabar-pad* 10
                       (text-size pane (luvcraft:knob-label knob)
                                  :text-style (make-text-style nil nil 17)))
                    (+ top 24) 4
                    :ink *metabar-rebuild-ink*))
    (draw-text* pane (luvcraft:format-knob-value knob session)
                (- *metabar-width* *metabar-pad*) (+ top 24)
                :align-x :right :align-y :center :text-size 17
                :text-face :bold
                :ink (if selected-p *metabar-fill-ink* *metabar-text-ink*))))

(defgeneric draw-metabar-knob (frame pane knob top selected-p)
  (:documentation "Paint KNOB's row with its top edge at TOP."))

(defmethod draw-metabar-knob :before (frame pane knob top selected-p)
  (declare (ignore frame))
  (let ((bottom (+ top (metabar-knob-row-height knob))))
    (draw-rectangle* pane 6 (+ top 2) (- *metabar-width* 6) (- bottom 2)
                     :ink (if selected-p
                              *metabar-selected-row-ink*
                              *metabar-row-ink*))
    (when selected-p
      (draw-rectangle* pane 6 (+ top 2) 9 (- bottom 2)
                       :ink *metabar-fill-ink*))))

(defmethod draw-metabar-knob (frame pane (knob luvcraft:scalar-knob) top selected-p)
  (let* ((session (metabar-session frame))
         (track-y (+ top 52))
         (fraction (luvcraft:knob-fraction knob session))
         (knob-x (+ *metabar-track-left*
                    (* fraction (- *metabar-track-right*
                                   *metabar-track-left*)))))
    (draw-metabar-knob-title frame pane knob top selected-p)
    ;; The nudge buttons at either end of the track.
    (draw-text* pane "−" (- *metabar-track-left* 18) track-y
                :align-x :center :align-y :center :text-size 24
                :ink *metabar-muted-ink*)
    (draw-text* pane "+" (+ *metabar-track-right* 18) track-y
                :align-x :center :align-y :center :text-size 24
                :ink *metabar-muted-ink*)
    ;; The track, its filled part, and the knob.
    (draw-rectangle* pane *metabar-track-left* (- track-y 4)
                     *metabar-track-right* (+ track-y 4)
                     :ink *metabar-track-ink*)
    (draw-rectangle* pane *metabar-track-left* (- track-y 4)
                     knob-x (+ track-y 4)
                     :ink *metabar-fill-ink*)
    (draw-circle* pane knob-x track-y 9 :ink *metabar-knob-ink*)
    (draw-circle* pane knob-x track-y 9 :filled nil :line-thickness 1
                  :ink *metabar-panel-ink*)))

(defmethod draw-metabar-knob (frame pane (knob luvcraft:switch-knob) top selected-p)
  ;; A pill at the right: filled and to the right when on.
  (let* ((session (metabar-session frame))
         (on-p (luvcraft:knob-value knob session))
         (y (+ top (/ *metabar-switch-height* 2)))
         (right (- *metabar-width* *metabar-pad*))
         (left (- right 46)))
    (draw-text* pane (luvcraft:knob-label knob)
                *metabar-pad* y
                :align-y :center :text-size 17
                :ink (if selected-p +white+ *metabar-text-ink*))
    (draw-analytic-rounded-rectangle*
     pane left (- y 11) right (+ y 11) :radius 11
     :ink (if on-p *metabar-fill-ink* *metabar-track-ink*))
    (draw-circle* pane (if on-p (- right 11) (+ left 11)) y 8
                  :ink *metabar-knob-ink*)))

(defmethod handle-repaint ((pane metabar-pane) region)
  (declare (ignore region))
  (let ((frame (pane-frame pane)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'luv-raster-medium)
          (clear-raster-medium-reliefs medium))
        (draw-rectangle* pane left top right bottom :ink *metabar-panel-ink*)
        (draw-line* pane (- right 1) top (- right 1) bottom
                    :ink *metabar-edge-ink* :line-thickness 2)
        (let ((y *metabar-top-pad*))
          (loop for row in (metabar-rows frame)
                for index from 0
                for selected-p = (= index (metabar-selected frame))
                do (ecase (first row)
                     (:group (draw-metabar-header
                              frame pane (second row) y selected-p))
                     (:knob (draw-metabar-knob
                             frame pane (second row) y selected-p))
                     (:action (draw-metabar-action
                               frame pane (second row) y selected-p)))
                   (incf y (metabar-row-extent row))))))))

(defun repaint-metabar (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  (setf (metabar-visible-state frame) (metabar-visible-state-for frame))
  frame)

;;; ---------------------------------------------------------------------
;;; The overlay: a strip on the left that slides.

(defclass luvcraft-metabar-overlay (luvcraft-widget-overlay)
  ((slide :initform 0d0 :accessor metabar-slide
          :documentation "0 off the left edge, 1 fully out; eased.")
   (open-p :initform t :accessor metabar-open-p)
   (last-time :initform nil :accessor metabar-last-time)
   ;; The pointer is dragging a track's knob: which row, or NIL.
   (dragging :initform nil :accessor metabar-dragging)))

(defmethod luvcraft:luvcraft-overlay-stage ((overlay luvcraft-metabar-overlay))
  (declare (ignore overlay))
  :hud)

(defun metabar-screen-state (overlay)
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
         (scale (min 1.0 (/ (- viewport-height 24.0) source-height)))
         (half-width (/ (* source-width scale) viewport-width))
         (half-height (/ (* source-height scale) viewport-height))
         ;; Eased slide: from wholly off the left edge to flush against it.
         (slide (metabar-slide overlay))
         (eased (- 1d0 (expt (- 1d0 slide) 3)))
         (center-x (+ -1.0 (* half-width (- (* 2 eased) 1)))))
    (make-array
     12 :element-type 'single-float
     :initial-contents
     (mapcar (lambda (value) (coerce value 'single-float))
             (list center-x 0.0 0.0 1.0
                   half-width 0.0 0.0 0.0
                   0.0 half-height 0.0 0.0)))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-metabar-overlay) session pass surface-texture)
  (declare (ignore session))
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when (and source (plusp (metabar-slide overlay)))
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source
       :target-format (luv:gpu-texture-format surface-texture))
      (let* ((state (metabar-screen-state overlay))
             (frame-state
               (ensure-spinning-compositor-frame-state overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group pass 0 (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-metabar-overlay) session)
  ;; Ease the slide toward open or closed; a closed and fully slid-away
  ;; bar removes itself.
  (let* ((now (or (luvcraft::luvcraft-session-last-frame-time session) 0d0))
         (last (metabar-last-time overlay))
         (seconds (if last (min 0.1d0 (max 0d0 (- now last))) 0d0))
         (target (if (metabar-open-p overlay) 1d0 0d0))
         (slide (metabar-slide overlay))
         (step (* 6d0 seconds)))
    (setf (metabar-last-time overlay) now
          (metabar-slide overlay)
          (cond ((> target slide) (min target (+ slide step)))
                ((< target slide) (max target (- slide step)))
                (t slide)))
    (when (and (not (metabar-open-p overlay))
               (zerop (metabar-slide overlay)))
      (luvcraft:remove-luvcraft-overlay session overlay)
      (return-from luvcraft:refresh-luvcraft-overlay overlay)))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (equal (metabar-visible-state-for frame)
                   (metabar-visible-state frame))
      (repaint-metabar frame)))
  overlay)

;;; ---------------------------------------------------------------------
;;; Driving the knobs.

(defun metabar-nudge (overlay direction &optional (multiplier 1))
  "Turn the chosen knob; open the chosen group; press the chosen action."
  (let* ((frame (widget-overlay-frame overlay))
         (row (metabar-selected-row frame)))
    (ecase (first row)
      (:knob
       (luvcraft:step-knob (second row) (widget-overlay-session overlay)
                           direction multiplier)
       (repaint-metabar frame))
      (:group (metabar-open-group-row overlay))
      (:action (metabar-press overlay)))))

(defun metabar-press (overlay)
  "Press the chosen thing: an action runs, a switch flips, a group opens."
  (let* ((frame (widget-overlay-frame overlay))
         (row (metabar-selected-row frame)))
    (case (first row)
      (:action (luvcraft:run-action (second row)
                                    (widget-overlay-session overlay)))
      (:knob (when (typep (second row) 'luvcraft:switch-knob)
               (luvcraft:toggle-knob (second row)
                                     (widget-overlay-session overlay))
               (repaint-metabar frame)))
      (:group (metabar-open-group-row overlay)))))

(defun metabar-open-group-row (overlay &optional group)
  "Open GROUP, or the chosen row's group, keeping its header selected."
  (let* ((frame (widget-overlay-frame overlay))
         (group (or group (metabar-selected-group frame))))
    (when group
      (setf (metabar-open-group frame) group
            (metabar-selected frame)
            (position group (metabar-rows frame)
                      :test (lambda (group row)
                              (and (eq (first row) :group)
                                   (eq (second row) group)))))
      (repaint-metabar frame))))

(defun metabar-select (overlay index)
  (let ((frame (widget-overlay-frame overlay))
        (count (metabar-row-count (widget-overlay-frame overlay))))
    (when (plusp count)
      (setf (metabar-selected frame) (mod index count))
      (repaint-metabar frame))))

(defun metabar-set-from-track (overlay index u)
  "Set row INDEX's knob from the pointer's texture-space U on its track."
  (let* ((frame (widget-overlay-frame overlay))
         (row (nth index (metabar-rows frame)))
         (knob (and (eq (first row) :knob) (second row))))
    (when (typep knob 'luvcraft:scalar-knob)
      (let* ((x (* u *metabar-width*))
             (fraction (max 0.0 (min 1.0 (/ (- x *metabar-track-left*)
                                            (- *metabar-track-right*
                                               *metabar-track-left*)))))
             (session (widget-overlay-session overlay))
             (minimum (luvcraft:knob-minimum knob))
             (maximum (luvcraft:knob-maximum knob))
             (step (luvcraft:knob-step knob))
             (raw (+ minimum (* fraction (- maximum minimum))))
             (quantized (* step (round raw step))))
        (luvcraft:set-knob-value knob quantized session)
        (repaint-metabar frame)))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore canvas))
  (let* ((frame (widget-overlay-frame overlay))
         (uv (luvcraft-widget-texture-coordinate
              overlay
              (luv:canvas-pointer-event-x event)
              (luv:canvas-pointer-event-y event))))
    (typecase event
      (luv:canvas-pointer-button-release-event
       (setf (metabar-dragging overlay) nil)
       (and uv t))
      (luv:canvas-pointer-motion-event
       (alexandria:when-let ((row (metabar-dragging overlay)))
         (when uv
           (metabar-set-from-track overlay row (first uv))))
       (and uv t))
      (luv:canvas-pointer-wheel-event
       (when uv
         (alexandria:when-let ((index (metabar-row-at frame (second uv))))
           (when (eq (first (nth index (metabar-rows frame))) :knob)
             (metabar-select overlay index)
             (let ((delta (luv:canvas-pointer-event-scroll-y event)))
               (unless (zerop delta)
                 (metabar-nudge overlay (if (plusp delta) 1 -1))))))
         t))
      (luv:canvas-pointer-button-press-event
       ;; A click in the world, past the bar, puts the tool away.
       (when (and (null uv)
                  (eq overlay (luvcraft:luvcraft-session-modal-focus session)))
         (close-luvcraft-metabar overlay))
       (when (and uv (eq :left (luv:canvas-pointer-event-button event)))
         (luvcraft:focus-luvcraft-session session overlay)
         (alexandria:when-let ((index (metabar-row-at frame (second uv))))
           (metabar-select overlay index)
           (let ((row (nth index (metabar-rows frame))))
             (if (and (eq (first row) :knob)
                      (typep (second row) 'luvcraft:scalar-knob))
                 (let ((x (* (first uv) *metabar-width*)))
                   (cond ((< x *metabar-track-left*)
                          (metabar-nudge overlay -1))
                         ((> x *metabar-track-right*)
                          (metabar-nudge overlay 1))
                         (t
                          (setf (metabar-dragging overlay) index)
                          (metabar-set-from-track overlay index (first uv)))))
                 (metabar-press overlay))))
         t))
      (t (and uv t)))))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let ((key (luv:canvas-key-event-key-name event))
        (frame (widget-overlay-frame overlay))
        (multiplier
          (if (member :shift (luv:canvas-key-event-modifiers event)) 10 1)))
    (case key
      (:escape (close-luvcraft-metabar overlay))
      (:return (if (metabar-selected-group frame)
                   (metabar-open-group-row overlay)
                   (close-luvcraft-metabar overlay)))
      ((:up :k) (metabar-select overlay (1- (metabar-selected frame))))
      ((:down :j) (metabar-select overlay (1+ (metabar-selected frame))))
      ((:left :h) (metabar-nudge overlay -1 multiplier))
      ((:right :l) (metabar-nudge overlay 1 multiplier))
      ((:space) (metabar-press overlay))))
  t)

;;; ---------------------------------------------------------------------
;;; Opening and closing.

(defun find-luvcraft-metabar (session)
  (find-if (lambda (overlay) (typep overlay 'luvcraft-metabar-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun open-luvcraft-metabar (session &key (title "luvcraft metabar"))
  "Create, attach, slide in, and focus SESSION's metabar."
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
              'luvcraft-metabar :frame-manager manager :enable t
              :session session))))
    (setf (frame-pretty-name frame) title
          (metabar-visible-state frame) (metabar-visible-state-for frame))
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-metabar-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (when (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror))
      (luvcraft:focus-luvcraft-session session overlay)
      overlay)))

(defun close-luvcraft-metabar (overlay)
  "Slide OVERLAY away; it removes itself once it is off the edge."
  (check-type overlay luvcraft-metabar-overlay)
  (let ((session (widget-overlay-session overlay)))
    (setf (metabar-open-p overlay) nil)
    (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
      (luvcraft:unfocus-luvcraft-session session)))
  nil)

(defmethod luvcraft:toggle-luvcraft-metabar
    ((session luvcraft:luvcraft-session))
  (alexandria:if-let ((overlay (find-luvcraft-metabar session)))
    (if (metabar-open-p overlay)
        (close-luvcraft-metabar overlay)
        (progn
          (setf (metabar-open-p overlay) t)
          (luvcraft:focus-luvcraft-session session overlay)))
    (open-luvcraft-metabar session))
  t)
