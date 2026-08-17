;;; The metabar: a drawer of live knobs, slid in from the left with RET.
;;;
;;; The game's grading, the terminal's emissions, and the sky's clock are
;;; TUNABLES (LUVCRAFT/TUNABLES.LISP): each one names its place, its range,
;;; its step, and what must happen after a change.  Its verbs -- focus what
;;; the crosshair is on, quit -- are ACTIONS from the same file.  The metabar
;;; is the gadget that offers both: one McCLIM pane, composited as a HUD
;;; strip down the left edge, one row per tunable with a track, a value, and
;;; a pair of nudge buttons, and a button per action beneath them.  It is a
;;; tool, not a thing in the world; it does not care where the player is
;;; standing.
;;;
;;; RET slides it out and takes the focus so the keys drive it: up and down
;;; choose a row, left and right nudge (shift for ten steps) or press the
;;; chosen action, RET or escape slide it back.  The mouse works too, on the
;;; tracks and the buttons, and the wheel over a row nudges it.

(in-package #:mcluv)

(defparameter *metabar-width* 460)
(defparameter *metabar-row-height* 74)
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
(defparameter *metabar-track-ink* (make-rgb-color 0.30 0.30 0.28))
(defparameter *metabar-fill-ink* (make-rgb-color 0.58 0.78 0.54))
(defparameter *metabar-knob-ink* (make-rgb-color 0.93 0.93 0.90))
(defparameter *metabar-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *metabar-muted-ink* (make-rgb-color 0.60 0.60 0.55))
(defparameter *metabar-edge-ink* (make-rgb-color 0.42 0.42 0.38))

(defclass metabar-pane (application-pane) ())

(define-application-frame luvcraft-metabar ()
  ((session :initarg :session :reader metabar-session)
   (selected :initform 0 :accessor metabar-selected)
   ;; The values as last painted, so a change made from a SLY buffer or by
   ;; another control shows up without anyone asking.
   (visible-state :initform nil :accessor metabar-visible-state))
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'metabar-pane)))
  (:layouts
   (default
    (horizontally (:width *metabar-width* :height (metabar-height)) bar))))

(defun metabar-row-count ()
  "Rows are the tunables followed by the actions, one selection order."
  (+ (length luvcraft:*tunables*) (length luvcraft:*actions*)))

(defun metabar-height ()
  (+ *metabar-top-pad*
     (* *metabar-row-height* (length luvcraft:*tunables*))
     (* *metabar-action-height* (length luvcraft:*actions*))
     *metabar-bottom-pad*))

(defun metabar-row-top (index)
  "The top of row INDEX: a tunable's row, or, past them, an action's."
  (let ((tunables (length luvcraft:*tunables*)))
    (if (< index tunables)
        (+ *metabar-top-pad* (* index *metabar-row-height*))
        (+ *metabar-top-pad* (* tunables *metabar-row-height*)
           (* (- index tunables) *metabar-action-height*)))))

(defun metabar-row-height (index)
  (if (< index (length luvcraft:*tunables*))
      *metabar-row-height*
      *metabar-action-height*))

(defun metabar-row-at (v)
  "The row index at texture-space V, or NIL."
  (let ((y (* v (metabar-height))))
    (loop for index below (metabar-row-count)
          for top = (metabar-row-top index)
          when (and (<= top y) (< y (+ top (metabar-row-height index))))
            return index)))

(defun metabar-visible-state-for (frame)
  (list* (metabar-selected frame)
         (mapcar (lambda (tunable)
                   (luvcraft:tunable-value tunable (metabar-session frame)))
                 luvcraft:*tunables*)))

;;; ---------------------------------------------------------------------
;;; Painting.

(defun draw-metabar-action (frame pane action index)
  (let* ((top (metabar-row-top index))
         (bottom (+ top *metabar-action-height*))
         (selected-p (= index (metabar-selected frame))))
    (draw-analytic-rounded-rectangle*
     pane 10 (+ top 4) (- *metabar-width* 10) (- bottom 4) :radius 6
     :ink (if selected-p *metabar-fill-ink* *metabar-selected-row-ink*))
    (draw-text* pane (luvcraft:action-label action)
                (/ *metabar-width* 2) (/ (+ top bottom) 2)
                :align-x :center :align-y :center :text-size 16
                :ink (if selected-p *metabar-panel-ink* *metabar-text-ink*))))

(defun draw-metabar-row (frame pane tunable index)
  (let* ((session (metabar-session frame))
         (top (metabar-row-top index))
         (bottom (+ top *metabar-row-height*))
         (selected-p (= index (metabar-selected frame)))
         (track-y (+ top 52))
         (fraction (luvcraft:tunable-fraction tunable session))
         (knob-x (+ *metabar-track-left*
                    (* fraction (- *metabar-track-right*
                                   *metabar-track-left*)))))
    (draw-rectangle* pane 6 (+ top 2) (- *metabar-width* 6) (- bottom 2)
                     :ink (if selected-p
                              *metabar-selected-row-ink*
                              *metabar-row-ink*))
    (when selected-p
      (draw-rectangle* pane 6 (+ top 2) 9 (- bottom 2)
                       :ink *metabar-fill-ink*))
    (draw-text* pane (luvcraft:tunable-label tunable)
                *metabar-pad* (+ top 24)
                :align-y :center :text-size 17
                :ink (if selected-p +white+ *metabar-text-ink*))
    (draw-text* pane (luvcraft:format-tunable-value tunable session)
                (- *metabar-width* *metabar-pad*) (+ top 24)
                :align-x :right :align-y :center :text-size 17
                :text-face :bold
                :ink (if selected-p *metabar-fill-ink* *metabar-text-ink*))
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
        (loop for tunable in luvcraft:*tunables*
              for index from 0
              do (draw-metabar-row frame pane tunable index))
        (loop for action in luvcraft:*actions*
              for index from (length luvcraft:*tunables*)
              do (draw-metabar-action frame pane action index))))))

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

(defun metabar-selected-tunable (frame)
  (nth (metabar-selected frame) luvcraft:*tunables*))

(defun metabar-selected-action (frame)
  (let ((index (- (metabar-selected frame) (length luvcraft:*tunables*))))
    (and (>= index 0) (nth index luvcraft:*actions*))))

(defun metabar-nudge (overlay direction &optional (multiplier 1))
  "Nudge the chosen tunable, or, if an action is chosen, press it."
  (let* ((frame (widget-overlay-frame overlay))
         (tunable (metabar-selected-tunable frame)))
    (if tunable
        (progn
          (luvcraft:step-tunable tunable (widget-overlay-session overlay)
                                 direction multiplier)
          (repaint-metabar frame))
        (metabar-press overlay))))

(defun metabar-press (overlay)
  "Press the chosen action, if the choice is one."
  (alexandria:when-let
      ((action (metabar-selected-action (widget-overlay-frame overlay))))
    (luvcraft:run-action action (widget-overlay-session overlay))))

(defun metabar-select (overlay index)
  (let ((frame (widget-overlay-frame overlay))
        (count (metabar-row-count)))
    (when (plusp count)
      (setf (metabar-selected frame) (mod index count))
      (repaint-metabar frame))))

(defun metabar-set-from-track (overlay index u)
  "Set row INDEX's tunable from the pointer's texture-space U on its track."
  (let* ((x (* u *metabar-width*))
         (fraction (max 0.0 (min 1.0 (/ (- x *metabar-track-left*)
                                        (- *metabar-track-right*
                                           *metabar-track-left*)))))
         (tunable (nth index luvcraft:*tunables*))
         (session (widget-overlay-session overlay))
         (minimum (luvcraft:tunable-minimum tunable))
         (maximum (luvcraft:tunable-maximum tunable))
         (step (luvcraft:tunable-step tunable))
         (raw (+ minimum (* fraction (- maximum minimum))))
         (quantized (* step (round raw step))))
    (luvcraft:set-tunable-value
     tunable
     (coerce quantized (type-of (luvcraft:tunable-value tunable session)))
     session)
    (repaint-metabar (widget-overlay-frame overlay))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore canvas))
  (let ((uv (luvcraft-widget-texture-coordinate
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
         (alexandria:when-let ((row (metabar-row-at (second uv))))
           (when (< row (length luvcraft:*tunables*))
             (metabar-select overlay row)
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
         (alexandria:when-let ((row (metabar-row-at (second uv))))
           (metabar-select overlay row)
           (if (>= row (length luvcraft:*tunables*))
               (metabar-press overlay)
               (let ((x (* (first uv) *metabar-width*)))
                 (cond ((< x *metabar-track-left*)
                        (metabar-nudge overlay -1))
                       ((> x *metabar-track-right*)
                        (metabar-nudge overlay 1))
                       (t
                        (setf (metabar-dragging overlay) row)
                        (metabar-set-from-track overlay row (first uv)))))))
         t))
      (t (and uv t)))))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let ((key (luv:canvas-key-event-key-name event))
        (multiplier
          (if (member :shift (luv:canvas-key-event-modifiers event)) 10 1)))
    (case key
      ((:escape :return) (close-luvcraft-metabar overlay))
      ((:up :k) (metabar-select
                 overlay (1- (metabar-selected (widget-overlay-frame overlay)))))
      ((:down :j) (metabar-select
                   overlay (1+ (metabar-selected (widget-overlay-frame overlay)))))
      ((:left :h) (metabar-nudge overlay -1 multiplier))
      ((:right :l) (metabar-nudge overlay 1 multiplier))
      ((:space) (metabar-press overlay))))
  t)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-key-release-event))
  (declare (ignore session canvas event))
  t)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-metabar-overlay) session)
  (declare (ignore session))
  nil)

(defmethod luvcraft:luvcraft-focus-camera-pose
    ((overlay luvcraft-metabar-overlay) session)
  ;; A tool: the camera stays exactly where it is, wide field of view too.
  (declare (ignore session))
  (let ((camera (luvcraft::luvcraft-session-camera
                 (widget-overlay-session overlay))))
    (luvcraft::make-camera-pose
     (luvcraft::copy-camera-position (luvcraft:camera-position camera))
     (luvcraft:camera-yaw camera) (luvcraft:camera-pitch camera)
     luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defmethod luvcraft:luvcraft-focus-left
    ((overlay luvcraft-metabar-overlay) session)
  (declare (ignore session))
  ;; Losing the focus -- to a wall, a click in the world -- slides the bar
  ;; away too.
  (setf (metabar-open-p overlay) nil)
  overlay)

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
