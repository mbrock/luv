;;; A shared retained-GPU lobby instrument.
;;;
;;; HANDLE-REPAINT reads only the frame's immutable semantic snapshot.  The
;;; application refresh seam copies lobby state before paint, then the direct
;;; compositor evaluates analytic media and Slug text in the destination's
;;; native pixels with ordinary premultiplied-alpha composition.

(in-package #:luv.lobby.mcclim)

(defconstant +lobby-hud-width+ 320)
(defconstant +lobby-hud-height+ 140)
(defconstant +lobby-hud-margin+ 16)

(defun lobby-hud-alpha-ink (red green blue alpha)
  (compose-in (make-rgb-color red green blue) (make-opacity alpha)))

(defparameter *lobby-hud-shadow-ink*
  (lobby-hud-alpha-ink 0.0 0.0 0.0 0.30))
(defparameter *lobby-hud-edge-ink*
  (lobby-hud-alpha-ink 0.44 0.49 0.46 0.48))
(defparameter *lobby-hud-panel-ink*
  (lobby-hud-alpha-ink 0.025 0.030 0.034 0.84))
(defparameter *lobby-hud-text-ink* (make-rgb-color 0.82 0.85 0.83))
(defparameter *lobby-hud-muted-ink* (make-rgb-color 0.49 0.53 0.51))

(defclass lobby-hud-pane (mcluv:transparent-gpu-application-pane) ())

(define-application-frame lobby-hud ()
  ((client :initarg :client :reader lobby-hud-client)
   (visible-snapshot
    :initarg :visible-snapshot
    :accessor lobby-hud-visible-snapshot)
   (painted-revision :initform -1 :accessor lobby-hud-painted-revision)
   (compositor :initform nil :accessor lobby-hud-compositor))
  (:menu-bar nil)
  ;; The pane background is part of McCLIM's retained stream.  Leaving it at
  ;; the implementation default paints an opaque white rectangle behind the
  ;; rounded translucent card even though the card itself is analytic.
  (:panes (sheet (make-pane 'lobby-hud-pane
                            :background +transparent-ink+
                            :width +lobby-hud-width+
                            :height +lobby-hud-height+
                            :min-width +lobby-hud-width+
                            :min-height +lobby-hud-height+
                            :max-width +lobby-hud-width+
                            :max-height +lobby-hud-height+)))
  ;; This single authored surface is its own layout.  An intermediate rack
  ;; pane would contribute the implementation's opaque background.
  (:layouts (default sheet)))

(defun lobby-status-ink (status)
  (ecase status
    (:online (make-rgb-color 0.34 0.86 0.43))
    ((:starting :connecting) (make-rgb-color 0.88 0.70 0.28))
    (:offline (make-rgb-color 0.72 0.38 0.32))
    ((:stopping :stopped) (make-rgb-color 0.38 0.42 0.41))))

(defun lobby-short-error (error)
  (when error
    (if (> (length error) 42)
        (concatenate 'string (subseq error 0 39) "...")
        error)))

(defmethod handle-repaint ((pane lobby-hud-pane) region)
  (declare (ignore region))
  ;; This is deliberately the only state read by paint.  CLIENT's lock and
  ;; worker are outside the McCLIM repaint transaction.
  (let* ((snapshot (lobby-hud-visible-snapshot (pane-frame pane)))
         (status (luv.lobby:lobby-snapshot-status snapshot))
         (peers (luv.lobby:lobby-snapshot-peers snapshot))
         (error (luv.lobby:lobby-snapshot-last-error snapshot)))
    (with-sheet-medium (medium pane)
      (mcluv:draw-analytic-rounded-rectangle*
       medium 4 5 (- +lobby-hud-width+ 2) (- +lobby-hud-height+ 1)
       :radius 14 :ink *lobby-hud-shadow-ink*)
      (mcluv:draw-analytic-rounded-rectangle*
       medium 1 1 (- +lobby-hud-width+ 3) (- +lobby-hud-height+ 5)
       :radius 13 :ink *lobby-hud-edge-ink*)
      (mcluv:draw-analytic-rounded-rectangle*
       medium 2 2 (- +lobby-hud-width+ 4) (- +lobby-hud-height+ 6)
       :radius 12 :ink *lobby-hud-panel-ink*)
      (draw-circle* pane 20 22 5 :ink (lobby-status-ink status))
      (draw-text* pane "lobby" 34 22
                  :align-y :center :text-size 14 :text-face :bold
                  :ink *lobby-hud-text-ink*)
      (draw-text* pane (string-downcase (symbol-name status))
                  (- +lobby-hud-width+ 18) 22
                  :align-x :right :align-y :center :text-size 11
                  :ink *lobby-hud-muted-ink*)
      (cond
        (peers
         (loop for peer in peers
               for y from 51 by 21
               repeat 4
               do (draw-circle* pane 21 y 4
                                :ink (make-rgb-color 0.34 0.86 0.43))
                  (draw-text* pane (luv.lobby:lobby-peer-name peer) 35 y
                              :align-y :center :text-size 13
                              :ink *lobby-hud-text-ink*)))
        ((and error (eq status :offline))
         (draw-text* pane (lobby-short-error error) 17 52
                     :align-y :center :text-size 11
                     :ink *lobby-hud-muted-ink*))
        (t
         (draw-text* pane "quiet" 17 52
                     :align-y :center :text-size 11
                     :ink *lobby-hud-muted-ink*))))))

(defun lobby-hud-mirror (frame &key (errorp t))
  (let* ((sheet (frame-top-level-sheet frame))
         (mirror (and sheet (mcluv::sheet-direct-mirror sheet))))
    (cond
      ((typep mirror 'mcluv:luv-gpu-mirror) mirror)
      (errorp (error "Lobby HUD requires an embedded direct-GPU mirror, got ~S."
                     mirror))
      (t nil))))

(defun validate-lobby-hud-direct-presentation (frame)
  (let* ((mirror (lobby-hud-mirror frame))
         (sheet (mcluv:mirror-sheet mirror)))
    (when (mcluv:mirror-texture mirror)
      (error "Lobby HUD unexpectedly acquired a raster backing texture."))
    (dolist (painted-sheet (mcluv::gpu-sheet-paint-order sheet))
      (let ((medium (mcluv::gpu-sheet-presentation-medium painted-sheet)))
        (when (and (typep medium 'mcluv:luv-gpu-medium)
                   (mcluv:gpu-medium-fallback-report medium))
          (error "Lobby HUD used decomposed primitive fallbacks: ~S"
                 (mcluv:gpu-medium-fallback-report medium)))))
    (when (find-if
           (lambda (command)
             (typep command 'mcluv::gpu-prepared-image-command))
           (mcluv::gpu-mirror-prepared-commands mirror))
      (error "Lobby HUD prepared an image/raster command.")))
  frame)

(defun repaint-lobby-hud (frame)
  (let ((mirror (lobby-hud-mirror frame)))
    (unless (mcluv:mirror-embedded-p mirror)
      (error "Lobby HUD mirror is not embedded in its application canvas."))
    (mcluv:repaint-gpu-mirror mirror)
    (validate-lobby-hud-direct-presentation frame)
    (setf (lobby-hud-painted-revision frame)
          (luv.lobby:lobby-snapshot-revision
           (lobby-hud-visible-snapshot frame))))
  frame)

(defun refresh-lobby-hud (frame)
  "Copy the current semantic snapshot and repaint only when its revision moved."
  (let ((snapshot
          (luv.lobby:lobby-client-snapshot (lobby-hud-client frame))))
    (if (/= (luv.lobby:lobby-snapshot-revision snapshot)
            (lobby-hud-painted-revision frame))
        (progn
          ;; Publish the complete value before McCLIM begins its repaint.
          (setf (lobby-hud-visible-snapshot frame) snapshot)
          (repaint-lobby-hud frame))
        (mcluv:prepare-gpu-mirror-compositor
         (lobby-hud-mirror frame))))
  frame)

(defun lobby-hud-screen-state
    (frame viewport-logical-extent &key (margin +lobby-hud-margin+))
  "Place FRAME in the lower-left using destination logical coordinates.

The affine state is expressed against the logical viewport.  A high-density
drawable therefore supplies its native samples directly to analytic edges and
Slug glyphs; no panel-sized texture is enlarged."
  (declare (ignore frame))
  (destructuring-bind (viewport-width viewport-height)
      viewport-logical-extent
    (let* ((scale
             (min 1.0
                  (/ (max 1.0 (- viewport-width (* 2 margin)))
                     +lobby-hud-width+)
                  (/ (max 1.0 (- viewport-height (* 2 margin)))
                     +lobby-hud-height+)))
           (half-width (/ (* +lobby-hud-width+ scale) viewport-width))
           (half-height (/ (* +lobby-hud-height+ scale) viewport-height))
           (center-x (+ -1.0 (/ (* 2.0 margin) viewport-width) half-width))
           (center-y (+ -1.0 (/ (* 2.0 margin) viewport-height) half-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list center-x center-y 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(defun make-embedded-lobby-hud
    (client canvas context device
     &key (title "lobby") (install-compositor-p t))
  "Create CLIENT's textureless retained HUD on an application's GPU owner."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager
           (or (first (clim-internals::frame-managers port))
               (make-instance 'mcluv:luv-frame-manager :port port)))
         (snapshot (luv.lobby:lobby-client-snapshot client))
         (frame
           (let ((mcluv:*embedded-mirror-target* canvas)
                 (mcluv:*embedded-mirror-context* context)
                 (mcluv:*embedded-mirror-device* device))
             (make-application-frame
              'lobby-hud :frame-manager manager :enable t
              :client client :visible-snapshot snapshot))))
    (setf (frame-pretty-name frame) title)
    (mcluv:make-gpu-frame-background-transparent frame)
    (handler-case
        (let* ((mirror (lobby-hud-mirror frame))
               (compositor
                 (and install-compositor-p
                      (make-instance 'mcluv:direct-gpu-mirror-compositor
                                     :mirror mirror))))
          (unless (and (mcluv:mirror-embedded-p mirror)
                       (null (mcluv:mirror-texture mirror)))
            (error "Lobby HUD did not receive a textureless embedded mirror."))
          (when compositor
            (setf (mcluv:mirror-compositor mirror) compositor
                  (lobby-hud-compositor frame) compositor))
          (repaint-lobby-hud frame)
          frame)
      (error (condition)
        (unless (eq :disowned (frame-state frame))
          (destroy-frame frame))
        (error condition)))))

(defun destroy-lobby-hud (frame)
  (when (and frame (not (eq :disowned (frame-state frame))))
    (destroy-frame frame))
  nil)
