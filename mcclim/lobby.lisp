;;; A quiet corner instrument for the luvcraft lobby radio.

(in-package #:mcluv)

(defconstant +lobby-hud-width+ 300)
(defconstant +lobby-hud-height+ 124)

(defclass lobby-hud-pane (application-pane) ())

(define-application-frame luvcraft-lobby-hud ()
  ((client :initarg :client :reader lobby-hud-client)
   (visible-state :initform nil :accessor lobby-hud-visible-state))
  (:menu-bar nil)
  (:panes (sheet (make-pane 'lobby-hud-pane)))
  (:layouts
   (default
    (horizontally (:width +lobby-hud-width+ :height +lobby-hud-height+) sheet))))

(defun lobby-hud-state (frame)
  (multiple-value-list
   (luvcraft:lobby-client-snapshot (lobby-hud-client frame))))

(defun lobby-status-ink (status)
  (ecase status
    (:online (make-rgb-color 0.34 0.86 0.43))
    (:connecting (make-rgb-color 0.88 0.70 0.28))
    (:offline (make-rgb-color 0.62 0.34 0.31))
    ((:starting :stopped) (make-rgb-color 0.36 0.39 0.42))))

(defmethod handle-repaint ((pane lobby-hud-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (state (lobby-hud-state frame))
         (status (first state))
         (peers (second state))
         (error (third state)))
    (with-sheet-medium (medium pane)
      (when (typep medium 'luv-raster-medium)
        (clear-raster-medium-reliefs medium))
      (draw-analytic-rounded-rectangle*
       medium 2 2 (- +lobby-hud-width+ 2) (- +lobby-hud-height+ 2)
       :radius 12 :ink (make-rgb-color 0.025 0.030 0.034))
      (draw-circle* pane 18 21 5 :ink (lobby-status-ink status))
      (draw-text* pane "lobby" 32 21 :align-y :center :text-size 14
                  :text-face :bold :ink (make-rgb-color 0.76 0.79 0.78))
      (draw-text* pane (string-downcase (symbol-name status))
                  (- +lobby-hud-width+ 14) 21
                  :align-x :right :align-y :center :text-size 11
                  :ink (make-rgb-color 0.43 0.47 0.47))
      (cond
        (peers
         (loop for name in peers
               for y from 48 by 22
               repeat 4
               do (draw-circle* pane 20 y 4
                                :ink (make-rgb-color 0.34 0.86 0.43))
                  (draw-text* pane name 33 y :align-y :center :text-size 13
                              :ink (make-rgb-color 0.72 0.76 0.73))))
        ((and error (eq status :offline))
         (draw-text* pane
                     (if (> (length error) 38)
                         (concatenate 'string (subseq error 0 35) "...")
                         error)
                     16 50 :align-y :center :text-size 11
                     :ink (make-rgb-color 0.47 0.48 0.46)))
        (t
         (draw-text* pane "quiet" 16 50 :align-y :center :text-size 11
                     :ink (make-rgb-color 0.38 0.41 0.40)))))))

(defun repaint-lobby-hud (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  (setf (lobby-hud-visible-state frame) (lobby-hud-state frame))
  frame)

(defclass luvcraft-lobby-hud-overlay (luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-lobby-hud-overlay))
  (declare (ignore overlay))
  :hud)

(defun lobby-hud-screen-state (overlay)
  (destructuring-bind (viewport-width viewport-height)
      (luv:canvas-extent
       (luvcraft:luvcraft-session-context (widget-overlay-session overlay)))
    (let* ((half-width (/ +lobby-hud-width+ viewport-width))
           (half-height (/ +lobby-hud-height+ viewport-height))
           (center-x (+ -1.0 (/ 16.0 viewport-width) half-width))
           (center-y (+ -1.0 (/ 16.0 viewport-height) half-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list center-x center-y 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-lobby-hud-overlay) session pass surface-texture)
  (declare (ignore pass))
  (prepare-direct-widget-overlay
   overlay session surface-texture (lobby-hud-screen-state overlay))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-lobby-hud-overlay) session)
  (declare (ignore session))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (equal (lobby-hud-state frame) (lobby-hud-visible-state frame))
      (repaint-lobby-hud frame)))
  overlay)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-lobby-hud-overlay) session)
  (declare (ignore overlay session))
  nil)

(defun open-luvcraft-lobby-hud (session)
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target* (luvcraft:luvcraft-session-canvas session))
                 (*embedded-mirror-context* (luvcraft:luvcraft-session-context session))
                 (*embedded-mirror-device* (luvcraft:luvcraft-session-device session)))
             (make-application-frame
              'luvcraft-lobby-hud :frame-manager manager :enable t
              :client (luvcraft:luvcraft-session-lobby-client session)))))
    (setf (frame-pretty-name frame) "luvcraft lobby")
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay (make-instance 'luvcraft-lobby-hud-overlay
                                   :session session :frame frame :mirror mirror)))
      (setf (mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (repaint-lobby-hud frame)
      overlay)))

(defmethod luvcraft:start-luvcraft-lobby :after
    ((session luvcraft:luvcraft-session))
  (when (luvcraft:luvcraft-session-lobby-client session)
    (open-luvcraft-lobby-hud session)))
