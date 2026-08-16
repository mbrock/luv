(in-package #:mcluv)

;;; A tiny real-gadget proof for the canvas input bridge.  It deliberately
;;; enables the frame without entering a conventional backend event loop:
;;; luv's native canvas thread already delivers portable pointer events to
;;; McCLIM's distributor.

(defclass relief-button-pane (climi::push-button-pane) ())

(defclass relief-toggle-button-pane (climi::toggle-button-pane) ())

(defun repaint-relief-button (pane pressed-p color)
  (with-bounding-rectangle* (left top right bottom) pane
    (let* ((inset 8)
           (offset (if pressed-p 2 0))
           (height (if pressed-p -2.0 7.0))
           (center-x (/ (+ left right) 2))
           (center-y (+ offset (/ (+ top bottom) 2))))
      (draw-rectangle* pane left top right bottom
                       :ink (make-rgb-color 0.10 0.115 0.13))
      (with-sheet-medium (medium pane)
        (draw-analytic-rounded-rectangle*
         medium (+ left inset) (+ top inset offset)
         (- right inset) (- bottom inset (- offset))
         :radius 14 :ink (make-relief-design color height)))
      (draw-text* pane (gadget-label pane) center-x center-y
                  :align-x :center :align-y :center
                  :ink (make-rgb-color 0.96 0.97 0.98)))))

(defmethod handle-repaint ((pane relief-button-pane) region)
  (declare (ignore region))
  (with-slots (climi::armed climi::pressedp) pane
    (repaint-relief-button
     pane (and climi::armed climi::pressedp)
     (make-rgb-color 0.20 0.48 0.78))))

(defmethod handle-repaint ((pane relief-toggle-button-pane) region)
  (declare (ignore region))
  (with-slots (climi::armed climi::pressedp) pane
    (repaint-relief-button
     pane (and climi::armed climi::pressedp)
     (if (gadget-value pane)
         (make-rgb-color 0.20 0.68 0.48)
         (make-rgb-color 0.27 0.31 0.36)))))

(define-application-frame widget-lab ()
  ((click-count
    :initform 0
    :accessor widget-lab-click-count)
   (toggle-value
    :initform nil
    :accessor widget-lab-toggle-value))
  (:menu-bar nil)
  (:panes
   (clicker (make-pane 'relief-button-pane
                       :label "Click me"
                       :activate-callback 'activate-widget-lab-clicker))
   (toggle (make-pane 'relief-toggle-button-pane
                      :label "Toggle is off"
                      :value nil
                      :value-changed-callback 'change-widget-lab-toggle)))
  (:layouts
   (default
    (vertically (:width 360 :height 180 :spacing 18)
      clicker
      toggle))))

(defun activate-widget-lab-clicker (gadget)
  (let ((frame (gadget-client gadget)))
    (incf (widget-lab-click-count frame))
    (setf (gadget-label gadget)
          (format nil "Clicked ~D time~:P"
                  (widget-lab-click-count frame)))))

(defun change-widget-lab-toggle (gadget value)
  (let ((frame (gadget-client gadget)))
    (setf (widget-lab-toggle-value frame) value
          (gadget-label gadget)
          (if value "Toggle is on" "Toggle is off"))
    ;; The gadget's value repaint is requested before McCLIM invokes this
    ;; callback, so ask for another after changing its label.
    (dispatch-repaint gadget +everywhere+)))

(defun open-widget-lab (&key (server-path '(:luv-gpu))
                             (title "McCLIM widgets on luv")
                             target context device)
  "Create, adopt, and enable a tiny interactive McCLIM gadget frame."
  (when (and target (not (and context device)))
    (error ":TARGET requires the shared :CONTEXT and :DEVICE."))
  (let* ((port (find-port :server-path server-path))
         ;; McCLIM main currently calls FIRST on the newly constructed
         ;; manager when a port has no managers yet.  Spell out that tiny
         ;; bootstrap here until FIND-FRAME-MANAGER's empty-port branch is
         ;; fixed upstream.
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target* target)
                 (*embedded-mirror-context* context)
                 (*embedded-mirror-device* device))
             (make-application-frame
              'widget-lab :frame-manager manager :enable t))))
    (setf (frame-pretty-name frame) title)
    (alexandria:when-let*
        ((sheet (frame-top-level-sheet frame))
         (mirror (sheet-direct-mirror sheet)))
      (when (typep mirror 'luv-gpu-mirror)
        ;; MAKE-APPLICATION-FRAME performs ordinary pane repaints after the
        ;; mirror is enabled. Publish once more when the complete pane tree and
        ;; all of its media exist, so no construction-time partial frame wins.
        (repaint-gpu-mirror mirror)))
    frame))

(defun close-widget-lab (frame)
  "Destroy an OPEN-WIDGET-LAB frame and its luv canvas."
  (check-type frame widget-lab)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
