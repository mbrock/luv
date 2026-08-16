(in-package #:mcluv)

;;; A tiny real-gadget proof for the canvas input bridge.  It deliberately
;;; enables the frame without entering a conventional backend event loop:
;;; luv's native canvas thread already delivers portable pointer events to
;;; McCLIM's distributor.

(define-application-frame widget-lab ()
  ((click-count
    :initform 0
    :accessor widget-lab-click-count)
   (toggle-value
    :initform nil
    :accessor widget-lab-toggle-value))
  (:menu-bar nil)
  (:panes
   (clicker :push-button
            :label "Click me"
            :activate-callback 'activate-widget-lab-clicker)
   (toggle :toggle-button
           :label "Toggle is off"
           :value nil
           :value-changed-callback 'change-widget-lab-toggle))
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

(defun open-widget-lab (&key (server-path '(:luv))
                             (title "McCLIM widgets on luv"))
  "Create, adopt, and enable a tiny interactive McCLIM gadget frame."
  (let* ((port (find-port :server-path server-path))
         ;; McCLIM main currently calls FIRST on the newly constructed
         ;; manager when a port has no managers yet.  Spell out that tiny
         ;; bootstrap here until FIND-FRAME-MANAGER's empty-port branch is
         ;; fixed upstream.
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (make-application-frame
            'widget-lab :frame-manager manager :enable t)))
    (setf (frame-pretty-name frame) title)
    frame))

(defun close-widget-lab (frame)
  "Destroy an OPEN-WIDGET-LAB frame and its luv canvas."
  (check-type frame widget-lab)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
