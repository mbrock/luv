(in-package #:mcluv)

;;; An intentionally disposable backend fixture for worksheet stage BY51K0.
;;; It is not the workbench shell: two ordinary child sheets are enough to
;;; expose layout, retained composition, clipping, hit testing, and focus.

(defclass workbench-backend-proof-pane (transparent-gpu-application-pane)
  ((role :initarg :role :reader workbench-backend-proof-pane-role)))

(defclass workbench-backend-proof-layout-pane
    (clime:never-repaint-background-mixin
     climi::multiple-child-composite-pane)
  ())

(defmethod initialize-instance :after
    ((pane workbench-backend-proof-layout-pane) &key contents)
  (dolist (child contents)
    (sheet-adopt-child pane child)))

(defmethod compose-space
    ((pane workbench-backend-proof-layout-pane)
     &key (width 640) (height 480))
  (declare (ignore pane))
  (make-space-requirement :width width :height height
                          :min-width 1 :min-height 1
                          :max-width +fill+ :max-height +fill+))

(defun workbench-backend-proof-pane-region (role width height)
  (flet ((x (fraction) (round (* width fraction)))
         (y (fraction) (round (* height fraction))))
    (ecase role
      (:back (make-bounding-rectangle (x 0.08) (y 0.10)
                                      (x 0.68) (y 0.72)))
      (:front (make-bounding-rectangle (x 0.36) (y 0.30)
                                       (x 0.92) (y 0.86))))))

(defmethod allocate-space
    ((pane workbench-backend-proof-layout-pane) width height)
  (dolist (child (sheet-children pane))
    (with-bounding-rectangle* (left top :width child-width :height child-height)
        (workbench-backend-proof-pane-region
         (workbench-backend-proof-pane-role child) width height)
      (move-and-resize-sheet
       child left top child-width child-height))))

(define-application-frame workbench-backend-proof ()
  ((log :initform nil :accessor workbench-backend-proof-log))
  (:menu-bar nil)
  (:panes
   (back (make-pane 'workbench-backend-proof-pane :role :back
                    :scroll-bars nil))
   (front (make-pane 'workbench-backend-proof-pane :role :front
                     :scroll-bars nil)))
  (:layouts
   (default
    (make-pane 'workbench-backend-proof-layout-pane
               :contents (list back front)))))

(defun workbench-backend-proof-note (pane kind &optional value)
  (let ((frame (pane-frame pane)))
    (push (list kind (workbench-backend-proof-pane-role pane) value)
          (workbench-backend-proof-log frame))
    (dispatch-repaint pane +everywhere+)))

(defmethod handle-event :after
    ((pane workbench-backend-proof-pane) (event window-manager-focus-event))
  (declare (ignore event))
  (workbench-backend-proof-note pane :focus))

(defmethod handle-event :after
    ((pane workbench-backend-proof-pane) (event pointer-button-press-event))
  (workbench-backend-proof-note pane :pointer
                                (list (pointer-event-x event)
                                      (pointer-event-y event))))

(defmethod handle-event :after
    ((pane workbench-backend-proof-pane) (event key-press-event))
  (workbench-backend-proof-note pane :key (keyboard-event-key-name event)))

(defmethod handle-repaint
    ((pane workbench-backend-proof-pane) region)
  (declare (ignore region))
  (with-bounding-rectangle* (left top right bottom) pane
    (let* ((front-p (eq :front (workbench-backend-proof-pane-role pane)))
           (frame (pane-frame pane))
           (focused-p (eq pane (port-keyboard-input-focus (port pane))))
           (ink (if front-p
                    (compose-in (make-rgb-color 0.92 0.34 0.18)
                                (make-opacity 0.82))
                    (compose-in (make-rgb-color 0.12 0.48 0.86)
                                (make-opacity 0.78)))))
      ;; Deliberately exceed the sheet bounds: the retained commands must carry
      ;; the child clip rather than painting into its sibling's exposed area.
      (draw-rectangle* pane (- left 24) (- top 24) (+ right 24) (+ bottom 24)
                       :ink ink)
      (draw-text* pane
                  (format nil "~A pane~:[~; · keyboard focus~]"
                          (if front-p "Front" "Back") focused-p)
                  (+ left 18) (+ top 30)
                  :text-style (make-text-style :sans-serif :bold :normal)
                  :ink +white+)
      (let ((latest (first (workbench-backend-proof-log frame))))
        (when latest
          (draw-text* pane (format nil "last event: ~{~A~^ / ~}" latest)
                      (+ left 18) (+ top 58) :ink +white+))))))

(defun workbench-backend-proof-mirror (frame)
  (sheet-direct-mirror (frame-top-level-sheet frame)))

(defun make-embedded-workbench-backend-proof
    (canvas context device width height)
  "Create the two-pane BY51K0 fixture over CANVAS at logical WIDTH by HEIGHT."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (*embedded-mirror-target* canvas)
         (*embedded-mirror-context* context)
         (*embedded-mirror-device* device)
         (frame (make-application-frame
                 'workbench-backend-proof :frame-manager manager :enable t))
         (mirror (workbench-backend-proof-mirror frame)))
    (make-gpu-frame-background-transparent frame)
    (dispatch-embedded-mirror-event
     mirror
     (make-instance 'luv:canvas-window-resized-event
                    :timestamp 0 :width width :height height))
    (repaint-gpu-mirror mirror)
    frame))

(defun destroy-workbench-backend-proof (frame)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
