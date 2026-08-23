(in-package #:luvcraft.clim)

;;; Luvcraft contributes only its application vocabulary and modal-overlay
;;; lifecycle.  Search, retained GPU drawing, input editing, and execution on
;;; the owning command frame belong to luv/mcclim's shared M-x instrument.

(defparameter *command-menu-tables*
  '(luvcraft-movement luvcraft-world luvcraft-terminal luvcraft-window)
  "The semantic input layers whose directly owned commands M-x offers.")

(defun luvcraft-command-menu-entries ()
  "Return Luvcraft's currently defined argument-free commands as conses.

This compatibility-shaped query is useful to callers which only inspect the
vocabulary.  The live menu itself retains MCLUV:COMMAND-MENU-ENTRY objects."
  (mapcar (lambda (entry)
            (cons (mcluv:command-menu-entry-label entry)
                  (mcluv:command-menu-entry-command-name entry)))
          (mcluv:command-menu-entries-for-tables *command-menu-tables*)))

(defun matching-command-menu-entries (entries query)
  "Search legacy cons ENTRIES through the shared M-x matcher."
  (mcluv:matching-command-menu-entries entries query :key #'car))

(defclass luvcraft-command-menu-overlay
    (mcluv:luvcraft-hud-widget-overlay)
  ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-command-menu-overlay))
  (declare (ignore overlay))
  :hud)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-command-menu-overlay) session pass surface-texture)
  (declare (ignore pass))
  (let* ((frame (mcluv:widget-overlay-frame overlay))
         (canvas (luvcraft:luvcraft-session-canvas session)))
    (mcluv:prepare-command-menu frame)
    (mcluv:prepare-direct-widget-overlay
     overlay session surface-texture
     (mcluv:command-menu-screen-state
      frame
      (list (luv:canvas-width canvas) (luv:canvas-height canvas)))))
  overlay)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-command-menu-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft:luvcraft-focus-camera-pose
    ((overlay luvcraft-command-menu-overlay) session)
  (declare (ignore session))
  (let ((camera (luvcraft:luvcraft-session-camera
                 (mcluv:widget-overlay-session overlay))))
    (luvcraft::make-camera-pose
     (luvcraft::copy-camera-position (luvcraft:camera-position camera))
     (luvcraft:camera-yaw camera) (luvcraft:camera-pitch camera)
     luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defmethod luvcraft:luvcraft-focus-entered
    ((overlay luvcraft-command-menu-overlay) session)
  (setf (luvcraft:luvcraft-session-pointer-capture-suspended-p session) nil)
  overlay)

(defun find-luvcraft-command-menu (session)
  (find-if (lambda (overlay)
             (typep overlay 'luvcraft-command-menu-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun close-luvcraft-command-menu (overlay session)
  "Cancel and release SESSION's M-x menu."
  (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
    (luvcraft:unfocus-luvcraft-session session))
  ;; Removing the widget invokes its ordinary release protocol, which destroys
  ;; the shared McCLIM frame and all retained direct-GPU resources.
  (luvcraft:remove-luvcraft-overlay session overlay)
  nil)

(defun apply-luvcraft-command-menu-action
    (overlay session action command)
  (case action
    (:dismiss
     (close-luvcraft-command-menu overlay session))
    (:execute
     (mcluv:execute-command-menu-command
      (mcluv:widget-overlay-frame overlay) command
      :before-execute
      (lambda () (close-luvcraft-command-menu overlay session)))))
  t)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-command-menu-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (multiple-value-bind (action command)
      (mcluv:handle-command-menu-key-event
       (mcluv:widget-overlay-frame overlay) event)
    (apply-luvcraft-command-menu-action
     overlay session action command)))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-command-menu-overlay) session canvas
     (event luv:canvas-event))
  (declare (ignore overlay session canvas event))
  t)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-command-menu-overlay) session canvas
     (event luv:canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (alexandria:when-let
      ((uv (mcluv:luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (destructuring-bind (width height)
        (mcluv:widget-overlay-logical-size overlay)
      (multiple-value-bind (action command)
          (mcluv:handle-command-menu-pointer-press
           (mcluv:widget-overlay-frame overlay)
           (* (first uv) width) (* (second uv) height)
           (luv:canvas-pointer-event-button event))
        (apply-luvcraft-command-menu-action
         overlay session action command))))
  t)

(defun open-luvcraft-command-menu (session &key (title "luvcraft M-x"))
  "Create, attach, and focus SESSION's shared direct-GPU M-x instrument."
  (let ((frame nil)
        (overlay nil)
        (transferred-p nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf frame
                 (mcluv:make-embedded-command-menu
                  (luvcraft-session-frame session)
                  (luvcraft:luvcraft-session-canvas session)
                  (luvcraft:luvcraft-session-context session)
                  (luvcraft:luvcraft-session-device session)
                  :command-tables *command-menu-tables*
                  :title title))
           (let ((mirror (mcluv:command-menu-mirror frame)))
             (setf overlay
                   (make-instance 'luvcraft-command-menu-overlay
                                  :session session :frame frame :mirror mirror)
                   (mcluv:mirror-compositor mirror) overlay))
           (setf transferred-p t)
           (luvcraft:add-luvcraft-overlay session overlay)
           (luvcraft:focus-luvcraft-session session overlay)
           (setf completed-p t)
           overlay)
      (unless completed-p
        (cond
          ((and overlay
                (member overlay (luvcraft:luvcraft-session-overlays session)
                        :test #'eq))
           (ignore-errors
            (luvcraft:remove-luvcraft-overlay session overlay)))
          ((and overlay (not transferred-p))
           (ignore-errors (luvcraft:release-luvcraft-overlay overlay)))
          ((and frame (not transferred-p))
           (ignore-errors (mcluv:destroy-command-menu frame))))))))

(defun toggle-luvcraft-command-menu (session)
  (alexandria:if-let ((overlay (find-luvcraft-command-menu session)))
    (close-luvcraft-command-menu overlay session)
    (open-luvcraft-command-menu session))
  t)

(define-command (com-execute-command :command-table luvcraft-world
                                     :name "Execute Command"
                                     :keystroke (#\x :meta))
    ()
  (toggle-luvcraft-command-menu (luvcraft-command-session)))
