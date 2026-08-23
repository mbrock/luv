(in-package #:luft.render)

;;; LUFT owns only the application boundary of M-x: which command tables form
;;; its vocabulary, when the modal surface is attached, and where it enters the
;;; atelier's final pass.  Search, input editing, painting, and execution all
;;; remain in MCLUV's shared retained-GPU instrument.

(defclass viewer-command-menu-instrument ()
  ((frame :initarg :frame :reader viewer-command-menu-frame)
   (compositor :initarg :compositor :reader viewer-command-menu-compositor)))

(defmethod mcluv:command-menu-tables-for ((viewer viewer))
  (declare (ignore viewer))
  '(luft-window luft-atelier))

(defun viewer-command-menu-attachment (viewer)
  (find-if (lambda (instrument)
             (typep instrument 'viewer-command-menu-instrument))
           (viewer-instruments viewer)))

(defun viewer-command-menu-present-p (viewer)
  (not (null (viewer-command-menu-attachment viewer))))

(defmethod viewer-instrument-priority
    ((instrument viewer-command-menu-instrument))
  (declare (ignore instrument))
  1000)

(defmethod refresh-viewer-instrument
    ((instrument viewer-command-menu-instrument) viewer)
  (declare (ignore viewer))
  ;; Input normally published the revision synchronously.  This guard never
  ;; discovers commands and is a no-op for a clean retained snapshot.
  (mcluv:prepare-command-menu (viewer-command-menu-frame instrument))
  instrument)

(defmethod encode-viewer-instrument
    ((instrument viewer-command-menu-instrument)
     viewer pass surface-texture physical-extent)
  (declare (ignore physical-extent))
  (let ((frame (viewer-command-menu-frame instrument)))
    (mcluv:encode-direct-gpu-mirror
     (viewer-command-menu-compositor instrument)
     pass surface-texture
     ;; Both composition and inverse input use the SDL canvas's logical
     ;; destination extent.  The native drawable therefore supplies its pixel
     ;; density directly rather than upscaling a panel raster.
     (mcluv:command-menu-screen-state
      frame (viewer-logical-extent viewer))))
  instrument)

(defmethod release-viewer-instrument
    ((instrument viewer-command-menu-instrument) viewer)
  (declare (ignore viewer))
  (mcluv:destroy-command-menu
   (viewer-command-menu-frame instrument)))

(defun open-viewer-command-menu (viewer &key (title "LUFT M-x"))
  "Attach VIEWER's shared textureless M-x surface and return it.

The application vocabulary is captured here, outside paint.  Releasing
relative-pointer mode also makes the retained panel immediately clickable."
  (or (viewer-command-menu-attachment viewer)
      (let ((frame nil)
            (transferred-p nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (clear-viewer-controls viewer)
               (when (viewer-pointer-captured-p viewer)
                 (set-canvas-relative-pointer-mode
                  (viewer-canvas viewer) nil)
                 (setf (viewer-pointer-captured-p viewer) nil))
               (setf frame
                     (mcluv:make-embedded-command-menu
                      viewer
                      (viewer-canvas viewer)
                      (viewer-context viewer)
                      (viewer-device viewer)
                      :command-tables
                      (mcluv:command-menu-tables-for viewer)
                      :title title))
               (let* ((mirror (mcluv:command-menu-mirror frame))
                      (compositor
                        (make-instance 'mcluv:direct-gpu-mirror-compositor
                                       :mirror mirror))
                      (attachment
                        (make-instance 'viewer-command-menu-instrument
                                       :frame frame
                                       :compositor compositor)))
                 (setf (mcluv:mirror-compositor mirror) compositor)
                 (setf transferred-p t)
                 (add-viewer-instrument viewer attachment)
                 (setf completed-p t)
                 attachment))
          (unless completed-p
            (when (and frame (not transferred-p))
              (mcluv:destroy-command-menu frame)))))))

(defun close-viewer-command-menu (viewer)
  "Detach and release VIEWER's M-x surface, if present."
  (alexandria:when-let
      ((attachment (viewer-command-menu-attachment viewer)))
    ;; REMOVE publishes absence before RELEASE destroys the McCLIM frame and
    ;; compositor buffers against the still-live viewer device.
    (remove-viewer-instrument viewer attachment))
  nil)

(defun toggle-viewer-command-menu (viewer)
  (if (viewer-command-menu-present-p viewer)
      (close-viewer-command-menu viewer)
      (open-viewer-command-menu viewer))
  t)

(defun apply-viewer-command-menu-action
    (viewer attachment action command)
  (case action
    (:dismiss
     (close-viewer-command-menu viewer))
    (:execute
     (mcluv:execute-command-menu-command
      (viewer-command-menu-frame attachment) command
      :before-execute
      (lambda () (close-viewer-command-menu viewer)))))
  t)

(clim:define-command (com-execute-command
                      :command-table luft-atelier
                      :name "Execute Command"
                      :keystroke (#\x :meta))
    ()
  (toggle-viewer-command-menu (viewer-command-viewer)))

(defun viewer-command-menu-invocation (viewer event)
  "Return LUFT's M-x command form when EVENT names it."
  (let ((command
          (mcluv:canvas-key-event-command
           viewer event :command-table 'luft-atelier)))
    (and (consp command)
         (eq 'com-execute-command (first command))
         command)))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-command-menu-instrument)
     viewer canvas (event canvas-key-press-event))
  (declare (ignore canvas))
  (multiple-value-bind (action command)
      (mcluv:handle-command-menu-key-event
       (viewer-command-menu-frame instrument) event)
    (apply-viewer-command-menu-action
     viewer instrument action command)))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-command-menu-instrument)
     viewer canvas (event canvas-key-release-event))
  (declare (ignore viewer canvas event))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-command-menu-instrument)
     viewer canvas (event canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (let ((frame (viewer-command-menu-frame instrument)))
    (multiple-value-bind (x y)
        (mcluv:command-menu-local-coordinate
         frame
         (canvas-pointer-event-x event)
         (canvas-pointer-event-y event)
         (viewer-logical-extent viewer))
      (when x
        (multiple-value-bind (action command)
            (mcluv:handle-command-menu-pointer-press
             frame x y (canvas-pointer-event-button event))
          (apply-viewer-command-menu-action
           viewer instrument action command)))))
  ;; A modal click outside the panel must not recapture the game pointer.
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-command-menu-instrument)
     viewer canvas (event canvas-pointer-wheel-event))
  (declare (ignore instrument viewer canvas event))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-command-menu-instrument)
     viewer canvas (event canvas-pointer-motion-event))
  (declare (ignore instrument canvas))
  ;; Retain the absolute logical position without orbiting the camera.
  (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
        (viewer-pointer-y viewer) (canvas-pointer-event-y event))
  t)
