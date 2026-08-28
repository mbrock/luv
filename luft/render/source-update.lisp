(in-package #:luft.render)

;;; LUFT attaches the shared source-update instrument to its viewer and names
;;; the ASDF root it wants assimilated.  Git workflow, painting, and live-load
;;; mechanics remain application-neutral.

(defclass viewer-source-update-instrument ()
  ((frame :initarg :frame :reader viewer-source-update-frame)
   (compositor :initarg :compositor :reader viewer-source-update-compositor)))

(defun viewer-source-update-attachment (viewer)
  (find-if (lambda (instrument)
             (typep instrument 'viewer-source-update-instrument))
           (viewer-instruments viewer)))

(defmethod viewer-instrument-priority
    ((instrument viewer-source-update-instrument))
  (declare (ignore instrument))
  900)

(defmethod refresh-viewer-instrument
    ((instrument viewer-source-update-instrument) viewer)
  (declare (ignore viewer))
  (let ((frame (viewer-source-update-frame instrument)))
    (mcluv:refresh-source-update frame)
    (mcluv:prepare-source-update frame))
  instrument)

(defmethod encode-viewer-instrument
    ((instrument viewer-source-update-instrument)
     viewer pass surface-texture physical-extent)
  (declare (ignore physical-extent))
  (let ((frame (viewer-source-update-frame instrument)))
    (mcluv:encode-direct-gpu-mirror
     (viewer-source-update-compositor instrument)
     pass surface-texture
     (mcluv:source-update-screen-state
      frame (viewer-logical-extent viewer))))
  instrument)

(defmethod release-viewer-instrument
    ((instrument viewer-source-update-instrument) viewer)
  (declare (ignore viewer))
  (mcluv:destroy-source-update (viewer-source-update-frame instrument)))

(defun luft-source-update-root ()
  "Return the checkout owned by this managed image."
  (uiop:ensure-directory-pathname
   (truename
    (or (and (boundp 'cl-user::*luv-project-root*)
             cl-user::*luv-project-root*)
        (uiop:pathname-directory-pathname
         (asdf:system-source-file :luft))))))

(defun open-viewer-source-update (viewer)
  "Attach LUFT's reviewed origin/main updater and begin fetching."
  (or (viewer-source-update-attachment viewer)
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
                     (mcluv:make-embedded-source-update
                      viewer
                      (viewer-canvas viewer)
                      (viewer-context viewer)
                      (viewer-device viewer)
                      (luft-source-update-root)
                      '("luft/render")
                      :title "LUFT source update"))
               (let* ((mirror (mcluv:source-update-mirror frame))
                      (compositor
                        (make-instance 'mcluv:direct-gpu-mirror-compositor
                                       :mirror mirror))
                      (instrument
                        (make-instance
                         'viewer-source-update-instrument
                         :frame frame :compositor compositor)))
                 (setf (mcluv:mirror-compositor mirror) compositor)
                 (setf transferred-p t)
                 (add-viewer-instrument viewer instrument)
                 (setf completed-p t)
                 instrument))
          (unless completed-p
            (when (and frame (not transferred-p))
              (mcluv:destroy-source-update frame)))))))

(defun close-viewer-source-update (viewer)
  "Close VIEWER's source updater unless it is applying an update."
  (alexandria:when-let
      ((instrument (viewer-source-update-attachment viewer)))
    (let* ((frame (viewer-source-update-frame instrument))
           (session (mcluv:source-update-frame-session frame)))
      (unless (mcluv:source-update-busy-p session)
        (remove-viewer-instrument viewer instrument))))
  nil)

(clim:define-command (com-review-source-update
                      :command-table luft-atelier
                      :name "Review Source Update")
    ()
  (open-viewer-source-update (viewer-command-viewer)))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-source-update-instrument)
     viewer canvas (event canvas-key-press-event))
  (declare (ignore canvas))
  (when (eq :dismiss
            (mcluv:handle-source-update-key-event
             (viewer-source-update-frame instrument) event))
    (close-viewer-source-update viewer))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-source-update-instrument)
     viewer canvas (event canvas-key-release-event))
  (declare (ignore instrument viewer canvas event))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-source-update-instrument)
     viewer canvas (event canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (let ((frame (viewer-source-update-frame instrument)))
    (multiple-value-bind (x y)
        (mcluv:source-update-local-coordinate
         frame
         (canvas-pointer-event-x event)
         (canvas-pointer-event-y event)
         (viewer-logical-extent viewer))
      (when x
        (mcluv:handle-source-update-pointer-press
         frame x y (canvas-pointer-event-button event)))))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-source-update-instrument)
     viewer canvas (event canvas-pointer-motion-event))
  (declare (ignore instrument canvas))
  (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
        (viewer-pointer-y viewer) (canvas-pointer-event-y event))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-source-update-instrument)
     viewer canvas (event canvas-pointer-wheel-event))
  (declare (ignore instrument viewer canvas event))
  t)

(defmethod handle-viewer-instrument-event
    ((instrument viewer-source-update-instrument)
     viewer canvas (event canvas-window-focus-lost-event))
  (declare (ignore instrument canvas event))
  (close-viewer-source-update viewer)
  t)
