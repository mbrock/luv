(in-package #:luft.render)

;;; LUFT contributes only application placement and lifecycle.  The radio and
;;; retained HUD remain shared components, while the central viewer-instrument
;;; dispatcher owns frame, input, and release ordering.

(defclass viewer-lobby-instrument ()
  ((client :initarg :client :reader viewer-lobby-client)
   (frame :initarg :frame :initform nil :accessor viewer-lobby-frame)
   (compositor :initarg :compositor :initform nil
               :accessor viewer-lobby-compositor)))

(defmethod viewer-instrument-priority ((instrument viewer-lobby-instrument))
  (declare (ignore instrument))
  10)

(defmethod viewer-instrument-present-p
    ((instrument viewer-lobby-instrument) viewer)
  (declare (ignore viewer))
  ;; A radio-only attachment must not make the renderer open an otherwise
  ;; empty overlay pass.  The worker remains alive independently of paint.
  (not (null (viewer-lobby-frame instrument))))

(defmethod refresh-viewer-instrument
    ((instrument viewer-lobby-instrument) viewer)
  (declare (ignore viewer))
  (when (viewer-lobby-frame instrument)
    (luv.lobby.mcclim:refresh-lobby-hud
     (viewer-lobby-frame instrument)))
  instrument)

(defmethod encode-viewer-instrument
    ((instrument viewer-lobby-instrument)
     viewer pass surface-texture physical-extent)
  (declare (ignore physical-extent))
  (when (viewer-lobby-frame instrument)
    (mcluv:encode-direct-gpu-mirror
     (viewer-lobby-compositor instrument)
     pass surface-texture
     (luv.lobby.mcclim:lobby-hud-screen-state
      (viewer-lobby-frame instrument)
      (viewer-logical-extent viewer))))
  instrument)

(defmethod release-viewer-instrument
    ((instrument viewer-lobby-instrument) viewer)
  (declare (ignore viewer))
  (let ((errors nil))
    ;; GPU ownership ends first while VIEWER's device is still live.  Network
    ;; shutdown is independent and bounded, and still runs if frame release
    ;; reports a problem.
    (handler-case
        (when (viewer-lobby-frame instrument)
          (luv.lobby.mcclim:destroy-lobby-hud
           (viewer-lobby-frame instrument))
          (setf (viewer-lobby-frame instrument) nil
                (viewer-lobby-compositor instrument) nil))
      (error (condition) (push (cons :hud condition) errors)))
    (handler-case
        (luv.lobby:stop-lobby-client (viewer-lobby-client instrument))
      (error (condition) (push (cons :radio condition) errors)))
    (when errors
      (error "LUFT lobby release failed in ~{~A~^, ~}: ~A"
             (mapcar #'car (reverse errors)) (cdar errors))))
  nil)

(defun viewer-lobby-attachment (viewer)
  (find-if (lambda (instrument)
             (typep instrument 'viewer-lobby-instrument))
           (viewer-instruments viewer)))

(defun %attach-viewer-lobby (viewer)
  (or (viewer-lobby-attachment viewer)
      (let ((client nil)
            (instrument nil)
            (transferred-p nil)
            (completed-p nil))
        (unwind-protect
             (progn
               ;; START only spawns the worker; broker discovery, connect,
               ;; subscribe, and reads all remain on that worker.
               (setf client
                     (luv.lobby:start-lobby-client
                      (luv.lobby:make-lobby-client
                       :client-id-prefix "luft")))
               (setf instrument
                     (make-instance
                      'viewer-lobby-instrument
                      :client client))
               (setf transferred-p t)
               (add-viewer-instrument viewer instrument)
               (setf completed-p t)
               instrument)
          (unless completed-p
            (when (and instrument
                       (member instrument (viewer-instruments viewer)
                               :test #'eq))
              (ignore-errors
               (remove-viewer-instrument viewer instrument)))
            (when (and client (not transferred-p))
              (ignore-errors
               (luv.lobby:stop-lobby-client client))))))))

(defun %open-viewer-lobby (viewer)
  (let ((instrument (%attach-viewer-lobby viewer)))
    (unless (viewer-lobby-frame instrument)
      (let ((frame nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (setf frame
                     (luv.lobby.mcclim:make-embedded-lobby-hud
                      (viewer-lobby-client instrument)
                      (viewer-canvas viewer)
                      (viewer-context viewer)
                      (viewer-device viewer)
                      :title "LUFT lobby"))
               (setf (viewer-lobby-frame instrument) frame
                     (viewer-lobby-compositor instrument)
                     (luv.lobby.mcclim:lobby-hud-compositor frame)
                     completed-p t))
          (unless completed-p
            (when frame
              (ignore-errors
               (luv.lobby.mcclim:destroy-lobby-hud frame)))
            (when (eq frame (viewer-lobby-frame instrument))
              (setf (viewer-lobby-frame instrument) nil
                    (viewer-lobby-compositor instrument) nil))))))
    instrument))

(defun %close-viewer-lobby (viewer)
  (alexandria:when-let ((instrument (viewer-lobby-attachment viewer)))
    (when (viewer-lobby-frame instrument)
      (luv.lobby.mcclim:destroy-lobby-hud
       (viewer-lobby-frame instrument))
      (setf (viewer-lobby-frame instrument) nil
            (viewer-lobby-compositor instrument) nil)))
  nil)

(defun attach-viewer-lobby (viewer)
  "Start VIEWER's shared radio once at a frame boundary, without its panel."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%attach-viewer-lobby viewer))))

(defun open-viewer-lobby (viewer)
  "Open the detailed panel at a frame boundary over the already-live radio."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%open-viewer-lobby viewer))))

(defun close-viewer-lobby (viewer)
  "Hide the detailed panel at a frame boundary, preserving its radio client."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%close-viewer-lobby viewer))))

(defun toggle-viewer-lobby (viewer)
  "Toggle the detailed panel atomically at VIEWER's next frame boundary."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (let ((instrument (viewer-lobby-attachment viewer)))
       (if (and instrument (viewer-lobby-frame instrument))
           (%close-viewer-lobby viewer)
           (%open-viewer-lobby viewer)))
     t)))

(clim:define-command (com-toggle-lobby-panel
                      :command-table luft-atelier
                      :name "Toggle Lobby Panel")
    ()
  (toggle-viewer-lobby (viewer-command-viewer)))
