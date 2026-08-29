(in-package #:luft.render)

;;; Luft temporarily retains only the radio's service lifetime. The workbench
;;; owns the fixed modeless pane, layout, input, repaint, and GPU release.

(defclass viewer-lobby-instrument ()
  ((client :initarg :client :reader viewer-lobby-client)))

(defmethod viewer-instrument-priority ((instrument viewer-lobby-instrument))
  (declare (ignore instrument))
  10)

(defmethod viewer-instrument-present-p
    ((instrument viewer-lobby-instrument) viewer)
  (declare (ignore instrument viewer))
  nil)

(defmethod release-viewer-instrument
    ((instrument viewer-lobby-instrument) viewer)
  (declare (ignore viewer))
  (luv.lobby:stop-lobby-client (viewer-lobby-client instrument))
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

(defun attach-viewer-lobby (viewer)
  "Start VIEWER's shared radio once at a frame boundary, without its panel."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%attach-viewer-lobby viewer))))

(defun open-viewer-lobby (viewer)
  "Open the detailed panel at a frame boundary over the already-live radio."
  (attach-viewer-lobby viewer)
  (luv.workbench:open-workbench-lobby
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(defun close-viewer-lobby (viewer)
  "Hide the detailed panel at a frame boundary, preserving its radio client."
  (alexandria:when-let
      ((workbench (luv.workbench:application-workbench viewer)))
    (luv.workbench:close-workbench-lobby workbench)))

(defun toggle-viewer-lobby (viewer)
  "Toggle the detailed panel atomically at VIEWER's next frame boundary."
  (attach-viewer-lobby viewer)
  (luv.workbench:toggle-workbench-lobby
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(clim:define-command (com-toggle-lobby-panel
                      :command-table luft-atelier
                      :name "Toggle Lobby Panel")
    ()
  (toggle-viewer-lobby (viewer-command-viewer)))
