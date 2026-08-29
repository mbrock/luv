(in-package #:luft.render)

;;; The viewer directly owns its radio service. The workbench independently
;;; owns the fixed modeless pane, layout, input, repaint, and GPU release.

(defun %attach-viewer-lobby (viewer)
  (or (sb-thread:with-mutex ((viewer-service-lock viewer))
        (viewer-lobby-client viewer))
      (let ((candidate
              (luv.lobby:start-lobby-client
               (luv.lobby:make-lobby-client :client-id-prefix "luft")))
            (winner nil))
        (unwind-protect-releasing
             (setf winner
                   (call-with-running-stop-controller
                    (viewer-stop-controller viewer)
                    (lambda ()
                      (sb-thread:with-mutex ((viewer-service-lock viewer))
                        (or (viewer-lobby-client viewer)
                            (setf (viewer-lobby-client viewer) candidate))))
                    :attachment candidate))
          (unless (eq winner candidate)
            (releasing :unpublished-lobby-radio
              (luv.lobby:stop-lobby-client candidate))))
        winner)))

(defun attach-viewer-lobby (viewer)
  "Start VIEWER's shared radio once at a frame boundary, without its panel."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%attach-viewer-lobby viewer))))

(defun release-viewer-lobby (viewer)
  "Detach and synchronously stop VIEWER's radio worker."
  (let ((client nil))
    (sb-thread:with-mutex ((viewer-service-lock viewer))
      (setf client (viewer-lobby-client viewer)
            (viewer-lobby-client viewer) nil))
    (when client (luv.lobby:stop-lobby-client client)))
  nil)

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
