;;; Host ownership only: the shared adapter renders engine knob objects.
(in-package #:luft.render)

(defmethod luv.knobs:knobs-for-host ((viewer game-host))
  (remove-if-not (lambda (knob)
                   (let ((name (luv.knobs:knob-name knob)))
                     (and (member (symbol-package name)
                                  (list (find-package :luft.render)
                                        (find-package :luft.render.shaders)))
                          (case name
                            (jump-speed (viewer-player viewer))
                            (residency-radius
                             (typep (viewer-source viewer) 'streaming-scene))
                            (t t)))))
                 luv.knobs:*knobs*))

(defmethod mcluv:metabar-actions-for ((viewer game-host))
  (declare (ignore viewer))
  '(:reset-view :quit))

(defmethod mcluv:metabar-action-label
    ((action (eql :reset-view)) (viewer game-host))
  (declare (ignore action viewer))
  "reset view")

(defmethod mcluv:metabar-action-label
    ((action (eql :quit)) (viewer game-host))
  (declare (ignore action))
  (format nil "quit ~A" (luv.workbench:workbench-application-name viewer)))

(defmethod mcluv:perform-metabar-action
    ((action (eql :reset-view)) (viewer game-host))
  (declare (ignore action))
  (reset-viewer-camera viewer))

(defmethod mcluv:perform-metabar-action
    ((action (eql :quit)) (viewer game-host))
  (declare (ignore action))
  (request-viewer-quit viewer))

(defun close-viewer-metabar (viewer)
  (when-let ((workbench (luv.workbench:application-workbench viewer)))
    (luv.workbench:close-workbench-metabar workbench)))

(defun open-viewer-metabar (viewer &key (title "LUFT metabar"))
  (declare (ignore title))
  (luv.workbench:open-workbench-metabar
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(defun toggle-viewer-metabar (viewer)
  (luv.workbench:toggle-workbench-metabar
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(clim:define-command (com-toggle-metabar :command-table luft-window
                                        :name "Toggle Metabar"
                                        :keystroke (:return)) ()
  (toggle-viewer-metabar (viewer-command-viewer)))
