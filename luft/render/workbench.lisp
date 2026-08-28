(in-package #:luft.render)

;;; Minimal conformance fixture for 5H9AKZ. It attaches the generic empty shell
;;; to a live viewer without moving any existing Luft instrument or tool.

(defmethod luv.workbench:workbench-application-name ((viewer viewer))
  (declare (ignore viewer))
  "Luft")

(defmethod luv.workbench:workbench-application-command-frame ((viewer viewer))
  viewer)

(defmethod luv.workbench:workbench-application-canvas ((viewer viewer))
  (viewer-canvas viewer))

(defmethod luv.workbench:workbench-application-context ((viewer viewer))
  (viewer-context viewer))

(defmethod luv.workbench:workbench-application-stop-controller ((viewer viewer))
  (viewer-stop-controller viewer))

(defmethod luv.workbench:suspend-workbench-application-input ((viewer viewer))
  (clear-viewer-controls viewer)
  (when (viewer-pointer-captured-p viewer)
    (set-canvas-relative-pointer-mode (viewer-canvas viewer) nil)
    (setf (viewer-pointer-captured-p viewer) nil))
  ;; Resumption deliberately does not recapture the pointer. A later explicit
  ;; application press may acquire it under the viewer's ordinary mode policy.
  (list :luft-input-suspension viewer))

(defmethod luv.workbench:resume-workbench-application-input
    ((viewer viewer) token)
  (unless (and (consp token)
               (eq :luft-input-suspension (first token))
               (eq viewer (second token)))
    (error "~S is not ~S's workbench input token." token viewer))
  nil)

(defun open-viewer-workbench-shell-proof (&optional (viewer *viewer*))
  "Attach the empty production shell for a generic live-boundary proof."
  (or (luv.workbench:application-workbench viewer)
      (luv.workbench:start-workbench viewer)))

(defun close-viewer-workbench-shell-proof (&optional (viewer *viewer*))
  (alexandria:when-let ((shell (luv.workbench:application-workbench viewer)))
    (luv.workbench:stop-workbench shell)))
