(in-package #:luft.render)

;;; Luft supplies application boundaries and semantic status values to the one
;;; Luv-owned workbench. The shell itself is attached by ordinary startup.

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
