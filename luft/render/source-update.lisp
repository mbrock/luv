(in-package #:luft.render)

;;; Luft contributes only the ASDF root and command entry point.  The fixed
;;; pane, source session, build run, modality, input, and release belong to the
;;; application workbench.

(defmethod mcluv:source-update-systems-for ((viewer viewer))
  (declare (ignore viewer))
  '("luft/render"))

(defmethod mcluv:source-update-title-for ((viewer viewer))
  (declare (ignore viewer))
  "LUFT source update")

(defun open-viewer-source-update (viewer)
  (luv.workbench:open-workbench-source-update
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(defun close-viewer-source-update (viewer)
  (alexandria:when-let
      ((workbench (luv.workbench:application-workbench viewer)))
    (luv.workbench:close-workbench-source-update workbench)))

(clim:define-command (com-review-source-update
                      :command-table luft-atelier
                      :name "Review Source Update")
    ()
  (open-viewer-source-update (viewer-command-viewer)))
