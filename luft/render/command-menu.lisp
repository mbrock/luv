(in-package #:luft.render)

;;; Luft contributes only its command vocabulary and command entry point.  The
;;; fixed pane, modality, focus, rendering, input, and release belong to the
;;; application workbench.

(defmethod mcluv:command-menu-tables-for ((viewer viewer))
  (declare (ignore viewer))
  '(luft-window luft-atelier))

(defun open-viewer-command-menu (viewer &key title)
  (declare (ignore title))
  (luv.workbench:open-workbench-command-menu
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(defun close-viewer-command-menu (viewer)
  (when-let
      ((workbench (luv.workbench:application-workbench viewer)))
    (luv.workbench:close-workbench-command-menu workbench)))

(defun toggle-viewer-command-menu (viewer)
  (luv.workbench:toggle-workbench-command-menu
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(clim:define-command (com-execute-command
                      :command-table luft-atelier
                      :name "Execute Command"
                      :keystroke (#\x :meta))
    ()
  (toggle-viewer-command-menu (viewer-command-viewer)))
