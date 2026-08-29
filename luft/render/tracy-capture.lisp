(in-package #:luft.render)

;;; LUFT owns one named controller directly. The shared controller owns every
;;; subprocess and concurrent transition; it never contributes a visual pane.

(defun viewer-tracy-capture (viewer)
  (sb-thread:with-mutex ((viewer-service-lock viewer))
    (viewer-tracy-capture-controller viewer)))

(defun ensure-viewer-tracy-capture (viewer)
  (or (sb-thread:with-mutex ((viewer-service-lock viewer))
        (viewer-tracy-capture-controller viewer))
      (let ((candidate
              (luv.tracy.capture:make-tracy-capture-controller
               :application-name "luft"
               :directory
               (merge-pathnames "build/tracy/"
                                (asdf:system-source-directory "luft"))))
            (winner nil))
        (unwind-protect-releasing
             (setf winner
                   (call-with-running-stop-controller
                    (viewer-stop-controller viewer)
                    (lambda ()
                      (sb-thread:with-mutex ((viewer-service-lock viewer))
                        (or (viewer-tracy-capture-controller viewer)
                            (setf (viewer-tracy-capture-controller viewer)
                                  candidate))))
                    :attachment candidate))
          (unless (eq winner candidate)
            (releasing :unpublished-tracy-controller
              (luv.tracy.capture:release-tracy-capture-controller candidate))))
        winner)))

(defun refresh-viewer-tracy-capture (viewer)
  "Name VIEWER's canvas lane once its active Tracy connection is ready."
  (let ((controller nil)
        (named-p nil))
    (sb-thread:with-mutex ((viewer-service-lock viewer))
      (setf controller (viewer-tracy-capture-controller viewer)
            named-p (viewer-tracy-canvas-thread-named-p viewer)))
    (when (and controller (not named-p)
               (luv.tracy.capture:tracy-capture-active-p controller)
               (tracy-connected-p))
      (name-tracy-thread "LUFT canvas")
      (sb-thread:with-mutex ((viewer-service-lock viewer))
        (when (eq controller (viewer-tracy-capture-controller viewer))
          (setf (viewer-tracy-canvas-thread-named-p viewer) t)))))
  viewer)

(defun release-viewer-tracy-capture (viewer)
  "Detach VIEWER's controller and asynchronously terminate its capture."
  (let ((controller nil))
    (sb-thread:with-mutex ((viewer-service-lock viewer))
      (setf controller (viewer-tracy-capture-controller viewer)
            (viewer-tracy-capture-controller viewer) nil
            (viewer-tracy-canvas-thread-named-p viewer) nil))
    (when controller
      (luv.tracy.capture:release-tracy-capture-controller controller)))
  nil)

(defun start-viewer-tracy-capture (viewer)
  "Start VIEWER's trace asynchronously and return its reserved pathname."
  (let ((controller (ensure-viewer-tracy-capture viewer)))
    (sb-thread:with-mutex ((viewer-service-lock viewer))
      (unless (eq controller (viewer-tracy-capture-controller viewer))
        (error "LUFT's Tracy controller detached while starting."))
      (setf (viewer-tracy-canvas-thread-named-p viewer) nil)
      (luv.tracy.capture:start-tracy-capture controller))))

(defun stop-viewer-tracy-capture (viewer)
  "Request VIEWER's graceful capture stop without waiting for finalization."
  (alexandria:when-let ((controller (viewer-tracy-capture viewer)))
    (luv.tracy.capture:stop-tracy-capture controller)))

(defun toggle-viewer-tracy-capture (viewer)
  "Atomically start or stop VIEWER's shared Tracy controller."
  (let ((controller (ensure-viewer-tracy-capture viewer)))
    (sb-thread:with-mutex ((viewer-service-lock viewer))
      (unless (eq controller (viewer-tracy-capture-controller viewer))
        (error "LUFT's Tracy controller detached while toggling."))
      (when (eq :idle
                (luv.tracy.capture:tracy-capture-state controller))
        (setf (viewer-tracy-canvas-thread-named-p viewer) nil))
      (luv.tracy.capture:toggle-tracy-capture controller))))

(defun open-viewer-tracy-capture (viewer &optional pathname)
  "Open PATHNAME or VIEWER's last trace without waiting for the Tracy GUI."
  (alexandria:when-let ((controller (viewer-tracy-capture viewer)))
    (luv.tracy.capture:open-tracy-capture controller pathname)))

(defun reveal-viewer-tracy-capture (viewer &optional pathname)
  "Reveal PATHNAME or VIEWER's last trace without waiting for Finder."
  (alexandria:when-let ((controller (viewer-tracy-capture viewer)))
    (luv.tracy.capture:reveal-tracy-capture controller pathname)))

(clim:define-command (com-toggle-tracy-capture
                      :command-table luft-window
                      :name "Toggle Tracy Capture"
                      :keystroke (:f9))
    ()
  (toggle-viewer-tracy-capture (viewer-command-viewer)))

(clim:define-command (com-start-tracy-capture
                      :command-table luft-window
                      :name "Start Tracy Capture")
    ()
  (start-viewer-tracy-capture (viewer-command-viewer)))

(clim:define-command (com-stop-tracy-capture
                      :command-table luft-window
                      :name "Stop Tracy Capture")
    ()
  (stop-viewer-tracy-capture (viewer-command-viewer)))

(clim:define-command (com-open-last-tracy-capture
                      :command-table luft-window
                      :name "Open Last Tracy Capture")
    ()
  (open-viewer-tracy-capture (viewer-command-viewer)))

(clim:define-command (com-reveal-last-tracy-capture
                      :command-table luft-window
                      :name "Reveal Last Tracy Capture")
    ()
  (reveal-viewer-tracy-capture (viewer-command-viewer)))
