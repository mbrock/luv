(in-package #:luft.render)

;;; LUFT owns only the application attachment and command vocabulary.  The
;;; shared controller owns every subprocess and every concurrent transition.

(defclass viewer-tracy-capture-instrument ()
  ((controller :initarg :controller
               :reader viewer-tracy-capture-controller)
   (canvas-thread-named-p
    :initform nil
    :accessor viewer-tracy-canvas-thread-named-p)))

(defmethod viewer-instrument-priority
    ((instrument viewer-tracy-capture-instrument))
  (declare (ignore instrument))
  -1000)

(defmethod viewer-instrument-present-p
    ((instrument viewer-tracy-capture-instrument) viewer)
  (declare (ignore viewer))
  ;; Participate only during the short connection handshake.  Once the
  ;; canvas lane is named, this nonvisual attachment must not cause an empty
  ;; overlay render pass that would itself pollute the capture being measured.
  (and (not (viewer-tracy-canvas-thread-named-p instrument))
       (luv.tracy.capture:tracy-capture-active-p
        (viewer-tracy-capture-controller instrument))))

(defmethod refresh-viewer-instrument
    ((instrument viewer-tracy-capture-instrument) viewer)
  (declare (ignore viewer))
  (when (and (not (viewer-tracy-canvas-thread-named-p instrument))
             (tracy-connected-p))
    (name-tracy-thread "LUFT canvas")
    (setf (viewer-tracy-canvas-thread-named-p instrument) t))
  instrument)

(defmethod release-viewer-instrument
    ((instrument viewer-tracy-capture-instrument) viewer)
  (declare (ignore viewer))
  (luv.tracy.capture:release-tracy-capture-controller
   (viewer-tracy-capture-controller instrument)))

(defun viewer-tracy-capture-attachment (viewer)
  (find-if (lambda (instrument)
             (typep instrument 'viewer-tracy-capture-instrument))
           (viewer-instruments viewer)))

(defun ensure-viewer-tracy-capture-attachment (viewer)
  (or (viewer-tracy-capture-attachment viewer)
      (add-viewer-instrument
       viewer
       (make-instance
        'viewer-tracy-capture-instrument
        :controller
        (luv.tracy.capture:make-tracy-capture-controller
         :application-name "luft"
         :directory
         (merge-pathnames "build/tracy/"
                          (asdf:system-source-directory "luft")))))))

(defun start-viewer-tracy-capture (viewer)
  "Start VIEWER's trace asynchronously and return its reserved pathname."
  (let ((instrument (ensure-viewer-tracy-capture-attachment viewer)))
    (setf (viewer-tracy-canvas-thread-named-p instrument) nil)
    (luv.tracy.capture:start-tracy-capture
     (viewer-tracy-capture-controller instrument))))

(defun stop-viewer-tracy-capture (viewer)
  "Request VIEWER's graceful capture stop without waiting for finalization."
  (alexandria:when-let
      ((instrument (viewer-tracy-capture-attachment viewer)))
    (luv.tracy.capture:stop-tracy-capture
     (viewer-tracy-capture-controller instrument))))

(defun toggle-viewer-tracy-capture (viewer)
  "Atomically start or stop VIEWER's shared Tracy controller."
  (let ((instrument (ensure-viewer-tracy-capture-attachment viewer)))
    (when (eq :idle
              (luv.tracy.capture:tracy-capture-state
               (viewer-tracy-capture-controller instrument)))
      (setf (viewer-tracy-canvas-thread-named-p instrument) nil))
    (luv.tracy.capture:toggle-tracy-capture
     (viewer-tracy-capture-controller instrument))))

(defun open-viewer-tracy-capture (viewer &optional pathname)
  "Open PATHNAME or VIEWER's last trace without waiting for the Tracy GUI."
  (alexandria:when-let
      ((instrument (viewer-tracy-capture-attachment viewer)))
    (luv.tracy.capture:open-tracy-capture
     (viewer-tracy-capture-controller instrument) pathname)))

(defun reveal-viewer-tracy-capture (viewer &optional pathname)
  "Reveal PATHNAME or VIEWER's last trace without waiting for Finder."
  (alexandria:when-let
      ((instrument (viewer-tracy-capture-attachment viewer)))
    (luv.tracy.capture:reveal-tracy-capture
     (viewer-tracy-capture-controller instrument) pathname)))

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
