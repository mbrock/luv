;;; LUFT's attachment and authored fields for the shared global status line.

(in-package #:luft.render)

(defclass viewer-status-bar ()
  ((frame :initarg :frame :reader viewer-status-bar-frame)
   (compositor :initarg :compositor :reader viewer-status-bar-compositor)))

(defmethod viewer-instrument-priority ((instrument viewer-status-bar))
  (declare (ignore instrument))
  100)

(defmethod mcluv:status-bar-application-name ((viewer viewer))
  (declare (ignore viewer))
  "LUFT")

(defmethod mcluv:status-bar-lobby-client ((viewer viewer))
  (alexandria:when-let ((attachment (viewer-lobby-attachment viewer)))
    (viewer-lobby-client attachment)))

(defmethod mcluv:status-bar-channels-for ((viewer viewer))
  (declare (ignore viewer))
  (append (call-next-method) '(:bevel :view)))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :bevel)) (viewer viewer) bar)
  (declare (ignore channel bar))
  (viewer-bevel-label viewer))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :view)) (viewer viewer) bar)
  (declare (ignore channel viewer bar))
  (string-downcase (symbol-name *projection*)))

(defun viewer-status-bar-attachment (viewer)
  (find-if (lambda (instrument) (typep instrument 'viewer-status-bar))
           (viewer-instruments viewer)))

(defmethod refresh-viewer-instrument
    ((instrument viewer-status-bar) viewer)
  (let ((bar (viewer-status-bar-frame instrument))
        (width (first (viewer-logical-extent viewer))))
    (mcluv:refresh-status-bar bar width)
    (mcluv:prepare-status-bar bar))
  instrument)

(defmethod encode-viewer-instrument
    ((instrument viewer-status-bar)
     viewer pass surface-texture physical-extent)
  (declare (ignore physical-extent))
  (let ((bar (viewer-status-bar-frame instrument)))
    (mcluv:encode-direct-gpu-mirror
     (viewer-status-bar-compositor instrument)
     pass surface-texture
     (mcluv:status-bar-screen-state bar (viewer-logical-extent viewer))))
  instrument)

(defmethod release-viewer-instrument
    ((instrument viewer-status-bar) viewer)
  (declare (ignore viewer))
  (mcluv:destroy-status-bar (viewer-status-bar-frame instrument)))

(defun %open-viewer-status-bar (viewer)
  (or (viewer-status-bar-attachment viewer)
      (let ((bar nil)
            (instrument nil)
            (transferred-p nil)
            (completed-p nil))
        (unwind-protect
             (let* ((width (first (viewer-logical-extent viewer)))
                    (created-bar
                      (setf bar
                            (mcluv:make-embedded-status-bar
                             viewer
                             (viewer-canvas viewer)
                             (viewer-context viewer)
                             (viewer-device viewer)
                             width :title "LUFT status")))
                    (mirror (mcluv:status-bar-mirror created-bar))
                    (compositor
                      (make-instance 'mcluv:direct-gpu-mirror-compositor
                                     :mirror mirror)))
               (setf instrument
                     (make-instance 'viewer-status-bar
                                    :frame created-bar
                                    :compositor compositor)
                     (mcluv:mirror-compositor mirror) compositor)
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
            ;; ADD owns rejection cleanup after the transfer boundary.
            (when (and bar (not transferred-p))
              (ignore-errors (mcluv:destroy-status-bar bar))))))))

(defun open-viewer-status-bar (viewer)
  "Attach VIEWER's default top status line exactly once at a frame boundary."
  (luv:request-canvas-frame
   (viewer-canvas viewer)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%open-viewer-status-bar viewer))))
