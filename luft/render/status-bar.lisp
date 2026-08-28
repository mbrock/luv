;;; LUFT's semantic fields for the shared workbench status line.

(in-package #:luft.render)

(defmethod mcluv:status-bar-application-name ((viewer viewer))
  (declare (ignore viewer))
  "LUFT")

(defmethod mcluv:status-bar-lobby-client ((viewer viewer))
  (alexandria:when-let ((attachment (viewer-lobby-attachment viewer)))
    (viewer-lobby-client attachment)))

(defmethod mcluv:status-bar-channels-for ((viewer viewer))
  (declare (ignore viewer))
  (append (call-next-method) '(:bevel :view :mode)))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :bevel)) (viewer viewer) bar)
  (declare (ignore channel bar))
  (viewer-bevel-label viewer))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :view)) (viewer viewer) bar)
  (declare (ignore channel viewer bar))
  (string-downcase (symbol-name *projection*)))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :mode)) (viewer viewer) bar)
  (declare (ignore channel bar))
  (if (typep (viewer-mode viewer) 'world-edit-mode)
      (format nil "edit · ~A · ~A"
              (string-downcase
               (symbol-name
                (material-placement-name (viewer-edit-material viewer))))
              (string-downcase
               (symbol-name (or (viewer-last-edit-status viewer) :ready))))
      (if (typep (viewer-mode viewer) 'orbit-mode) "orbit" "play")))
