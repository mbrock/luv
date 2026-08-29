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
  (append (call-next-method)
          '(:coordinates :chunks :stream :bevel :view :mode)))

(defun status-bar-position (position)
  (format nil "~,1F,~,1F,~,1F"
          (vec3:vec3-x position)
          (vec3:vec3-y position)
          (vec3:vec3-z position)))

(defun status-bar-position-chunk (position)
  (format nil "~D,~D"
          (floor (vec3:vec3-x position) luft:+chunk-size+)
          (floor (vec3:vec3-y position) luft:+chunk-size+)))

(defmethod mcluv:status-bar-channel-label
    ((channel (eql :coordinates)) (viewer viewer))
  (declare (ignore channel viewer))
  "xyz")

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :coordinates)) (viewer viewer) bar)
  (declare (ignore channel bar))
  (let ((camera (camera-position (viewer-camera viewer))))
    (if (viewer-player viewer)
        (format nil "p~A c~A"
                (status-bar-position
                 (walking-player-position (viewer-player viewer)))
                (status-bar-position camera))
        (format nil "c~A" (status-bar-position camera)))))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :chunks)) (viewer viewer) bar)
  (declare (ignore channel bar))
  (let ((scene (viewer-source viewer)))
    (when (typep scene 'streaming-scene)
      (let ((focus (streaming-scene-focus scene)))
        (format nil "p~A c~A f~A"
                (if (viewer-player viewer)
                    (status-bar-position-chunk
                     (walking-player-position (viewer-player viewer)))
                    "--")
                (status-bar-position-chunk
                 (camera-position (viewer-camera viewer)))
                (if focus
                    (format nil "~D,~D" (car focus) (cdr focus))
                    "--"))))))

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :stream)) (viewer viewer) bar)
  (declare (ignore channel bar))
  (let ((scene (viewer-source viewer)))
    (when (typep scene 'streaming-scene)
      (format nil "~Dv/~Dd/~Dr/~Dg/~D+~Dq"
              (hash-table-count (streaming-scene-loaded scene))
              (hash-table-count (streaming-scene-desired scene))
              (hash-table-count (streaming-scene-store scene))
              (if (viewer-renderer viewer)
                  (length (renderer-slot-order (viewer-renderer viewer)))
                  0)
              (hash-table-count (streaming-scene-load-outstanding scene))
              (hash-table-count (streaming-scene-outstanding scene))))))

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
