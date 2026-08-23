;;; Luvcraft's attachment and semantic fields for the shared status line.

(in-package #:mcluv)

(defclass luvcraft-status-bar-overlay (luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-status-bar-overlay))
  (declare (ignore overlay))
  :hud)

(defmethod status-bar-application-name
    ((session luvcraft:luvcraft-session))
  (declare (ignore session))
  "luvcraft")

(defmethod status-bar-lobby-client
    ((session luvcraft:luvcraft-session))
  (luvcraft:luvcraft-session-lobby-client session))

(defmethod status-bar-channels-for
    ((session luvcraft:luvcraft-session))
  (declare (ignore session))
  (append (call-next-method) '(:chunks)))

(defmethod status-bar-channel-value
    ((channel (eql :chunks))
     (session luvcraft:luvcraft-session) (bar status-bar))
  (declare (ignore channel bar))
  (hash-table-count (luvcraft:luvcraft-session-chunk-products session)))

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-status-bar-overlay) session)
  (let ((bar (widget-overlay-frame overlay))
        (width (luv:canvas-width (luvcraft:luvcraft-session-canvas session))))
    (refresh-status-bar bar width)
    (prepare-status-bar bar))
  overlay)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-status-bar-overlay) session pass surface-texture)
  (declare (ignore pass))
  (let* ((bar (widget-overlay-frame overlay))
         (canvas (luvcraft:luvcraft-session-canvas session))
         (logical-extent (list (luv:canvas-width canvas)
                               (luv:canvas-height canvas))))
    (prepare-direct-widget-overlay
     overlay session surface-texture
     (status-bar-screen-state bar logical-extent)))
  overlay)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-status-bar-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-status-bar-overlay) session canvas
     (event luv:canvas-pointer-event))
  "Leave the read-only status line transparent to ordinary game input."
  (declare (ignore overlay session canvas event))
  nil)

(defmethod luvcraft:luvcraft-overlay-focus-insets
    ((overlay luvcraft-status-bar-overlay) session)
  (declare (ignore overlay session))
  (values 0.0 +status-bar-height+ 0.0 0.0))

(defun find-luvcraft-status-bar (session)
  (find-if (lambda (overlay)
             (typep overlay 'luvcraft-status-bar-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun %open-luvcraft-status-bar (session)
  (or (find-luvcraft-status-bar session)
      (let ((bar nil)
            (overlay nil)
            (transferred-p nil)
            (completed-p nil))
        (unwind-protect
             (let* ((canvas (luvcraft:luvcraft-session-canvas session))
                    (created-bar
                      (setf bar
                            (make-embedded-status-bar
                             session canvas
                             (luvcraft:luvcraft-session-context session)
                             (luvcraft:luvcraft-session-device session)
                             (luv:canvas-width canvas)
                             :title "luvcraft status")))
                    (mirror (status-bar-mirror created-bar)))
               (setf overlay
                     (make-instance 'luvcraft-status-bar-overlay
                                    :session session :frame created-bar
                                    :mirror mirror)
                     (mirror-compositor mirror) overlay)
               (setf transferred-p t)
               (luvcraft:add-luvcraft-overlay session overlay)
               (setf completed-p t)
               overlay)
          (unless completed-p
            (when (and overlay
                       (member overlay
                               (luvcraft:luvcraft-session-overlays session)
                               :test #'eq))
              (ignore-errors
               (luvcraft:remove-luvcraft-overlay
                session overlay)))
            ;; ADD owns rejection cleanup once ownership was transferred.
            (when (and bar (not transferred-p))
              (ignore-errors (destroy-status-bar bar))))))))

(defun open-luvcraft-status-bar (session)
  "Attach SESSION's shared top status line exactly once at a frame boundary."
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%open-luvcraft-status-bar session))))

(defmethod luvcraft:attach-luvcraft-hud :after
    ((session luvcraft:luvcraft-session))
  ;; The detailed lobby remains an explicitly opened tool.  Its radio is
  ;; already live, so the compact line can represent the same semantic state.
  (open-luvcraft-status-bar session))
