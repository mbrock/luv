;;; A first McCLIM object inside luvcraft: a real gadget raster sampled by the
;;; block world's color pass.  The frame shares the world's native canvas and
;;; GPU device, but retains its own CLIM geometry and CPU raster.

(in-package #:mcluv)

(defclass luvcraft-widget-overlay (spinning-texture-compositor)
  ((session :initarg :session :reader widget-overlay-session)
   (frame :initarg :frame :reader widget-overlay-frame)
   (mirror :initarg :mirror :reader widget-overlay-mirror)))

(defmethod present-raster-mirror-texture
    ((mirror luv-raster-mirror) context texture
     (overlay luvcraft-widget-overlay))
  (declare (ignore mirror context texture overlay))
  ;; Upload is complete.  Luvcraft will sample this texture in its next frame.
  nil)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-widget-overlay) session pass surface-texture)
  (declare (ignore session))
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when source
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source :depth-format :depth32-float)
      (let ((frame-state
              (ensure-spinning-compositor-frame-state
               overlay surface-texture)))
        (luv:write-buffer
         (spinning-frame-state-buffer frame-state)
         (spinning-compositor-state
          overlay
          (float (/ (get-internal-real-time)
                    internal-time-units-per-second)
                 1.0d0)))
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group
         pass 0 (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay luvcraft-widget-overlay))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (eq :disowned (frame-state frame))
      (destroy-frame frame)))
  overlay)

(defun open-luvcraft-widget-lab
    (session &key (title "McCLIM gadget inside luvcraft") (speed 0.08))
  "Create a real McCLIM gadget frame sampled by SESSION's color pass."
  (let* ((frame
           (open-widget-lab
            :title title
            :target (luvcraft:luvcraft-session-canvas session)
            :context (luvcraft::luvcraft-session-context session)
            :device (luvcraft::luvcraft-session-device session)))
         (mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
         (overlay
           (make-instance 'luvcraft-widget-overlay
                          :session session :frame frame :mirror mirror
                          :speed speed)))
    (setf (mirror-compositor mirror) overlay)
    (luvcraft:add-luvcraft-overlay session overlay)
    overlay))

(defun close-luvcraft-widget-lab (overlay)
  "Remove and release an OPEN-LUVCRAFT-WIDGET-LAB overlay."
  (check-type overlay luvcraft-widget-overlay)
  (luvcraft:remove-luvcraft-overlay
   (widget-overlay-session overlay) overlay)
  nil)
