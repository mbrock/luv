;;; A first McCLIM object inside luvcraft: a real gadget raster sampled by the
;;; block world's color pass.  The frame shares the world's native canvas and
;;; GPU device, but retains its own CLIM geometry and CPU raster.

(in-package #:mcluv)

(defclass luvcraft-widget-overlay (spinning-texture-compositor)
  ((session :initarg :session :reader widget-overlay-session)
   (frame :initarg :frame :reader widget-overlay-frame)
   (mirror :initarg :mirror :reader widget-overlay-mirror)
   (render-state :initform nil :accessor widget-overlay-render-state)))

(defconstant +lisp-machine-screen-scale+ 0.68)
(defconstant +lisp-machine-screen-orbit+ 0.12)
(defconstant +lisp-machine-screen-base-w+ 1.18)
(defconstant +lisp-machine-screen-perspective+ 0.48)

(defmethod present-raster-mirror-texture
    ((mirror luv-raster-mirror) context texture
     (overlay luvcraft-widget-overlay))
  (declare (ignore mirror context texture overlay))
  ;; Upload is complete.  Luvcraft will sample this texture in its next frame.
  nil)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-widget-overlay) session pass surface-texture)
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when source
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source :depth-format :depth32-float)
      (let* ((source-size (luv:gpu-texture-size source))
             (viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (aspect-scale
               (/ (/ (first source-size) (second source-size))
                  (/ (first viewport-size) (second viewport-size))))
             (state
               (spinning-compositor-state
                overlay
                (float (/ (get-internal-real-time)
                          internal-time-units-per-second)
                       1.0d0)
                :aspect-scale aspect-scale))
             (frame-state
              (ensure-spinning-compositor-frame-state
               overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer
         (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline
         pass (spinning-compositor-chassis-pipeline overlay))
        (luv:set-bind-group
         pass 0 (spinning-frame-state-bind-group frame-state))
        (dotimes (layer 3)
          (luv:draw pass 4 1 (* layer 4)))
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group
         pass 0 (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defun projected-screen-vertex (state width height u v)
  (let* ((sine (aref state 0))
         (cosine (aref state 1))
         (aspect (aref state 2))
         (center-x (- (* 2.0 u) 1.0))
         (center-y (- (* 2.0 v) 1.0))
         (clip-x
           (+ (* center-x +lisp-machine-screen-scale+ aspect cosine)
              (* sine +lisp-machine-screen-orbit+)))
         (clip-y (* center-y +lisp-machine-screen-scale+))
         (clip-w
           (+ +lisp-machine-screen-base-w+
              (* center-x sine +lisp-machine-screen-perspective+))))
    (list (* width 0.5 (+ 1.0 (/ clip-x clip-w)))
          (* height 0.5 (+ 1.0 (/ clip-y clip-w)))
          clip-w u v)))

(defun barycentric-coordinates (x y a b c)
  (destructuring-bind (ax ay &rest ignored-a) a
    (declare (ignore ignored-a))
    (destructuring-bind (bx by &rest ignored-b) b
      (declare (ignore ignored-b))
      (destructuring-bind (cx cy &rest ignored-c) c
        (declare (ignore ignored-c))
        (let ((denominator
                (+ (* (- by cy) (- ax cx))
                   (* (- cx bx) (- ay cy)))))
          (unless (zerop denominator)
            (let* ((wa
                     (/ (+ (* (- by cy) (- x cx))
                           (* (- cx bx) (- y cy)))
                        denominator))
                   (wb
                     (/ (+ (* (- cy ay) (- x cx))
                           (* (- ax cx) (- y cy)))
                        denominator))
                   (wc (- 1.0 wa wb)))
              (when (and (>= wa 0.0) (>= wb 0.0) (>= wc 0.0))
                (list wa wb wc)))))))))

(defun perspective-texture-coordinate (weights vertices)
  (let ((normalizer 0.0)
        (u 0.0)
        (v 0.0))
    (loop for weight in weights
          for vertex in vertices
          for reciprocal-w = (/ 1.0 (third vertex))
          for corrected = (* weight reciprocal-w)
          do (incf normalizer corrected)
             (incf u (* corrected (fourth vertex)))
             (incf v (* corrected (fifth vertex))))
    (values (/ u normalizer) (/ v normalizer))))

(defun luvcraft-widget-texture-coordinate (overlay x y)
  "Project canvas X,Y through OVERLAY's last rendered screen quadrilateral."
  (let ((state (widget-overlay-render-state overlay)))
    (when state
      (destructuring-bind (width height)
          (luv:canvas-extent
           (luvcraft::luvcraft-session-context
            (widget-overlay-session overlay)))
        (let* ((top-left
                 (projected-screen-vertex state width height 0.0 0.0))
               (top-right
                 (projected-screen-vertex state width height 1.0 0.0))
               (bottom-left
                 (projected-screen-vertex state width height 0.0 1.0))
               (bottom-right
                 (projected-screen-vertex state width height 1.0 1.0))
               (first-triangle
                 (list top-left top-right bottom-left))
               (second-triangle
                 (list bottom-left top-right bottom-right)))
          (or (alexandria:when-let
                  ((weights
                     (barycentric-coordinates
                      x y top-left top-right bottom-left)))
                (multiple-value-list
                 (perspective-texture-coordinate
                  weights first-triangle)))
              (alexandria:when-let
                  ((weights
                     (barycentric-coordinates
                      x y bottom-left top-right bottom-right)))
                (multiple-value-list
                 (perspective-texture-coordinate
                  weights second-triangle)))))))))

(defun translated-widget-pointer-event (event x y)
  (etypecase event
    (luv:canvas-pointer-motion-event
     (make-instance
      (class-of event)
      :timestamp (luv:canvas-event-timestamp event) :x x :y y
      :delta-x 0.0 :delta-y 0.0))
    (luv:canvas-pointer-button-event
     (make-instance
      (class-of event)
      :timestamp (luv:canvas-event-timestamp event) :x x :y y
      :button (luv:canvas-pointer-event-button event)
      :clicks (luv:canvas-pointer-event-clicks event)))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-widget-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore session))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (let* ((size
             (luv:gpu-texture-size
              (mirror-texture (widget-overlay-mirror overlay))))
           (x (* (first uv) (first size)))
           (y (* (second uv) (second size))))
      (luv:handle-canvas-event
       (widget-overlay-mirror overlay) canvas
       (translated-widget-pointer-event event x y))
      t)))

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
