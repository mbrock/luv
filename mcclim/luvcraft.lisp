;;; A first McCLIM object inside luvcraft: a real gadget raster sampled by the
;;; block world's color pass.  The frame shares the world's native canvas and
;;; GPU device, but retains its own CLIM geometry and CPU raster.

(in-package #:mcluv)

(defclass luvcraft-widget-overlay (spinning-texture-compositor)
  ((session :initarg :session :reader widget-overlay-session)
   (frame :initarg :frame :reader widget-overlay-frame)
   (mirror :initarg :mirror :reader widget-overlay-mirror)
   (center :initarg :center :reader widget-overlay-center)
   (right-axis :initarg :right-axis :reader widget-overlay-right-axis)
   (up-axis :initarg :up-axis :reader widget-overlay-up-axis)
   (render-state :initform nil :accessor widget-overlay-render-state)))

(defun world-device-clip-state (overlay session width height)
  "Return center, right, and up clip vectors for OVERLAY in SESSION's camera."
  (let* ((camera (luvcraft:luvcraft-session-camera session))
         (uniforms (luvcraft:camera-uniform-data camera width height))
         (camera-position (luvcraft:camera-position camera)))
    (flet ((lane-vector (offset)
             (vec:make-vec3
              (aref uniforms offset)
              (aref uniforms (+ offset 1))
              (aref uniforms (+ offset 2))))
           (difference (left right)
             (vec:make-vec3
              (- (vec:vec3-x left) (vec:vec3-x right))
              (- (vec:vec3-y left) (vec:vec3-y right))
              (- (vec:vec3-z left) (vec:vec3-z right)))))
      (let ((right (lane-vector 4))
            (up (lane-vector 8))
            (forward (lane-vector 12))
            (x-scale (aref uniforms 16))
            (y-scale (aref uniforms 17))
            (z-scale (aref uniforms 18))
            (z-offset (aref uniforms 19)))
        (labels ((clip-vector (vector point-p)
                   (let* ((relative
                            (if point-p
                                (difference vector camera-position)
                                vector))
                          (view-x (vec:vec3-dot relative right))
                          (view-y (vec:vec3-dot relative up))
                          (view-z (vec:vec3-dot relative forward)))
                     (list (* view-x x-scale)
                           (- (* view-y y-scale))
                           (+ (* view-z z-scale)
                              (if point-p z-offset 0.0))
                           view-z))))
          (make-array
           12 :element-type 'single-float
           :initial-contents
           (mapcar
            (lambda (value) (coerce value 'single-float))
            (append
             (clip-vector (widget-overlay-center overlay) t)
             (clip-vector (widget-overlay-right-axis overlay) nil)
             (clip-vector (widget-overlay-up-axis overlay) nil)))))))))

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
      (let* ((viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (state
               (world-device-clip-state
                overlay session (first viewport-size) (second viewport-size)))
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
  (let* ((center-x (- (* 2.0 u) 1.0))
         (center-y (- (* 2.0 v) 1.0))
         (clip-x (+ (aref state 0)
                    (* center-x (aref state 4))
                    (* center-y (aref state 8))))
         (clip-y (+ (aref state 1)
                    (* center-x (aref state 5))
                    (* center-y (aref state 9))))
         (clip-w (+ (aref state 3)
                    (* center-x (aref state 7))
                    (* center-y (aref state 11)))))
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
    (when (and state
               (loop for u in '(0.0 1.0 0.0 1.0)
                     for v in '(0.0 0.0 1.0 1.0)
                     always (plusp
                             (third
                              (projected-screen-vertex state 1 1 u v)))))
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
      (when (and (typep event 'luv:canvas-pointer-button-press-event)
                 (eq :left (luv:canvas-pointer-event-button event)))
        (luvcraft:focus-luvcraft-session session overlay))
      (luv:handle-canvas-event
       (widget-overlay-mirror overlay) canvas
       (translated-widget-pointer-event event x y))
      t)))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-widget-overlay) session canvas
     (event luv:canvas-pointer-event))
  (luvcraft:handle-luvcraft-overlay-event overlay session canvas event))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-widget-overlay) session canvas
     (event luv:canvas-event))
  (declare (ignore session))
  (luv:handle-canvas-event (widget-overlay-mirror overlay) canvas event)
  t)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-widget-overlay) session)
  (declare (ignore session))
  (destructuring-bind (width height)
      (luv:canvas-extent
       (luvcraft::luvcraft-session-context
        (widget-overlay-session overlay)))
    (when (luvcraft-widget-texture-coordinate
           overlay (/ width 2.0) (/ height 2.0))
      0.0)))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay luvcraft-widget-overlay))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (eq :disowned (frame-state frame))
      (destroy-frame frame)))
  overlay)

(defun add-scaled-vector (origin &rest vector-scales)
  (let ((x (vec:vec3-x origin))
        (y (vec:vec3-y origin))
        (z (vec:vec3-z origin)))
    (loop for (vector scale) on vector-scales by #'cddr
          do (incf x (* (vec:vec3-x vector) scale))
             (incf y (* (vec:vec3-y vector) scale))
             (incf z (* (vec:vec3-z vector) scale)))
    (vec:make-vec3 x y z)))

(defun open-luvcraft-widget-lab
    (session &key (title "McCLIM gadget inside luvcraft")
                  (distance 4.0) (width 1.8) (right-offset 2.8))
  "Create a McCLIM terminal fixed in front of SESSION's current world camera."
  (let* ((frame
           (open-widget-lab
            :title title
            :target (luvcraft:luvcraft-session-canvas session)
            :context (luvcraft::luvcraft-session-context session)
            :device (luvcraft::luvcraft-session-device session)))
         (mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
         (source-size (luv:gpu-texture-size (mirror-texture mirror)))
         (aspect (/ (first source-size) (second source-size)))
         (camera (luvcraft:luvcraft-session-camera session))
         (camera-position (luvcraft:camera-position camera)))
    (multiple-value-bind (right ignored-up forward)
        (luvcraft:camera-basis camera)
      (declare (ignore ignored-up))
      (let ((overlay
              (make-instance
               'luvcraft-widget-overlay
               :session session :frame frame :mirror mirror
               :center
               (add-scaled-vector camera-position
                                  forward distance right right-offset)
               :right-axis (vec:vec3-scale right (/ width 2.0))
               :up-axis
               (vec:make-vec3 0.0 (- (/ width aspect 2.0)) 0.0))))
        (setf (mirror-compositor mirror) overlay)
        (luvcraft:add-luvcraft-overlay session overlay)
        overlay))))

(defun close-luvcraft-widget-lab (overlay)
  "Remove and release an OPEN-LUVCRAFT-WIDGET-LAB overlay."
  (check-type overlay luvcraft-widget-overlay)
  (luvcraft:remove-luvcraft-overlay
   (widget-overlay-session overlay) overlay)
  nil)
