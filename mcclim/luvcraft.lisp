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
   (normal-axis :initarg :normal-axis :reader widget-overlay-normal-axis)
   (height-scale :initarg :height-scale :initform 2.0
                 :reader widget-overlay-height-scale)
   (render-state :initform nil :accessor widget-overlay-render-state)
   (relief-layout :initform nil :accessor widget-overlay-relief-layout)
   (relief-vertex-module :initform nil
                         :accessor widget-overlay-relief-vertex-module)
   (relief-fragment-module :initform nil
                           :accessor widget-overlay-relief-fragment-module)
   (relief-pipeline :initform nil :accessor widget-overlay-relief-pipeline)))

(spv:define-shader widget-relief-world-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (homogeneous-texture-coordinate :vec3 :location 1)
              (shade-padding :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-texture-coordinate :vec2 :location 0)
               (render-shade :float :location 1)))
  (let* ()
    (spv:set-output
     clip-position
     (spv:vec4 position (spv:swizzle homogeneous-texture-coordinate :x)))
    (spv:set-output
     render-texture-coordinate
     (spv:swizzle homogeneous-texture-coordinate :yz))
    (spv:set-output render-shade
                    (spv:swizzle shade-padding :x))))

(spv:define-shader widget-relief-world-fragment-specification
    (:stage :fragment
     :inputs ((texture-coordinate :vec2 :location 0)
              (shade :float :location 1))
     :resources
     ((image :texture-2d :binding 0 :sample-transfer :identity)
      (texture-sampler :sampler :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((texel (spv:sample image texture-sampler texture-coordinate)))
    (spv:set-output
     color-output
     (spv:vec4 (* (spv:swizzle texel :rgb) shade)
               (spv:swizzle texel :a)))))

(defun clear-widget-overlay-relief-resources (overlay)
  (dolist (resource
            (list (widget-overlay-relief-pipeline overlay)
                  (widget-overlay-relief-fragment-module overlay)
                  (widget-overlay-relief-vertex-module overlay)))
    (when resource (luv:destroy resource)))
  (setf (widget-overlay-relief-layout overlay) nil
        (widget-overlay-relief-pipeline overlay) nil
        (widget-overlay-relief-fragment-module overlay) nil
        (widget-overlay-relief-vertex-module overlay) nil)
  overlay)

(defmethod release-raster-mirror-compositor :before
    ((overlay luvcraft-widget-overlay))
  (clear-widget-overlay-relief-resources overlay))

(defun ensure-widget-overlay-relief-pipeline (overlay format depth-format)
  (let ((layout (spinning-compositor-layout overlay)))
    (unless (and (eq layout (widget-overlay-relief-layout overlay))
                 (widget-overlay-relief-pipeline overlay))
      (clear-widget-overlay-relief-resources overlay)
      (let* ((device (spinning-compositor-device overlay))
             (vertex
               (luv:create
                device
                (luv:make-shader-module-descriptor
                 :label "McCLIM world relief vertex" :language :mathematical
                 :code (widget-relief-world-vertex-specification))))
             (fragment
               (luv:create
                device
                (luv:make-shader-module-descriptor
                 :label "McCLIM world relief fragment" :language :mathematical
                 :code (widget-relief-world-fragment-specification))))
             (pipeline
               (luv:create
                device
                (luv:make-render-pipeline-descriptor
                 :label "extruded McCLIM surface relief"
                 :layout layout
                 :vertex
                 `(:module ,vertex
                   :buffers
                   ((:array-stride 36
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)))))
                 :fragment
                 `(:module ,fragment
                   :targets ((:format ,format)))
                 :depth-stencil
                 (when depth-format
                   `(:format ,depth-format
                     :depth-write-enabled t :depth-compare :less))
                 :primitive '(:topology :triangle-list)))))
        (setf (widget-overlay-relief-layout overlay) layout
              (widget-overlay-relief-vertex-module overlay) vertex
              (widget-overlay-relief-fragment-module overlay) fragment
              (widget-overlay-relief-pipeline overlay) pipeline))))
  overlay)

(defun ensure-widget-relief-frame-buffer (overlay frame-state byte-count)
  (when (> byte-count (spinning-frame-state-relief-capacity frame-state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              (spinning-compositor-device overlay)
              (luv:make-buffer-descriptor
               :label "extruded McCLIM relief vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let
          ((old (spinning-frame-state-relief-buffer frame-state)))
        (luv:destroy old))
      (setf (spinning-frame-state-relief-buffer frame-state) replacement
            (spinning-frame-state-relief-capacity frame-state) capacity)))
  (spinning-frame-state-relief-buffer frame-state))

(defun world-device-clip-state (overlay session width height)
  "Return center, right, up, and normal clip vectors for OVERLAY's surface."
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
           16 :element-type 'single-float
           :initial-contents
           (mapcar
            (lambda (value) (coerce value 'single-float))
            (append
              (clip-vector (widget-overlay-center overlay) t)
              (clip-vector (widget-overlay-right-axis overlay) nil)
              (clip-vector (widget-overlay-up-axis overlay) nil)
              (clip-vector (widget-overlay-normal-axis overlay) nil)))))))))

(defun widget-overlay-reliefs (overlay)
  (let ((sheet (mirror-sheet (widget-overlay-mirror overlay))))
    (loop for child in (gpu-sheet-paint-order sheet)
          for medium = (gpu-sheet-presentation-medium child)
          when (typep medium 'luv-raster-medium)
            append (coerce (raster-medium-reliefs medium) 'list))))

(defun surface-relief-perimeter (relief &optional (corner-segments 5))
  (let* ((left (surface-relief-x1 relief))
         (top (surface-relief-y1 relief))
         (right (surface-relief-x2 relief))
         (bottom (surface-relief-y2 relief))
         (radius
           (min (surface-relief-radius relief)
                (* 0.5 (- right left)) (* 0.5 (- bottom top))))
         points)
    (dolist (corner
              (list (list (- right radius) (+ top radius) (* -0.5 pi))
                    (list (- right radius) (- bottom radius) 0.0)
                    (list (+ left radius) (- bottom radius) (* 0.5 pi))
                    (list (+ left radius) (+ top radius) pi)))
      (destructuring-bind (center-x center-y start-angle) corner
        (dotimes (index corner-segments)
          (let ((angle
                  (+ start-angle
                     (* (* 0.5 pi) (/ index corner-segments)))))
            (push (list (+ center-x (* radius (cos angle)))
                        (+ center-y (* radius (sin angle))))
                  points)))))
    (nreverse points)))

(defun append-widget-relief-vertex
    (vertices state width height pixel-height x y shade)
  (let* ((u (/ x width))
         (v (/ y height))
         (local-x (- (* 2.0 u) 1.0))
         (local-y (- (* 2.0 v) 1.0)))
    (flet ((push-value (value)
             (vector-push-extend (coerce value 'single-float) vertices)))
      (let ((clip
              (loop for lane below 4
                    collect
                    (+ (aref state lane)
                       (* local-x (aref state (+ 4 lane)))
                       (* local-y (aref state (+ 8 lane)))
                       (* pixel-height (aref state (+ 12 lane)))))))
        (dolist (value (subseq clip 0 3)) (push-value value))
        (push-value (fourth clip)))
      (push-value u)
      (push-value v)
      (push-value shade)
      (push-value 0.0)
      (push-value 0.0))))

(defun widget-overlay-relief-vertices (overlay state width height)
  "Build the small world-space mesh derived from OVERLAY's relief commands."
  (let ((vertices
          (make-array 256 :element-type 'single-float
                          :adjustable t :fill-pointer 0))
        (pixel-world-scale
          (* (widget-overlay-height-scale overlay)
             (/ (* 2.0 (vec:vec3-length (widget-overlay-right-axis overlay)))
                width))))
    (dolist (relief (widget-overlay-reliefs overlay))
      ;; Negative ink height remains an analytical recess. Geometry drops to
      ;; the base plane until the compositor can cut a matching surface hole.
      (let* ((world-height
               (* pixel-world-scale (max 0.0 (surface-relief-height relief))))
             (points (surface-relief-perimeter relief))
             (center-x
               (* 0.5
                  (+ (surface-relief-x1 relief)
                     (surface-relief-x2 relief))))
             (center-y
               (* 0.5
                  (+ (surface-relief-y1 relief)
                     (surface-relief-y2 relief)))))
        (when (plusp world-height)
          (loop for tail on points
                for point = (first tail)
                for next = (or (second tail) (first points))
                do
                   ;; The top reuses the exact McCLIM texture, so text and
                   ;; other overpaint move with the physical control.
                   (append-widget-relief-vertex
                    vertices state width height world-height
                    center-x center-y 1.0)
                   (append-widget-relief-vertex
                    vertices state width height world-height
                    (first point) (second point) 1.0)
                   (append-widget-relief-vertex
                    vertices state width height world-height
                    (first next) (second next) 1.0)
                   ;; Two side-wall triangles, shaded independently of the
                   ;; analytical top face.
                   (dolist (vertex
                             (list (list point 0.0) (list next 0.0)
                                   (list next world-height)
                                   (list point 0.0) (list next world-height)
                                   (list point world-height)))
                     (append-widget-relief-vertex
                      vertices state width height (second vertex)
                      (first (first vertex)) (second (first vertex)) 0.56))))))
    vertices))

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
       overlay (mirror-context mirror) source
       :depth-format :depth32-float
       :target-format
       (luv:gpu-texture-format
        (luvcraft::luvcraft-session-color-texture session)))
      (let* ((viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (state
               (world-device-clip-state
                overlay session (first viewport-size) (second viewport-size)))
             (frame-state
              (ensure-spinning-compositor-frame-state
               overlay surface-texture))
             (source-size (luv:gpu-texture-size source))
             (relief-vertices
               (widget-overlay-relief-vertices
                overlay state (first source-size) (second source-size))))
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
        (luv:draw pass 4)
        (when (plusp (length relief-vertices))
          (ensure-widget-overlay-relief-pipeline
           overlay (luv:gpu-texture-format surface-texture) :depth32-float)
          (let* ((byte-count (* 4 (length relief-vertices)))
                 (buffer
                   (ensure-widget-relief-frame-buffer
                    overlay frame-state byte-count)))
            (luv:write-buffer buffer relief-vertices)
            (luv:set-pipeline pass (widget-overlay-relief-pipeline overlay))
            (luv:set-bind-group
             pass 0 (spinning-frame-state-bind-group frame-state))
            (luv:set-vertex-buffer pass 0 buffer)
            (luv:draw pass (/ (length relief-vertices) 9)))))))
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
      ;; Project into the window's own coordinates rather than the drawable's.
      ;; A pointer event carries the position SDL reports, which is in logical
      ;; points; on a dense display the drawable is a multiple of that, and
      ;; hit-testing a point against a quad projected into pixels misses by
      ;; exactly the display's scale factor -- which is why no widget overlay
      ;; could be clicked at all on a Retina Mac.
      (let* ((canvas (luvcraft::luvcraft-session-canvas
                      (widget-overlay-session overlay)))
             (width (luv:canvas-width canvas))
             (height (luv:canvas-height canvas)))
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

(defun embed-luvcraft-frame
    (session frame &key (distance 4.0) (width 1.8) (right-offset 2.8))
  "Embed enabled McCLIM FRAME in front of SESSION's current camera."
  (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
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
               (vec:make-vec3 0.0 (- (/ width aspect 2.0)) 0.0)
               :normal-axis (vec:vec3-scale forward -1.0))))
        (setf (mirror-compositor mirror) overlay)
        (luvcraft:add-luvcraft-overlay session overlay)
        overlay))))

(defun open-luvcraft-widget-lab
    (session &key (title "McCLIM gadget inside luvcraft")
                  (distance 4.0) (width 1.8) (right-offset 2.8))
  "Create a McCLIM terminal fixed in front of SESSION's current world camera."
  (embed-luvcraft-frame
   session
   (open-widget-lab
    :title title
    :target (luvcraft:luvcraft-session-canvas session)
    :context (luvcraft::luvcraft-session-context session)
    :device (luvcraft::luvcraft-session-device session))
   :distance distance :width width :right-offset right-offset))

(defun open-luvcraft-surveyor-map
    (session &key (title "surveyor map")
                  (distance 3.2) (width 3.2) (right-offset 0.0))
  "Create and embed a live-world McCLIM surveyor map for SESSION."
  (embed-luvcraft-frame
   session
   (open-surveyor-map
    session :title title
    :target (luvcraft:luvcraft-session-canvas session)
    :context (luvcraft::luvcraft-session-context session)
    :device (luvcraft::luvcraft-session-device session))
   :distance distance :width width :right-offset right-offset))

(defun close-luvcraft-widget-lab (overlay)
  "Remove and release an OPEN-LUVCRAFT-WIDGET-LAB overlay."
  (check-type overlay luvcraft-widget-overlay)
  (luvcraft:remove-luvcraft-overlay
   (widget-overlay-session overlay) overlay)
  nil)

(defun close-luvcraft-surveyor-map (overlay)
  "Remove and release an OPEN-LUVCRAFT-SURVEYOR-MAP overlay."
  (close-luvcraft-widget-lab overlay))
