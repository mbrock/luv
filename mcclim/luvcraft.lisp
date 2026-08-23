;;; McCLIM command surfaces inside luvcraft. The frame shares the world's
;;; native canvas and GPU device; its mirror retains painter-ordered semantic
;;; commands and dense buffers which the world or HUD pass replays directly.

(in-package #:mcluv)

(defclass luvcraft-widget-overlay (direct-gpu-mirror-compositor)
  ((session :initarg :session :reader widget-overlay-session)
   (frame :initarg :frame :reader widget-overlay-frame)
   ;; Where the widget is in the world.  Written once for a widget fixed to
   ;; a wall, and each frame for one carried in the hand.
   (center :initarg :center :initform nil :accessor widget-overlay-center)
   (right-axis :initarg :right-axis :initform nil
               :accessor widget-overlay-right-axis)
   (up-axis :initarg :up-axis :initform nil :accessor widget-overlay-up-axis)
   (normal-axis :initarg :normal-axis :initform nil
                :accessor widget-overlay-normal-axis)))

(defclass luvcraft-direct-widget-overlay (luvcraft-widget-overlay)
  ((render-target-texture
    :initform nil :accessor widget-overlay-render-target-texture))
  (:documentation
   "A McCLIM surface whose ordered semantic commands enter its final pass.

Embedded mirrors retain dense command buffers rather than a pane-sized raster
texture. Solids, analytic paint, images, and Slug text are evaluated at the
actual destination pixel."))

(defclass luvcraft-world-widget-overlay (luvcraft-direct-widget-overlay) ()
  (:documentation "A direct McCLIM surface mounted in the 3D scene."))

(defclass luvcraft-hud-widget-overlay (luvcraft-direct-widget-overlay) ()
  (:documentation "A direct McCLIM surface mounted in the presentation HUD."))

(defgeneric luvcraft-widget-target-format
    (overlay session surface-texture)
  (:documentation
   "Return the application attachment format used to encode OVERLAY.

SURFACE-TEXTURE is NIL at the pre-pass refresh boundary and the concrete
drawable during replay."))

(defmethod luvcraft-widget-target-format
    ((overlay luvcraft-world-widget-overlay) session surface-texture)
  (declare (ignore overlay session surface-texture))
  luvcraft::+luvcraft-scene-color-format+)

(defmethod luvcraft-widget-target-format
    ((overlay luvcraft-hud-widget-overlay) session surface-texture)
  (declare (ignore overlay))
  (if surface-texture
      (luv:gpu-texture-format surface-texture)
      (luv:canvas-format
       (luvcraft::luvcraft-session-context session))))

(defgeneric luvcraft-widget-render-target
    (overlay session surface-texture)
  (:documentation
   "Return the actual texture attachment receiving OVERLAY this frame."))

(defmethod luvcraft-widget-render-target
    ((overlay luvcraft-world-widget-overlay) session surface-texture)
  (declare (ignore overlay surface-texture))
  (luvcraft::luvcraft-session-color-texture session))

(defmethod luvcraft-widget-render-target
    ((overlay luvcraft-hud-widget-overlay) session surface-texture)
  (declare (ignore overlay surface-texture))
  (luvcraft::luvcraft-session-presentation-texture session))

(defmethod luvcraft:refresh-luvcraft-overlay :after
    ((overlay luvcraft-direct-widget-overlay) session)
  ;; This refresh boundary precedes every game render pass.  A widget whose
  ;; McCLIM command stream did not change must nevertheless replace stale live
  ;; shader pipelines before ENCODE becomes replay-only.
  (prepare-gpu-mirror-compositor
   (widget-overlay-mirror overlay)
   :target-format (luvcraft-widget-target-format overlay session nil)
   :depth-stencil (direct-gpu-mirror-depth-stencil overlay)))

(eval-when (:load-toplevel :execute)
  ;; These method coordinates moved from the world-only class to the shared
  ;; direct compositor.  DEFMethod replaces one coordinate but cannot know
  ;; that an old coordinate has become obsolete in a durable image.
  (flet ((forget-method (name qualifiers specializer-names)
           (when (fboundp name)
             (let* ((generic (fdefinition name))
                    (specializers
                      (mapcar #'find-class specializer-names))
                    (method
                      (find-method generic qualifiers specializers nil)))
               (when method (remove-method generic method))))))
    (forget-method 'gpu-command-rasterized-p nil
                   '(luvcraft-world-widget-overlay
                     gpu-prepared-text-command))
    (forget-method 'gpu-command-rasterized-p nil
                   '(luvcraft-direct-widget-overlay
                     gpu-prepared-text-command))
    (forget-method 'gpu-command-rasterized-p nil
                   '(luvcraft-direct-widget-overlay t))
    (forget-method 'release-raster-mirror-compositor '(:before)
                   '(luvcraft-world-widget-overlay))
    (forget-method 'release-raster-mirror-compositor '(:before)
                   '(luvcraft-widget-overlay))
    (forget-method 'present-raster-mirror-texture nil
                   '(luv-raster-mirror t t luvcraft-widget-overlay))
    (forget-method 'set-direct-gpu-mirror-scissor nil
                   '(t luvcraft-direct-widget-overlay t t t))
    (forget-method 'luvcraft:encode-luvcraft-overlay '(:after)
                   '(luvcraft-world-widget-overlay t t t))))

(defmethod direct-gpu-mirror-depth-stencil
    ((overlay luvcraft-world-widget-overlay))
  (declare (ignore overlay))
  '(:format :depth32-float
    :depth-write-enabled nil :depth-compare :less))

(defmethod direct-gpu-mirror-depth-stencil
    ((overlay luvcraft-hud-widget-overlay))
  (declare (ignore overlay))
  nil)

(defmethod prepare-mirror-compositor-revision
    ((overlay luvcraft-world-widget-overlay) mirror revision)
  ;; Publication is itself a preparation boundary.  A world widget's final
  ;; attachment is the floating-point scene target, not its McCLIM canvas;
  ;; preparing the canvas format here would needlessly replace the scene
  ;; cohort before the ordinary pre-pass refresh restored it.
  (prepare-mirror-compositor-target-revision
   overlay mirror revision
   :target-format luvcraft::+luvcraft-scene-color-format+
   :depth-stencil (direct-gpu-mirror-depth-stencil overlay)))

(defun place-widget-overlay-on-surface (overlay display session)
  "Put OVERLAY where DISPLAY's surface is this frame.

Asked at draw time, because the surface may be a phone in a moving hand;
for a wall the answer is the same every frame and costs a few vector ops."
  (multiple-value-bind (center right-axis up-axis normal-axis)
      (luvcraft:terminal-surface-panel-frame
       (luvcraft:terminal-display-surface display) session)
    (setf (widget-overlay-center overlay) center
          (widget-overlay-right-axis overlay) right-axis
          (widget-overlay-up-axis overlay) up-axis
          (widget-overlay-normal-axis overlay) normal-axis))
  overlay)

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

(defun widget-overlay-logical-size (overlay)
  "Return OVERLAY's McCLIM surface size without requiring a backing texture."
  (multiple-value-list
   (gpu-mirror-logical-size (widget-overlay-mirror overlay))))

(luv:zdefun (prepare-direct-widget-overlay :zone :mcluv/prepare-overlay)
    (overlay session surface-texture state)
  "Publish STATE and return the destination frame's affine resources."
  (let* ((render-target
           (luvcraft-widget-render-target overlay session surface-texture))
         (target-format
           (luvcraft-widget-target-format overlay session render-target))
        (depth-stencil (direct-gpu-mirror-depth-stencil overlay)))
    (ensure-direct-gpu-mirror-pipelines
     overlay target-format depth-stencil)
    (let ((frame-state
            (ensure-direct-widget-frame-state overlay surface-texture)))
      (setf (widget-overlay-render-state overlay) state
            (widget-overlay-render-target-texture overlay) render-target)
      (luv:write-buffer (direct-widget-frame-buffer frame-state) state)
      frame-state)))

(defun ensure-luvcraft-widget-chassis-pipeline (overlay)
  "Return the opaque game-world housing pipeline for OVERLAY.

The housing is deliberately an application adapter. It is not part of the
shared semantic McCLIM compositor used by LUFT and other applications."
  (or (gethash :chassis (direct-widget-pipelines overlay))
      (let ((created nil) (completed-p nil))
        (unwind-protect
             (labels ((create (descriptor)
                        (let ((resource
                                (luv:create
                                 (direct-widget-device overlay) descriptor)))
                          (push resource created)
                          resource)))
               (let* ((vertex
                        (create
                         (luv:make-shader-module-descriptor
                          :label "Luvcraft McCLIM chassis vertex"
                          :language :mathematical
                          :code
                          (lisp-machine-chassis-vertex-specification))))
                      (fragment
                        (create
                         (luv:make-shader-module-descriptor
                          :label "Luvcraft McCLIM chassis fragment"
                          :language :mathematical
                          :code
                          (lisp-machine-chassis-fragment-specification))))
                      (pipeline
                        (create
                         (luv:make-render-pipeline-descriptor
                          :label "Luvcraft McCLIM terminal chassis"
                          :layout (direct-widget-shape-layout overlay)
                          :vertex `(:module ,vertex)
                          :fragment
                          `(:module ,fragment
                            :targets
                            ((:format ,(direct-widget-target-format overlay)
                              :blend :premultiplied-alpha)))
                          :depth-stencil
                          (direct-widget-depth-stencil overlay)
                          :primitive '(:topology :triangle-strip)))))
                 (setf (gethash :chassis (direct-widget-pipelines overlay))
                       pipeline
                       (direct-widget-resources overlay)
                       (nconc created (direct-widget-resources overlay))
                       completed-p t)
                 pipeline))
          (unless completed-p
            (dolist (resource created)
              (ignore-errors (luv:destroy resource))))))))


(luv:zdefmethod (luvcraft:encode-luvcraft-overlay :zone :mcluv/encode-chassis)
    ((overlay luvcraft-widget-overlay) session pass surface-texture)
  (let ((mirror (widget-overlay-mirror overlay)))
    (when (gpu-mirror-prepared-commands mirror)
      (let* ((viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (state
               (world-device-clip-state
                overlay session (first viewport-size) (second viewport-size)))
             (frame-state
               (prepare-direct-widget-overlay
                overlay session surface-texture state)))
        ;; The physical housing is game geometry; only the CLIM screen itself
        ;; comes from the retained semantic stream.
        (luv:set-pipeline
         pass (ensure-luvcraft-widget-chassis-pipeline overlay))
        (luv:set-bind-group
         pass 0 (direct-widget-frame-shape-bind-group frame-state))
        (dotimes (layer 3)
          (luv:draw pass 4 1 (* layer 4))))))
  overlay)

(defmethod luvcraft:encode-luvcraft-overlay :before
    ((overlay luvcraft-direct-widget-overlay) session pass surface-texture)
  (declare (ignore session pass surface-texture))
  ;; Primary methods set this only when they actually draw their backing
  ;; layer.  A hidden inventory or fully retracted metabar must not replay the
  ;; previous visible frame's text.
  (setf (widget-overlay-render-state overlay) nil
        (widget-overlay-render-target-texture overlay) nil))

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay
                 :zone :mcluv/encode-commands
                 :value
                 (length
                  (gpu-mirror-prepared-commands
                   (widget-overlay-mirror overlay))))
    :after
    ((overlay luvcraft-direct-widget-overlay) session pass surface-texture)
  "Replay OVERLAY's retained McCLIM commands in exact painter order."
  (alexandria:when-let ((state (widget-overlay-render-state overlay)))
    (encode-direct-gpu-mirror
     overlay pass (widget-overlay-render-target-texture overlay) state
     :frame-texture surface-texture))
  overlay)

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
                              (direct-mirror-projected-screen-vertex
                               state 1 1 u v)))))
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
                 (direct-mirror-projected-screen-vertex
                  state width height 0.0 0.0))
               (top-right
                 (direct-mirror-projected-screen-vertex
                  state width height 1.0 0.0))
               (bottom-left
                 (direct-mirror-projected-screen-vertex
                  state width height 0.0 1.0))
               (bottom-right
                 (direct-mirror-projected-screen-vertex
                  state width height 1.0 1.0))
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
    (let* ((size (widget-overlay-logical-size overlay))
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
  (let* ((canvas
           (luvcraft::luvcraft-session-canvas
            (widget-overlay-session overlay)))
         (width (luv:canvas-width canvas))
         (height (luv:canvas-height canvas)))
    ;; Pointer positions and this centre probe are both in SDL logical points.
    ;; The semantic renderer still targets the full drawable extent; mixing
    ;; those two coordinate spaces here displaced the focus ray on Retina.
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
         (source-size (multiple-value-list (gpu-mirror-logical-size mirror)))
         (aspect (/ (first source-size) (second source-size)))
         (camera (luvcraft:luvcraft-session-camera session))
         (camera-position (luvcraft:camera-position camera)))
    (multiple-value-bind (right ignored-up forward)
        (luvcraft:camera-basis camera)
      (declare (ignore ignored-up))
      (let ((overlay
              (make-instance
               'luvcraft-world-widget-overlay
               :session session :frame frame :mirror mirror
               :center
               (add-scaled-vector camera-position
                                  forward distance right right-offset)
               :right-axis (vec:vec3-scale right (/ width 2.0))
               :up-axis
               (vec:make-vec3 0.0 (- (/ width aspect 2.0)) 0.0)
               :normal-axis (vec:vec3-scale forward -1.0))))
        (setf (mirror-compositor mirror) overlay)
        ;; The frame was enabled before it acquired its world compositor.
        ;; Repaint once so text is retained for the scene pass but omitted
        ;; from the backing texture.
        (repaint-gpu-mirror mirror)
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
