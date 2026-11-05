(in-package #:luvcraft.agent)

;;; FNT1 is the small, versioned runtime export of the Blender companion rigs.
;;; Authored skin matrices supply secondary motion. Rest chains in the .rig
;;; sidecar let the game solve its own legs against the actual terrain.
(defstruct companion-mesh vertices joints weights indices poses bone-count frame-count stride rig)
(defstruct companion-leg name bones hip knee ankle pole phase)
(defstruct companion-rig stance legs palm poses)
(defstruct companion-foot point heading from from-heading target target-heading
           (progress 1d0) swing-p distance-driven-p supported-p (stance-p t))
(defstruct companion-pose mesh palette feet position (pelvis 0d0) moving-p scale)

(defvar *companion-meshes* (make-hash-table))

(defparameter *avatar-stature* (/ 1.8 1.85)
  "Cells per figure unit of the 1.85-unit avatar a remote player wears: a player's 1.8 cells.")

(defun companion-stature (kind)
  (ecase kind
    (:gnome (gnome-stature)) (:cat (cat-stature)) (:fantus (fantus-stature))
    (:avatar (float *avatar-stature* 1.0))))

(defun companion-body-height (kind)
  (* (companion-stature kind)
     (ecase kind
       (:gnome (gnome-body-height)) (:cat (cat-figure-height)) (:fantus (fantus-figure-height))
       (:avatar 1.85))))

(defun companion-walk-speed (kind)
  (coerce (* *embodied-agent-walk-speed* (companion-stature kind)) 'double-float))

(defun companion-rim-light (kind)
  (float (ecase kind
           (:gnome luvcraft.shaders::*gnome-rim-light*)
           (:cat luvcraft.shaders::*cat-rim-light*)
           (:fantus luvcraft.shaders::*fantus-rim-light*)
           (:avatar luvcraft.shaders::*gnome-rim-light*)) 1.0))

(defun companion-mesh-path (kind)
  (asdf:system-relative-pathname
   :luvcraft (ecase kind
               (:fantus "luvcraft/fantus/fantus.fmesh")
               (:gnome "luvcraft/companions/gnome.fmesh")
               (:cat "luvcraft/companions/cat.fmesh")
               (:avatar "luvcraft/avatar/claude-avatar.fmesh"))))

(defun load-companion-rig (mesh-path bone-count)
  (let* ((*read-eval* nil)
         (path (make-pathname :type "rig" :defaults mesh-path))
         (data (with-open-file (stream path) (read stream)))
         (digest (format nil "~(~{~2,'0X~}~)" (coerce (sb-md5:md5sum-file mesh-path) 'list))))
    (assert (and (= 1 (getf data :version))
                 (= bone-count (getf data :bone-count))
                 (string= digest (getf data :mesh-md5))) ()
            "Stale IK rig ~A. Re-run luvcraft/agent/export-rig.py on its .blend." path)
    (make-companion-rig
     :stance (getf data :stance)
     ;; Still poses stored after the Walk loop, by name, as rest poses.
     :poses (getf data :poses)
     ;; Where a held thing sits: a bone and a point in the rest frame.
     :palm (let ((palm (getf data :palm)))
             (and palm (cons (getf palm :bone) (apply #'make-vec3 (getf palm :point)))))
     :legs (mapcar
            (lambda (leg)
              (assert (every (lambda (i) (<= 0 i (1- bone-count))) (getf leg :bones)))
              (make-companion-leg
               :name (getf leg :name) :bones (getf leg :bones) :phase (getf leg :phase)
               :hip (apply #'make-vec3 (getf leg :hip))
               :knee (apply #'make-vec3 (getf leg :knee))
               :ankle (apply #'make-vec3 (getf leg :ankle))
               :pole (apply #'make-vec3 (getf leg :pole))))
            (getf data :legs)))))

(defun load-companion-mesh (&optional (kind :fantus))
  (with-open-file (stream (companion-mesh-path kind)
                         :element-type '(unsigned-byte 8))
    (labels ((word ()
               (loop for shift from 0 below 32 by 8
                     sum (ash (read-byte stream) shift)))
             (scalar ()
               (let ((bits (word)))
                 (sb-kernel:make-single-float
                  (if (logbitp 31 bits) (- bits #x100000000) bits))))
             (floats (count)
               (let ((data (make-array count :element-type 'single-float)))
                 (dotimes (i count data) (setf (aref data i) (scalar))))))
      (assert (= (word) #x31544e46) () "Not a companion FNT1 mesh: ~A" kind)
      (let* ((vertex-count (word)) (index-count (word))
             (bone-count (word)) (frame-count (word)) (stride (scalar))
             (vertices (make-array (* vertex-count 9) :element-type 'single-float))
             (joints (make-array (* vertex-count 4) :element-type '(unsigned-byte 32)))
             (weights (make-array (* vertex-count 4) :element-type 'single-float))
             (indices (make-array index-count :element-type '(unsigned-byte 32))))
        (assert (and (plusp bone-count) (plusp frame-count) (plusp stride)
                     (zerop (mod index-count 3))))
        (dotimes (v vertex-count)
          (dotimes (i 9) (setf (aref vertices (+ (* v 9) i)) (scalar)))
          (dotimes (i 4)
            (let ((joint (word)))
              (assert (< joint bone-count))
              (setf (aref joints (+ (* v 4) i)) joint)))
          (dotimes (i 4) (setf (aref weights (+ (* v 4) i)) (scalar)))
          (assert (< (abs (- (loop for i below 4
                                  sum (aref weights (+ (* v 4) i))) 1.0)) 0.001)))
        (dotimes (i index-count)
          (let ((index (word)))
            (assert (< index vertex-count))
            (setf (aref indices i) index)))
        (let* ((rig (load-companion-rig (companion-mesh-path kind) bone-count))
               (poses (floats (* (+ 1 frame-count (length (companion-rig-poses rig)))
                                 bone-count 12))))
          (assert (null (read-byte stream nil nil)))
          (make-companion-mesh :vertices vertices :joints joints :weights weights
                               :indices indices :poses poses :bone-count bone-count
                               :frame-count frame-count :stride stride :rig rig))))))

(defun companion-rest-pose-index (mesh name)
  "Where MESH's rest pose NAME starts in its poses: 0, the Idle pose, for NIL."
  (if name
      (let ((position (position name (companion-rig-poses (companion-mesh-rig mesh)))))
        (assert position () "No ~S pose in this companion mesh." name)
        (* 12 (companion-mesh-bone-count mesh) (+ 1 (companion-mesh-frame-count mesh) position)))
      0))

(defgeneric companion-rest-pose (agent)
  (:documentation "The still pose AGENT's body rests in when it is not walking: NIL for Idle.")
  (:method (agent) (declare (ignore agent)) nil))

(defun companion-mesh (kind)
  (or (gethash kind *companion-meshes*)
      (setf (gethash kind *companion-meshes*) (load-companion-mesh kind))))

(defun update-companion-gait (agent seconds &optional (session (gnome-session agent)))
  "Advance the IK cycle only for actual horizontal travel; blend to rest at stops."
  (let* ((x (luvcraft::body-x agent)) (z (luvcraft::body-z agent))
         (kind (embodied-agent-save-kind agent))
         (previous (companion-previous-position agent))
         (distance (if previous (sqrt (+ (expt (- x (car previous)) 2)
                                         (expt (- z (cdr previous)) 2))) 0d0))
         ;; A placed/restored body must not sprint through the intervening world.
         (travel (if (and (plusp seconds) (< distance (* 12 seconds))) distance 0d0))
         (moving (> travel (* 0.02 seconds)))
         (blend (companion-walk-blend agent))
         (amount (coerce (- 1 (exp (* -14 seconds))) 'single-float)))
    (setf (companion-previous-position agent) (cons x z)
          (companion-walk-phase agent)
          (mod (+ (companion-walk-phase agent)
                  (/ travel (* (companion-stature kind)
                               (companion-mesh-stride (companion-mesh kind))))) 1d0)
          (companion-walk-blend agent) (+ blend (* amount (- (if moving 1.0 0.0) blend))))
    (advance-companion-kinematics
     agent session seconds travel
     (if moving (make-vec3 (/ (- x (car previous)) travel) 0 (/ (- z (cdr previous)) travel))
         (make-vec3 0 0 1)) moving))
  agent)

(defmethod luvcraft:advance-luvcraft-overlay :after ((agent embodied-agent) session seconds)
  (update-companion-gait agent seconds session))

(defun sample-companion-palette (mesh phase blend palette &optional (rest 0))
  "Sample the authored animation, blending from the rest pose at index REST;
live IK may replace legs before skinning."
  (declare (optimize (speed 3))
           (type single-float blend)
           (type fixnum rest)
           (type (simple-array single-float (*)) palette))
  (let* ((poses (companion-mesh-poses mesh))
         (size (* 12 (companion-mesh-bone-count mesh)))
         (frame (* (mod phase 1d0) (companion-mesh-frame-count mesh)))
         (first (floor frame)) (fraction (coerce (- frame first) 'single-float))
         (a (* size (1+ first)))
         (b (* size (1+ (mod (1+ first) (companion-mesh-frame-count mesh))))))
    (declare (type (simple-array single-float (*)) poses))
    ;; Closely sampled rigid matrices interpolate smoothly, including the seam.
    (dotimes (i size)
      (let* ((idle (aref poses (+ rest i)))
             (walk (+ (aref poses (+ a i))
                      (* fraction (- (aref poses (+ b i)) (aref poses (+ a i)))))))
        (setf (aref palette i) (+ idle (* blend (- walk idle)))))))
  palette)

(defun sample-agent-palette (agent mesh palette)
  "AGENT's authored pose now: its walk, blended from the pose it rests in."
  (sample-companion-palette mesh (companion-walk-phase agent) (companion-walk-blend agent)
                            palette (companion-rest-pose-index mesh (companion-rest-pose agent))))

(defun skin-companion-palette (mesh palette output)
  "Write interleaved position, normal, colour and bind point into OUTPUT.
PALETTE and OUTPUT are caller-owned scratch arrays; no per-vertex allocation."
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) palette output))
  (let ((vertices (companion-mesh-vertices mesh))
        (joints (companion-mesh-joints mesh))
        (weights (companion-mesh-weights mesh)))
    (declare (type (simple-array single-float (*)) vertices weights)
             (type (simple-array (unsigned-byte 32) (*)) joints))
    (loop for v fixnum below (truncate (length vertices) 9)
          for source fixnum = (* v 9) for target fixnum = (* v 12)
          do (dotimes (axis 3)
               (let ((point 0.0) (normal 0.0))
                 (dotimes (influence 4)
                   (let* ((j (+ (* v 4) influence)) (weight (aref weights j))
                          (m (+ (* 12 (aref joints j)) (* 4 axis))))
                     (incf point
                           (* weight (+ (* (aref palette m) (aref vertices source))
                                        (* (aref palette (+ m 1)) (aref vertices (+ source 1)))
                                        (* (aref palette (+ m 2)) (aref vertices (+ source 2)))
                                        (aref palette (+ m 3)))))
                     (incf normal
                           (* weight (+ (* (aref palette m) (aref vertices (+ source 3)))
                                        (* (aref palette (+ m 1)) (aref vertices (+ source 4)))
                                        (* (aref palette (+ m 2)) (aref vertices (+ source 5))))))))
                 (setf (aref output (+ target axis)) point
                       (aref output (+ target 3 axis)) normal
                       (aref output (+ target 6 axis)) (aref vertices (+ source 6 axis))
                       (aref output (+ target 9 axis)) (aref vertices (+ source axis)))))))
  output)

(defun skin-companion-mesh (mesh phase blend palette output)
  "Preview authored poses without world contacts (also used by asset checks)."
  (sample-companion-palette mesh phase blend palette)
  (skin-companion-palette mesh palette output))

(defconstant +companion-vertex-floats+ 17
  "Rest position, normal, albedo, four joint indices and four weights.")

(defun companion-mesh-vertex-data (mesh)
  "MESH's static GPU vertices; the vertex shader skins them with the palette."
  (let* ((vertices (companion-mesh-vertices mesh)) (joints (companion-mesh-joints mesh))
         (weights (companion-mesh-weights mesh)) (count (truncate (length vertices) 9))
         (data (make-array (* count +companion-vertex-floats+) :element-type 'single-float)))
    (dotimes (v count data)
      (let ((target (* v +companion-vertex-floats+)))
        (replace data vertices :start1 target :start2 (* v 9) :end2 (* (1+ v) 9))
        (dotimes (i 4)
          (setf (aref data (+ target 9 i)) (float (aref joints (+ (* v 4) i)) 1.0)
                (aref data (+ target 13 i)) (aref weights (+ (* v 4) i))))))))

(defclass companion-body-overlay ()
  ((agent :initarg :agent :reader companion-body-agent)
   (pipeline :initarg :pipeline :accessor companion-body-pipeline)
   (layout :initarg :layout :accessor companion-body-layout)
   (indices :initarg :indices :accessor companion-body-indices)
   (vertex-buffer :initarg :vertex-buffer :accessor companion-body-vertex-buffer)
   (mesh :initarg :mesh :reader companion-body-mesh)
   (palette :initarg :palette :reader companion-body-palette)
   (instance :initform (make-array 8 :element-type 'single-float :initial-element 0.0)
             :reader companion-body-instance)
   (frames :initform (luv:make-canvas-frame-resource-cache) :reader companion-body-frames))
  (:documentation "Blender-authored indexed mesh with distance-driven skeletal animation."))

;;; One drawable's palette and instance buffers, and the bind group that pairs
;;; the palette with the host's uniform buffer for the same drawable.
(defstruct companion-body-frame palette instance group host-uniforms)

(defun release-companion-body-frame (frame)
  (luv:destroy (companion-body-frame-palette frame))
  (luv:destroy (companion-body-frame-instance frame))
  (when (companion-body-frame-group frame)
    (luv:destroy (companion-body-frame-group frame))))

(defun companion-body-visible-p (agent session)
  "Whether AGENT's body sphere meets SESSION's view frustum."
  (let ((camera (luvcraft:luvcraft-session-camera session)))
    (multiple-value-bind (x y z radius) (embodied-agent-body-sphere agent)
      (multiple-value-bind (right up forward) (luvcraft::camera-basis camera)
        (let* ((eye (luvcraft:camera-position camera))
               (dx (- x (vec3-x eye))) (dy (- y (vec3-y eye))) (dz (- z (vec3-z eye)))
               (extent (luv:canvas-extent (luvcraft:luvcraft-session-context session)))
               (tan-y (tan (/ (luvcraft:camera-field-of-view camera) 2)))
               (tan-x (* tan-y (/ (first extent) (max 1 (second extent))))))
          (flet ((along (axis) (+ (* dx (vec3-x axis)) (* dy (vec3-y axis)) (* dz (vec3-z axis)))))
            (let ((depth (along forward)))
              (and (> depth (- radius))
                   (<= (abs (along right)) (+ (* depth tan-x) (* radius (sqrt (+ 1 (* tan-x tan-x))))))
                   (<= (abs (along up)) (+ (* depth tan-y) (* radius (sqrt (+ 1 (* tan-y tan-y))))))))))))))

(defmethod luvcraft:luvcraft-focus-score ((overlay companion-body-overlay) session)
  (declare (ignore overlay session)) nil)

(defmethod luvcraft::luvcraft-overlay-live-shader-pipelines ((overlay companion-body-overlay))
  (list (companion-body-pipeline overlay)))

(defmethod luvcraft:refresh-luvcraft-overlay ((overlay companion-body-overlay) session)
  (declare (ignore session))
  (luv:refresh-live-shader-pipeline (companion-body-pipeline overlay)))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay companion-body-overlay) session pass surface-texture)
  (let* ((agent (companion-body-agent overlay)) (mesh (companion-body-mesh overlay))
         (kind (embodied-agent-save-kind agent)))
    (when (companion-body-visible-p agent session)
      (encode-companion-body overlay agent mesh kind session pass surface-texture))))

(defun companion-body-frame (overlay session surface-texture)
  "SURFACE-TEXTURE's frame-local companion buffers, bound to the host's uniforms."
  (let* ((device (luvcraft:luvcraft-session-device session))
         (palette (companion-body-palette overlay))
         (frame (luv:canvas-frame-resource
                 (companion-body-frames overlay) (luvcraft:luvcraft-session-context session)
                 surface-texture
                 (lambda (key target)
                   (declare (ignore key target))
                   (flet ((buffer (label floats usage)
                            (luv:create device (luv:make-buffer-descriptor
                                                :label label :size (* 4 floats) :usage usage))))
                     (make-companion-body-frame
                      :palette (buffer "Companion frame-local palette" (length palette)
                                       '(:storage :copy-dst))
                      :instance (buffer "Companion frame-local instance" 8
                                        '(:vertex :copy-dst)))))))
         (uniforms (sdf-agent-frame-uniform-buffer session surface-texture)))
    ;; The host may rebuild its frame state (a resized window) without telling
    ;; this cache; bind the uniform buffer it holds for this drawable now.
    (unless (eq uniforms (companion-body-frame-host-uniforms frame))
      (when (companion-body-frame-group frame)
        (luv:destroy (companion-body-frame-group frame)))
      (setf (companion-body-frame-group frame)
            (luv:create device (luv:make-bind-group-descriptor
                                :label "Companion skin bindings"
                                :layout (companion-body-layout overlay)
                                :entries `((:binding 2 :resource ,uniforms)
                                           (:binding 3 :resource ,(companion-body-frame-palette frame)))))
            (companion-body-frame-host-uniforms frame) uniforms))
    frame))

(defun encode-companion-body (overlay agent mesh kind session pass surface-texture)
  (let* ((instance (companion-body-instance overlay))
         (yaw (embodied-agent-facing-yaw agent))
         (pose (companion-kinematics agent))
         (palette (if (and pose (eq mesh (companion-pose-mesh pose)))
                      (companion-pose-palette pose)
                      (sample-agent-palette agent mesh (companion-body-palette overlay))))
         (frame (companion-body-frame overlay session surface-texture)))
    (setf (aref instance 0) (coerce (luvcraft::body-x agent) 'single-float)
          (aref instance 1) (coerce (luvcraft::body-y agent) 'single-float)
          (aref instance 2) (coerce (luvcraft::body-z agent) 'single-float)
          (aref instance 3) (companion-stature kind)
          (aref instance 4) (coerce (sin yaw) 'single-float)
          (aref instance 6) (coerce (cos yaw) 'single-float)
          (aref instance 7) (companion-rim-light kind))
    (luv:write-buffer (companion-body-frame-palette frame) palette)
    (luv:write-buffer (companion-body-frame-instance frame) instance)
    (luv:set-pipeline pass (luvcraft::live-shader-pipeline-native-pipeline
                           (companion-body-pipeline overlay)))
    (luv:set-vertex-buffer pass 0 (companion-body-vertex-buffer overlay))
    (luv:set-vertex-buffer pass 1 (companion-body-frame-instance frame))
    (luv:set-bind-group pass 0 (companion-body-frame-group frame))
    (luv:draw-indexed pass (companion-body-indices overlay) :uint32
                      (length (companion-mesh-indices mesh)))))

(defmethod luvcraft:evict-luvcraft-overlay-frame-key ((overlay companion-body-overlay) key)
  (luv:evict-canvas-frame-resource-key (companion-body-frames overlay) key #'release-companion-body-frame))

(defmethod luvcraft:release-luvcraft-overlay ((overlay companion-body-overlay))
  (luv:destroy-canvas-frame-resource-cache (companion-body-frames overlay) #'release-companion-body-frame)
  (when (companion-body-pipeline overlay)
    (luvcraft::release-live-shader-pipeline (companion-body-pipeline overlay))
    (setf (companion-body-pipeline overlay) nil))
  (when (companion-body-indices overlay)
    (luv:destroy (companion-body-indices overlay))
    (setf (companion-body-indices overlay) nil))
  (when (companion-body-vertex-buffer overlay)
    (luv:destroy (companion-body-vertex-buffer overlay))
    (setf (companion-body-vertex-buffer overlay) nil))
  (when (companion-body-layout overlay)
    (luv:destroy (companion-body-layout overlay))
    (setf (companion-body-layout overlay) nil))
  (when (eq (gnome-body (companion-body-agent overlay)) overlay)
    (setf (gnome-body (companion-body-agent overlay)) nil)))

(defun make-companion-mesh-body (agent)
  (let* ((session (gnome-session agent)) (device (luvcraft:luvcraft-session-device session))
         (mesh (companion-mesh (embodied-agent-save-kind agent)))
         (vertex-data (companion-mesh-vertex-data mesh))
         (indices nil) (vertices nil) (layout nil) (pipeline nil) (completed nil))
    (unwind-protect
         (progn
           (setf indices
                 (luv:create device (luv:make-buffer-descriptor
                                     :label "Companion triangles"
                                     :size (* 4 (length (companion-mesh-indices mesh)))
                                     :usage '(:index :copy-dst)))
                 vertices
                 (luv:create device (luv:make-buffer-descriptor
                                     :label "Companion rest vertices"
                                     :size (* 4 (length vertex-data))
                                     :usage '(:vertex :copy-dst)))
                 ;; The host's scene uniforms beside this body's bone palette.
                 layout
                 (luv:create device (luv:make-bind-group-layout-descriptor
                                     :label "Companion skin bindings"
                                     :entries '((:binding 2 :type :uniform-buffer)
                                                (:binding 3 :type :storage-buffer))))
                 pipeline
                 (luvcraft::make-live-shader-pipeline
                  :role :companion-mesh :vertex-role :companion-mesh :label "Companion mesh"
                  :device device :layout layout
                  :vertex-buffers
                  `((:array-stride ,(* 4 +companion-vertex-floats+)
                     :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                                  (:shader-location 1 :offset 12 :format :float32x3)
                                  (:shader-location 2 :offset 24 :format :float32x3)
                                  (:shader-location 3 :offset 36 :format :float32x4)
                                  (:shader-location 4 :offset 52 :format :float32x4)))
                    (:array-stride 32 :step-mode :instance
                     :attributes ((:shader-location 5 :offset 0 :format :float32x4)
                                  (:shader-location 6 :offset 16 :format :float32x4))))
                  :target-format luvcraft::+luvcraft-scene-color-format+
                  :primitive '(:topology :triangle-list :cull-mode :none)
                  :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                   :depth-compare :less)))
           (luv:write-buffer indices (companion-mesh-indices mesh))
           (luv:write-buffer vertices vertex-data)
           (let ((overlay (make-instance
                           'companion-body-overlay :agent agent :mesh mesh :indices indices
                           :vertex-buffer vertices :layout layout :pipeline pipeline
                           :palette (make-array (* 12 (companion-mesh-bone-count mesh))
                                                :element-type 'single-float))))
             (luvcraft:add-luvcraft-overlay session overlay)
             (setf (gnome-body agent) overlay completed t)
             overlay))
      (unless completed
        (when pipeline (luvcraft::release-live-shader-pipeline pipeline))
        (dolist (resource (list indices vertices layout))
          (when resource (luv:destroy resource)))))))
