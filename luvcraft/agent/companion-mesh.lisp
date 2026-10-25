(in-package #:luvcraft.agent)

;;; FNT1 is the small, versioned runtime export of the Blender companion rigs.
;;; The editable armature and IK constraints stay in Blender. Its evaluated
;;; skin matrices are sampled by distance here, not by a free-running clock.
(defstruct companion-mesh vertices joints weights indices poses bone-count frame-count stride)

(defvar *companion-meshes* (make-hash-table))

(defun companion-stature (kind)
  (ecase kind (:gnome (gnome-stature)) (:cat (cat-stature)) (:fantus (fantus-stature))))

(defun companion-body-height (kind)
  (* (companion-stature kind)
     (ecase kind
       (:gnome (gnome-body-height)) (:cat (cat-figure-height)) (:fantus (fantus-figure-height)))))

(defun companion-walk-speed (kind)
  (coerce (* *embodied-agent-walk-speed* (companion-stature kind)) 'double-float))

(defun companion-rim-light (kind)
  (float (ecase kind
           (:gnome luvcraft.shaders::*gnome-rim-light*)
           (:cat luvcraft.shaders::*cat-rim-light*)
           (:fantus luvcraft.shaders::*fantus-rim-light*)) 1.0))

(defun load-companion-mesh (&optional (kind :fantus))
  (with-open-file (stream (asdf:system-relative-pathname
                          :luvcraft
                          (ecase kind
                            (:fantus "luvcraft/fantus/fantus.fmesh")
                            (:gnome "luvcraft/companions/gnome.fmesh")
                            (:cat "luvcraft/companions/cat.fmesh")))
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
        (let ((poses (floats (* (1+ frame-count) bone-count 12))))
          (assert (null (read-byte stream nil nil)))
          (make-companion-mesh :vertices vertices :joints joints :weights weights
                               :indices indices :poses poses :bone-count bone-count
                               :frame-count frame-count :stride stride))))))

(defun companion-mesh (kind)
  (or (gethash kind *companion-meshes*)
      (setf (gethash kind *companion-meshes*) (load-companion-mesh kind))))

(defun update-companion-gait (agent seconds)
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
          (companion-walk-blend agent) (+ blend (* amount (- (if moving 1.0 0.0) blend)))))
  agent)

(defmethod luvcraft:advance-luvcraft-overlay :after ((agent embodied-agent) session seconds)
  (declare (ignore session))
  (update-companion-gait agent seconds))

(defun skin-companion-mesh (mesh phase blend palette output)
  "Write interleaved position, normal, colour and bind point into OUTPUT.
PALETTE and OUTPUT are caller-owned scratch arrays; no per-vertex allocation."
  (declare (optimize (speed 3))
           (type single-float blend)
           (type (simple-array single-float (*)) palette output))
  (let* ((poses (companion-mesh-poses mesh))
         (vertices (companion-mesh-vertices mesh))
         (joints (companion-mesh-joints mesh))
         (weights (companion-mesh-weights mesh))
         (size (* 12 (companion-mesh-bone-count mesh)))
         (frame (* (mod phase 1d0) (companion-mesh-frame-count mesh)))
         (first (floor frame)) (fraction (coerce (- frame first) 'single-float))
         (a (* size (1+ first)))
         (b (* size (1+ (mod (1+ first) (companion-mesh-frame-count mesh))))))
    (declare (type (simple-array single-float (*)) poses vertices weights)
             (type (simple-array (unsigned-byte 32) (*)) joints))
    ;; Closely sampled rigid matrices interpolate smoothly, including the seam.
    (dotimes (i size)
      (let* ((idle (aref poses i))
             (walk (+ (aref poses (+ a i))
                      (* fraction (- (aref poses (+ b i)) (aref poses (+ a i)))))))
        (setf (aref palette i) (+ idle (* blend (- walk idle))))))
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

(defclass companion-body-overlay ()
  ((agent :initarg :agent :reader companion-body-agent)
   (pipeline :initarg :pipeline :accessor companion-body-pipeline)
   (indices :initarg :indices :accessor companion-body-indices)
   (mesh :initarg :mesh :reader companion-body-mesh)
   (palette :initarg :palette :reader companion-body-palette)
   (vertices :initarg :vertices :reader companion-body-vertices)
   (instance :initform (make-array 8 :element-type 'single-float :initial-element 0.0)
             :reader companion-body-instance)
   (frames :initform (luv:make-canvas-frame-resource-cache) :reader companion-body-frames))
  (:documentation "Blender-authored indexed mesh with distance-driven skeletal animation."))

(defun release-companion-body-frame (frame)
  (mapc #'luv:destroy frame))

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
         (kind (embodied-agent-save-kind agent))
         (vertices (companion-body-vertices overlay)) (instance (companion-body-instance overlay))
         (yaw (embodied-agent-facing-yaw agent))
         (frame (luv:canvas-frame-resource
                 (companion-body-frames overlay) (luvcraft:luvcraft-session-context session)
                 surface-texture
                 (lambda (key target)
                   (declare (ignore key target))
                   (loop for count in (list (length vertices) 8)
                         collect (luv:create
                                  (luvcraft:luvcraft-session-device session)
                                  (luv:make-buffer-descriptor
                                   :label "Companion frame-local skin" :size (* 4 count)
                                   :usage '(:vertex :copy-dst))))))))
    (skin-companion-mesh mesh (companion-walk-phase agent) (companion-walk-blend agent)
                         (companion-body-palette overlay) vertices)
    (setf (aref instance 0) (coerce (luvcraft::body-x agent) 'single-float)
          (aref instance 1) (coerce (luvcraft::body-y agent) 'single-float)
          (aref instance 2) (coerce (luvcraft::body-z agent) 'single-float)
          (aref instance 3) (companion-stature kind)
          (aref instance 4) (coerce (sin yaw) 'single-float)
          (aref instance 6) (coerce (cos yaw) 'single-float)
          (aref instance 7) (companion-rim-light kind))
    (luv:write-buffer (first frame) vertices)
    (luv:write-buffer (second frame) instance)
    (luv:set-pipeline pass (luvcraft::live-shader-pipeline-native-pipeline
                           (companion-body-pipeline overlay)))
    (luv:set-vertex-buffer pass 0 (first frame))
    (luv:set-vertex-buffer pass 1 (second frame))
    (luv:set-bind-group pass 0 (sdf-agent-scene-bind-group session surface-texture))
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
  (when (eq (gnome-body (companion-body-agent overlay)) overlay)
    (setf (gnome-body (companion-body-agent overlay)) nil)))

(defun make-companion-mesh-body (agent)
  (let* ((session (gnome-session agent)) (device (luvcraft:luvcraft-session-device session))
         (mesh (companion-mesh (embodied-agent-save-kind agent)))
         (indices nil) (pipeline nil) (completed nil))
    (unwind-protect
         (progn
           (setf indices
                 (luv:create device (luv:make-buffer-descriptor
                                     :label "Companion triangles"
                                     :size (* 4 (length (companion-mesh-indices mesh)))
                                     :usage '(:index :copy-dst)))
                 pipeline
                 (luvcraft::make-live-shader-pipeline
                  :role :companion-mesh :vertex-role :companion-mesh :label "Companion mesh"
                  :device device :layout (sdf-agent-pipeline-layout session)
                  :vertex-buffers
                  '((:array-stride 48
                     :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                                  (:shader-location 1 :offset 12 :format :float32x3)
                                  (:shader-location 2 :offset 24 :format :float32x3)
                                  (:shader-location 3 :offset 36 :format :float32x3)))
                    (:array-stride 32 :step-mode :instance
                     :attributes ((:shader-location 4 :offset 0 :format :float32x4)
                                  (:shader-location 5 :offset 16 :format :float32x4))))
                  :target-format luvcraft::+luvcraft-scene-color-format+
                  :primitive '(:topology :triangle-list :cull-mode :none)
                  :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                   :depth-compare :less)))
           (luv:write-buffer indices (companion-mesh-indices mesh))
           (let ((overlay (make-instance
                           'companion-body-overlay :agent agent :mesh mesh :indices indices
                           :pipeline pipeline
                           :palette (make-array (* 12 (companion-mesh-bone-count mesh))
                                                :element-type 'single-float)
                           :vertices (make-array (* 12 (/ (length (companion-mesh-vertices mesh)) 9))
                                                 :element-type 'single-float))))
             (luvcraft:add-luvcraft-overlay session overlay)
             (setf (gnome-body agent) overlay completed t)
             overlay))
      (unless completed
        (when indices (luv:destroy indices))
        (when pipeline (luvcraft::release-live-shader-pipeline pipeline))))))
