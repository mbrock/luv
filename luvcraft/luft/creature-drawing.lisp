(in-package #:luft.render.shaders)

;;; Retain the original material/UV program verbatim. The host contributes
;;; only projection, jitter and velocity, not another surface implementation.
(define-live-shader creature-projection-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((host-clip :vec4 :built-in :position)
               (creature-current :vec4 :location 9)
               (creature-previous :vec4 :location 10))
     :resources ((camera-state :uniform-block :binding 7 :members #.*scene-uniform-members*)
                 (creature-positions :storage-buffer :binding 8 :element :vec4)))
  (let* ((base (* vertex-index (uint 2)))
         (world (assume-quantity (swizzle (buffer-element creature-positions base) :xyz)
                                 :quantity quantities:world-position :unit quantities:cell))
         (old (assume-quantity (swizzle (buffer-element creature-positions (+ base (uint 1))) :xyz)
                               :quantity quantities:world-position :unit quantities:cell))
         (divisor (swizzle (representation render-parameters) :z))
         (current (mesh-view-clip world camera-position camera-right camera-up
                                  camera-forward camera-projection divisor))
         (previous (mesh-view-clip old previous-camera-position previous-camera-right
                                   previous-camera-up previous-camera-forward previous-camera-projection divisor))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output host-clip
                (vec4 (+ (swizzle current :x) (* (swizzle jitter :x) (swizzle current :w)))
                      (+ (swizzle current :y) (* (swizzle jitter :y) (swizzle current :w)))
                      (swizzle current :z) (swizzle current :w)))
    (set-output creature-current current)
    (set-output creature-previous previous)))

(define-live-shader creature-motion-specification
    (:stage :fragment
     :inputs ((creature-current :vec4 :location 9) (creature-previous :vec4 :location 10))
     :outputs ((creature-motion :vec2 :location 1)))
  (set-output creature-motion (mesh-temporal-motion creature-previous creature-current)))

(in-package #:luvcraft.luft.game)

(defun creature-shader-specification (stage temporal-p)
  "Join the old surface with the host's projection/velocity interface."
  (let* ((old (ecase stage
                (:vertex (luvcraft.shaders::block-world-vertex-specification))
                (:fragment (luvcraft.shaders::block-world-fragment-specification))))
         (extra (ecase stage
                  (:vertex (luft.render.shaders::creature-projection-specification))
                  (:fragment (and temporal-p (luft.render.shaders::creature-motion-specification))))))
    (if (null extra) old
        (flet ((position-p (output)
                 (eq :position (luv.shader:shader-interface-built-in output))))
          (make-instance
           'luv.shader:shader-specification :name 'creature-surface :stage stage
           :inputs (append (luv.shader:shader-specification-inputs old)
                           (luv.shader:shader-specification-inputs extra))
           :outputs (append (remove-if #'position-p (luv.shader:shader-specification-outputs old))
                            (luv.shader:shader-specification-outputs extra))
           :resources (append (luv.shader:shader-specification-resources old)
                              (luv.shader:shader-specification-resources extra))
           :bindings (append (luv.shader:shader-specification-bindings old)
                             (luv.shader:shader-specification-bindings extra))
           :statements
           (append (remove-if
                    (lambda (statement)
                      (and (typep statement 'luv.shader:shader-output-assignment)
                           (position-p (luv.shader:shader-assignment-output statement))))
                    (luv.shader:shader-specification-statements old))
                   (luv.shader:shader-specification-statements extra)))))))

;;; Keep the original creature mesh builder AND surface program. Only the
;;; frame ABI changes: these Y-up vertices sample Luft's actual sun map.
(defclass creature-figure-drawing (ball-figure-drawing)
  ((surface :accessor creature-surface)
   (shadow :accessor creature-shadow)
   (atlas :accessor creature-atlas)
   (normal-atlas :accessor creature-normal-atlas)
   (sampler :accessor creature-sampler)
   (comparison :accessor creature-comparison)))

(defclass creature-figure-binding (ball-figure-binding)
  ((source-buffer :initform nil :accessor creature-source-buffer)
   (vertices :accessor creature-vertices)
   (positions :accessor creature-positions)
   (uniforms :accessor creature-uniforms)
   (surface :accessor creature-surface-binding)
   (shadow :accessor creature-shadow-binding)
   (count :initform 0 :accessor creature-count)))

(defun make-creature-figure-drawing (device formats samples)
  (let ((drawing (change-class (make-ball-figure-drawing device formats samples)
                               'creature-figure-drawing)))
    (render::with-gpu-construction (drawing)
      (flet ((own (descriptor) (render::own-gpu-resource drawing device descriptor)))
        (setf (creature-sampler drawing)
              (own (luv:make-sampler-descriptor :min-filter :nearest :mag-filter :nearest))
              (creature-comparison drawing)
              (own (luv:make-sampler-descriptor :compare :less-or-equal)))
        (loop for pixels in (list (luvcraft::make-block-texture-atlas)
                                  (luvcraft::make-block-normal-atlas))
              for slot in '(atlas normal-atlas)
              for format in (list luvcraft::+block-atlas-texture-format+
                                   luvcraft::+block-normal-atlas-texture-format+)
              do (let* ((width (array-dimension pixels 1))
                        (height (array-dimension pixels 0))
                        (texture (own (luv:make-texture-descriptor
                                       :size (list width height) :dimensions :2d :format format
                                       :usage '(:copy-dst :texture-binding)))))
                   (luv:write-texture
                    (luv:device-queue device) (luv:make-texture-copy :texture texture) pixels
                    (luv:make-texture-data-layout :bytes-per-row (* width 4) :rows-per-image height)
                    (list width height))
                   (setf (slot-value drawing slot)
                         (own (luv:make-texture-view-descriptor :texture texture))))))
      (let ((buffers
              '((:array-stride 56
                 :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                              (:shader-location 1 :offset 12 :format :float32x3)
                              (:shader-location 2 :offset 24 :format :float32x3)
                              (:shader-location 3 :offset 36 :format :float32x3)
                              (:shader-location 4 :offset 48 :format :float32x2))))))
        (setf (creature-surface drawing)
              (render::own-gpu-object
               drawing
               (render:make-drawing-program
                device :label "original Luvcraft creature surfaces"
                :vertex (creature-shader-specification :vertex (> (length formats) 1))
                :fragment (creature-shader-specification :fragment (> (length formats) 1))
                :vertex-buffers buffers
                :targets (mapcar (lambda (format) `(:format ,format)) formats)
                :sample-count samples
                :depth-stencil '(:format :depth32-float :depth-write-enabled t :depth-compare :less)))
              (creature-shadow drawing)
              (render::own-gpu-object
               drawing
               (render:make-drawing-program
                device :label "original Luvcraft creatures in Luft sun map"
                :vertex (luvcraft.shaders::block-world-shadow-vertex-specification)
                :vertex-buffers buffers
                :depth-stencil '(:format :depth32-float :depth-write-enabled t :depth-compare :less))))))))

(defmethod render::make-scene-drawing-binding
    ((drawing creature-figure-drawing) device camera shadow sampler)
  (make-instance 'creature-figure-binding :inputs (list device camera shadow sampler)))

(defun creature-figure-vertices (data &optional previous-p)
  "Expand kind 4 through the original box emitter, including atlas UV scale.
Lane 19 is the original tile identity, not a sampled or averaged colour."
  (let ((vertices (make-array (* (/ (length data) render::+figure-scalar-count+) 36
                                  luvcraft::+block-mesh-floats-per-vertex+)
                              :element-type 'single-float :fill-pointer 0)))
    (loop for base from 0 below (length data) by render::+figure-scalar-count+
          when (= 4.0 (aref data (+ base 15))) do
            (let ((animal (make-instance
                           'luvcraft::turtle
                           :position (let ((pose (+ base (if previous-p 4 0))))
                                       (vec3:make-vec3 (- (aref data pose) 1024.0)
                                                       (aref data (+ pose 2))
                                                       (- (aref data (+ pose 1)) 1024.0)))
                           :yaw (let ((heading (+ base (if previous-p 10 8))))
                                  (atan (aref data heading) (aref data (1+ heading)))))))
              (luvcraft::emit-critter-box
               vertices animal 0.0 0.0 0.0
               (aref data (+ base 12)) (aref data (+ base 14)) (aref data (+ base 13))
               (luvcraft::block-atlas-tile-at-offset (round (aref data (+ base 19)))) 1.0 0.0)))
    vertices))

(defmethod render::upload-figure-instances :after
    ((drawing creature-figure-drawing) (binding creature-figure-binding) data)
  (when (and (plusp (length data))
             (not (eq (creature-source-buffer binding) (render::figure-binding-buffer binding))))
    (destructuring-bind (device camera shadow sampler) (render::figure-binding-inputs binding)
      (declare (ignore sampler))
      (flet ((own (descriptor) (render::own-gpu-resource binding device descriptor)))
        (setf (creature-vertices binding)
              (own (luv:make-buffer-descriptor
                    :size (* (render::figure-binding-capacity binding) 36 56)
                    :usage '(:vertex :copy-dst)))
              (creature-positions binding)
              (own (luv:make-buffer-descriptor
                    :size (* (render::figure-binding-capacity binding) 36 32)
                    :usage '(:storage :copy-dst)))
              (creature-uniforms binding)
              (own (luv:make-buffer-descriptor :size (* 76 4) :usage '(:uniform :copy-dst)))
              (creature-surface-binding binding)
              (render::own-gpu-object
               binding
               (render:make-program-binding
                (creature-surface drawing) device
                :block-atlas (creature-atlas drawing) :block-normal-atlas (creature-normal-atlas drawing)
                :block-sampler (creature-sampler drawing) :frame-state (creature-uniforms binding)
                :shadow-map shadow :shadow-sampler (creature-sampler drawing)
                :shadow-comparison-sampler (creature-comparison drawing)
                :camera-state camera :creature-positions (creature-positions binding)))
              (creature-shadow-binding binding)
              (render::own-gpu-object
               binding (render:make-program-binding
                        (creature-shadow drawing) device :frame-state (creature-uniforms binding)))
              (creature-source-buffer binding) (render::figure-binding-buffer binding)))))
  (let* ((vertices (creature-figure-vertices data))
         (previous (creature-figure-vertices data t))
         (positions (make-array (* (/ (length vertices) 14) 8) :element-type 'single-float)))
    (setf (creature-count binding) (/ (length vertices) luvcraft::+block-mesh-floats-per-vertex+))
    (loop for vertex from 0 below (length vertices) by 14
          for target from 0 by 8 do
            (loop for source in (list vertices previous) for base from target by 4 do
              (setf (aref positions base) (+ 1024.0 (aref source vertex))
                    (aref positions (+ base 1)) (+ 1024.0 (aref source (+ vertex 2)))
                    (aref positions (+ base 2)) (aref source (+ vertex 1))
                    (aref positions (+ base 3)) 0.0)))
    (when (plusp (length vertices))
      (luv:write-buffer (creature-vertices binding) vertices)
      (luv:write-buffer (creature-positions binding) positions))))

(defun creature-frame-uniforms (data)
  "Translate the current Luft camera/light ABI, never recompute another sun map."
  (let ((out (make-array 76 :element-type 'single-float :initial-element 0.0)))
    (flet ((copy-row (destination source)
             (replace out data :start1 (* destination 4) :start2 (* source 4) :end2 (* (1+ source) 4)))
           (direction (destination source)
             (loop for axis in '(0 2 1) for i from (* destination 4)
                   do (setf (aref out i) (aref data (+ (* source 4) axis))))))
      (direction 0 0)
      (decf (aref out 0) 1024.0)
      (decf (aref out 2) 1024.0)
      (loop for row from 1 to 3 do (direction row row))
      (copy-row 4 4)
      ;; Fog and surface weathering keep the original shader's source space.
      (setf (aref out 20) 0.0 (aref out 21) 180.0
            (aref out 22) (aref data 116) (aref out 23) (aref data 107))
      (direction 6 14)
      (setf (aref out 27) (aref data 103))
      (copy-row 7 15)
      (setf (aref out 31) (aref data 59))
      (copy-row 8 25)
      (copy-row 9 26)
      (copy-row 10 16)
      (copy-row 11 27)
      (setf (aref out 47) 0.0)
      (copy-row 12 22)
      ;; Old PCSS uses depth span and world/texel, derived from the exact rows.
      (flet ((row-length (row)
               (sqrt (loop for axis below 3
                           for value = (aref data (+ (* row 4) axis)) sum (* value value)))))
        (let ((radius (aref data 91)))
          (setf (aref out 51) 0.0
                (aref out 52) (/ 1.0 (row-length 20))
                (aref out 53) (/ (* 2.0 (aref data 88)) (row-length 18))
                (aref out 54) radius (aref out 55) radius)))
      (setf (aref out 56) (float (* luvcraft::+block-atlas-tile-size+
                                                    luvcraft::*block-atlas-tile-capacity*) 1.0))
      (loop for source from 18 to 21 for destination from 15 do
        (direction destination source)
        (setf (aref out (+ (* destination 4) 3))
              (+ (aref data (+ (* source 4) 3))
                 (* 1024.0 (+ (aref data (* source 4)) (aref data (+ (* source 4) 1))))))))
    out))

(defmethod render::upload-scene-drawing-environment
    ((drawing creature-figure-drawing) (binding creature-figure-binding) data)
  (when (plusp (creature-count binding))
    (luv:write-buffer (creature-uniforms binding) (creature-frame-uniforms data))))

(defun encode-creatures (drawing pass binding shadow-p)
  (when (plusp (creature-count binding))
    (luv:set-vertex-buffer pass 0 (creature-vertices binding))
    (render:encode-program
     (if shadow-p (creature-shadow drawing) (creature-surface drawing)) pass
     (if shadow-p (creature-shadow-binding binding) (creature-surface-binding binding))
     (luv:make-gpu-draw-command :vertex-count (creature-count binding)))))

(defmethod render::encode-scene-drawing :around
    ((drawing creature-figure-drawing) pass (binding creature-figure-binding))
  (let ((runs (render::figure-binding-runs binding)))
    (unwind-protect
         (progn
           (setf (render::figure-binding-runs binding) (remove 4 runs :key #'first))
           (call-next-method)
           (encode-creatures drawing pass binding nil))
      (setf (render::figure-binding-runs binding) runs))))

(defmethod render::encode-scene-drawing-shadow :after
    ((drawing creature-figure-drawing) pass (binding creature-figure-binding))
  (encode-creatures drawing pass binding t))
