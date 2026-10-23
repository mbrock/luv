(in-package #:luvcraft.luft.game)

;;; A game's extension of the host's instanced figure stage. Real sphere
;;; triangles write depth; the original ball's procedural paint is reused.
(defclass ball-figure-drawing (render::figure-drawing)
  ((sphere-program :accessor ball-sphere-program)
   (particle-program :accessor particle-program)
   (shadow-program :accessor ball-shadow-program)))

(defclass ball-figure-binding (render::figure-binding)
  ((sphere-binding :accessor ball-sphere-binding)
   (particle-binding :accessor particle-binding)
   (shadow-binding :accessor ball-shadow-binding)
   (sphere-buffer :initform nil :accessor ball-sphere-buffer)))

(defun make-ball-figure-drawing (device formats samples)
  (let ((drawing (change-class (render:make-figure-drawing device formats samples)
                               'ball-figure-drawing)))
    (render::with-gpu-construction (drawing)
      (setf (ball-sphere-program drawing)
            (render::own-gpu-object
             drawing
             (render:make-drawing-program
              device :label "Luvcraft balls on Luft"
              :vertex (luft.render.shaders::ball-vertex-specification)
              :fragment (luft.render.shaders::ball-fragment-specification)
              :targets (mapcar (lambda (format) `(:format ,format)) formats)
              :sample-count samples
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))
            (particle-program drawing)
            (render::own-gpu-object
             drawing
             (render:make-drawing-program
              device :label "Luvcraft mining fragments"
              :vertex (luft.render.shaders::figure-box-vertex-specification)
              :fragment (luft.render.shaders::particle-fragment-specification)
              :targets (mapcar (lambda (format) `(:format ,format)) formats)
              :sample-count samples
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))
            (ball-shadow-program drawing)
            (render::own-gpu-object
             drawing
             (render:make-drawing-program
              device :label "Luvcraft ball shadows"
              :vertex (luft.render.shaders::ball-shadow-vertex-specification)
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))))))

(defmethod render::make-scene-drawing-binding
    ((drawing ball-figure-drawing) device camera shadow sampler)
  (make-instance 'ball-figure-binding :inputs (list device camera shadow sampler)))

(defmethod render::upload-figure-instances :after
    ((drawing ball-figure-drawing) (binding ball-figure-binding) data)
  (when (and (plusp (length data))
             (not (eq (ball-sphere-buffer binding) (render::figure-binding-buffer binding))))
    (destructuring-bind (device camera shadow sampler) (render::figure-binding-inputs binding)
      (declare (ignore shadow sampler))
      (setf (ball-sphere-binding binding)
            (render::own-gpu-object
             binding (render:make-program-binding
                      (ball-sphere-program drawing) device :camera-state camera
                      :figure-instances (render::figure-binding-buffer binding)))
            (particle-binding binding)
            (render::own-gpu-object
             binding (render:make-program-binding
                      (particle-program drawing) device :camera-state camera
                      :figure-instances (render::figure-binding-buffer binding)
                      :box-vertices (render::figure-box-vertices drawing)))
            (ball-shadow-binding binding)
            (render::own-gpu-object
             binding (render:make-program-binding
                      (ball-shadow-program drawing) device :camera-state camera
                      :figure-instances (render::figure-binding-buffer binding)))
            (ball-sphere-buffer binding) (render::figure-binding-buffer binding)))))

(defmethod render::encode-scene-drawing
    ((drawing ball-figure-drawing) pass (binding ball-figure-binding))
  (dolist (run (render::figure-binding-runs binding))
    (destructuring-bind (kind first count) run
      (render:encode-program
       (ecase kind (0 (render::scene-drawing-program drawing))
                   (1 (render::figure-box-program drawing)) (2 (ball-sphere-program drawing))
                   (3 (particle-program drawing)))
       pass
       (ecase kind (0 (render::figure-binding-program-binding binding))
                   (1 (render::figure-binding-box-binding binding)) (2 (ball-sphere-binding binding))
                   (3 (particle-binding binding)))
       (luv:make-gpu-draw-command :vertex-count (ecase kind (0 6) ((1 3) 36) (2 3072))
                                  :first-instance first :instance-count count)))))

(defmethod render::encode-scene-drawing-shadow
    ((drawing ball-figure-drawing) pass (binding ball-figure-binding))
  (dolist (run (render::figure-binding-runs binding))
    (destructuring-bind (kind first count) run
      (when (= kind 2)
        (render:encode-program
         (ball-shadow-program drawing) pass (ball-shadow-binding binding)
         (luv:make-gpu-draw-command :vertex-count 3072
                                    :first-instance first :instance-count count))))))

(in-package #:luft.render.shaders)

(define-shader-function ball-vertex-normal (index)
  (let* ((triangle (floor (/ index 6.0)))
         (corner (- index (* triangle 6.0)))
         (u (+ (- triangle (* (floor (/ triangle 32.0)) 32.0))
               (if (= corner 0.0) 0.0 (if (= corner 3.0) 0.0 (if (= corner 5.0) 0.0 1.0)))))
         (v (+ (floor (/ triangle 32.0))
               (if (= corner 0.0) 0.0 (if (= corner 1.0) 0.0 (if (= corner 3.0) 0.0 1.0)))))
         (longitude (* u (/ 6.2831853 32.0)))
         (latitude (- (* v (/ 3.14159265 16.0)) 1.5707963)))
    (vec3 (* (cos latitude) (cos longitude))
          (* (cos latitude) (sin longitude)) (sin latitude))))

(define-live-shader ball-shadow-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources ((camera-state :uniform-block :binding 0 :members #.*scene-uniform-members*)
                 (figure-instances :storage-buffer :binding 1 :element :vec4)))
  (let* ((pose (buffer-element figure-instances (* instance-index (uint 6))))
         (world (assume-quantity
                 (+ (swizzle pose :xyz) (* (ball-vertex-normal (float vertex-index)) (swizzle pose :w)))
                 :quantity quantities:world-position :unit quantities:cell)))
    (set-output clip-position
                (light-clip-position world shadow-row-x shadow-row-y shadow-row-z shadow-row-w))))

(define-live-shader ball-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (normal-output :vec3 :location 0)
               (spin-output :vec4 :location 1 :interpolation :flat)
               (tint-output :vec4 :location 2 :interpolation :flat)
               (pattern-output :float :location 3 :interpolation :flat)
               (current-output :vec4 :location 4)
               (previous-output :vec4 :location 5)
               (body-light-output :vec4 :location 6 :interpolation :flat))
     :resources ((camera-state :uniform-block :binding 0 :members #.*scene-uniform-members*)
                 (figure-instances :storage-buffer :binding 1 :element :vec4)))
  (let* ((base (* instance-index (uint 6)))
         (pose (buffer-element figure-instances base))
         (old (buffer-element figure-instances (+ base (uint 1))))
         (spin (buffer-element figure-instances (+ base (uint 2))))
         (shape (buffer-element figure-instances (+ base (uint 3))))
         (tint (buffer-element figure-instances (+ base (uint 4))))
         (normal (ball-vertex-normal (float vertex-index)))
         (world (assume-quantity (+ (swizzle pose :xyz) (* normal (swizzle pose :w)))
                                 :quantity quantities:world-position :unit quantities:cell))
         (old-world (assume-quantity (+ (swizzle old :xyz) (* normal (swizzle old :w)))
                                     :quantity quantities:world-position :unit quantities:cell))
         (divisor (swizzle (representation render-parameters) :z))
         (current (mesh-view-clip world camera-position camera-right camera-up
                                  camera-forward camera-projection divisor))
         (previous (mesh-view-clip old-world previous-camera-position previous-camera-right
                                   previous-camera-up previous-camera-forward previous-camera-projection divisor))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle current :x) (* (swizzle jitter :x) (swizzle current :w)))
                      (+ (swizzle current :y) (* (swizzle jitter :y) (swizzle current :w)))
                      (swizzle current :z) (swizzle current :w)))
    (set-output normal-output normal)
    (set-output spin-output spin)
    (set-output tint-output tint)
    (set-output pattern-output (swizzle shape :x))
    (set-output current-output current)
    (set-output body-light-output (buffer-element figure-instances (+ base (uint 5))))
    (set-output previous-output previous)))

(define-live-shader ball-fragment-specification
    (:stage :fragment
     :inputs ((normal-input :vec3 :location 0)
              (spin :vec4 :location 1 :interpolation :flat)
              (tint :vec4 :location 2 :interpolation :flat)
              (pattern :float :location 3 :interpolation :flat)
              (current :vec4 :location 4) (previous :vec4 :location 5)
              (body-light :vec4 :location 6 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0) (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0 :members #.*scene-uniform-members*)))
  (let* ((normal (normalize normal-input))
         (local (luvcraft.shaders::physics-sphere-unspin
                 (vec3 (swizzle normal :x) (swizzle normal :z) (swizzle normal :y)) spin))
         (base (swizzle tint :xyz))
         (albedo (if (< pattern 0.5) base
                     (if (< pattern 1.5) (luvcraft.shaders::physics-sphere-band-color base local)
                         (if (< pattern 2.5) (luvcraft.shaders::physics-sphere-marble-color base local)
                             (if (< pattern 3.5) (luvcraft.shaders::physics-sphere-beach-color local)
                                 (luvcraft.shaders::physics-sphere-smiley-color base local))))))
         (sun (representation (swizzle sun-vector :xyz)))
         (direct (* (smoothstep 0.90 1.0 (swizzle body-light :w))
                    (max 0.0 (/ (+ (dot normal sun) 0.30) 1.30))))
         (upness (swizzle normal :z))
         (light (+ (* (representation (swizzle sun-color-vector :xyz)) direct)
                   (* (representation (swizzle sky-color-vector :xyz))
                      (+ 0.15 (* 0.54 (+ 0.5 (* 0.5 upness)))))
                   (* (representation (swizzle ground-color-vector :xyz))
                      (+ 0.05 (* 0.34 (- 0.5 (* 0.5 upness))))))))
    (set-output color-output
                (vec4 (+ (* albedo (+ (* light (swizzle body-light :w) (swizzle body-light :w))
                                     (* voxel-light-gain (swizzle body-light :xyz) (swizzle body-light :xyz))))
                         (* albedo (swizzle tint :w))) 1.0))
    (set-output motion-output (mesh-temporal-motion previous current))))
