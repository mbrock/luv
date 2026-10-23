;;;; Disposable grass/stone experiment; no production shader or HAL changes.
(in-package #:luft.render.shaders)

(defparameter *cache-resolution* (gpu-fps::env-integer "MATERIAL_RESOLUTION" 512))
(defparameter *cache-period* 4.0)
(defparameter *cache-levels*
  (loop with row = 0
        for size = *cache-resolution* then (floor size 2)
        collect (list size row)
        do (incf row (+ size 2))
        until (= size 1)))
(defparameter *cache-extent*
  (list (* 2 (+ *cache-resolution* 2))
        (reduce #'+ *cache-levels* :key (lambda (level) (+ (first level) 2)))))
(assert (and (>= *cache-resolution* 16)
             (zerop (logand *cache-resolution* (1- *cache-resolution*)))))

;; Numeric helpers contain no resource handles. Sampling is emitted at the
;; call site, as in the renderer's existing image-finishing shaders.
(eval `(define-shader-function cache-level-info (level)
         ,(loop for (size row) in (reverse *cache-levels*)
                for index downfrom (1- (length *cache-levels*))
                for form = `(vec2 ,(float size) ,(float row))
                  then `(if (< level ,(+ index 0.5)) (vec2 ,(float size) ,(float row)) ,form)
                finally (return form))))

(eval `(define-shader-function cache-uv (uv code level)
         (let* ((info (cache-level-info level))
                (size (swizzle info :x))
                (column (* (- code 1.0) (+ size 2.0))))
           (/ (+ (vec2 (+ column 1.0) (+ (swizzle info :y) 1.0))
                 (* (fract uv) size))
              (vec2 ,(float (first *cache-extent*)) ,(float (second *cache-extent*)))))))

(define-shader-function cache-periodic-height (code uv)
  ;; Four translated evaluations join with zero-slope blend weights at the
  ;; tile boundaries. The authored grass/stone functions remain the generator.
  (let* ((q (* (fract uv) 4.0))
         (blend (smoothstep (vec2 0.0 0.0) (vec2 1.0 1.0) (fract uv)))
         (a (material-relief-height code (vec3 q 0.371)))
         (b (material-relief-height code (vec3 (- q (vec2 4.0 0.0)) 0.371)))
         (c (material-relief-height code (vec3 (- q (vec2 0.0 4.0)) 0.371)))
         (d (material-relief-height code (vec3 (- q (vec2 4.0 4.0)) 0.371))))
    (mix (mix a b (swizzle blend :x))
         (mix c d (swizzle blend :x)) (swizzle blend :y))))

(define-shader-function cache-surface-height (raw pattern fade laid)
  (let* ((cut (* (swizzle laid :x)
                 (if (> (swizzle pattern :x) 4.5) 1.0 (swizzle laid :z))
                 (+ 0.75 (* 0.25 raw)))))
    (if (< (swizzle pattern :x) 0.5) raw (mix raw cut fade))))

(define-shader-function cache-relief-normal (normal world strength pattern fade raw gradient here)
  ;; Keep the original mortar/pattern finite difference, but linearly predict
  ;; the raw field at its offsets from the cached gradient, not more fields.
  (let* ((helper (if (< (abs (swizzle normal :z)) 0.9)
                     (vec3 0.0 0.0 1.0) (vec3 1.0 0.0 0.0)))
         (tangent (normalize (cross-product normal helper)))
         (bitangent (cross-product normal tangent))
         (step 0.015)
         (a (+ world (* tangent step)))
         (b (+ world (* bitangent step)))
         (ha (cache-surface-height (+ raw (* (/ step 0.09) (dot gradient tangent)))
                                   pattern fade (laid-pattern pattern normal a)))
         (hb (cache-surface-height (+ raw (* (/ step 0.09) (dot gradient bitangent)))
                                   pattern fade (laid-pattern pattern normal b))))
    (normalize (- normal (* (+ (* tangent (- ha here)) (* bitangent (- hb here)))
                            (/ (* strength 0.09) step))))))

(defun cache-sample-form (uv code level)
  `(sample-level material-cache material-cache-sampler (cache-uv ,uv ,code ,level) 0.0))

(defun cache-filter-form (uv)
  `(mix ,(cache-sample-form uv 'relief 'cache-level)
        ,(cache-sample-form uv 'relief '(min (+ cache-level 1.0) cache-last-level))
        (fract cache-lod)))

(defun cached-fragment-specification ()
  (let* ((original (funcall *probe-original-fragment*))
         (source (luv.shader:shader-object-source-form original))
         (options (copy-tree (third source)))
         (body (copy-tree (cdddr source)))
         (bindings (second (first body)))
         (before (ldiff bindings (member (assoc 'height bindings) bindings)))
         (after (member (assoc 'height bindings) bindings)))
    (setf (getf options :resources)
          (append (getf options :resources)
                  '((material-cache :texture-2d :binding 9 :sample-transfer :identity)
                    (material-cache-sampler :sampler :binding 10))))
    (setf (second (first body))
          (append before
                  `((cache-p (and (> relief 0.5) (< relief 2.5)))
                    (cache-last-level ,(float (1- (length *cache-levels*))))
                    (cache-footprint
                     (* (max (sqrt (dot (derivative-x world) (derivative-x world)))
                             (sqrt (dot (derivative-y world) (derivative-y world))))
                        ,(/ (float *cache-resolution*) *cache-period*)))
                    (cache-lod (clamp (* (log (max cache-footprint 1.0)) 1.442695) 0.0 cache-last-level))
                    (cache-level (floor cache-lod))
                    (cache-axis-power (* (* normal normal) (* normal normal)))
                    (cache-weights (/ cache-axis-power (dot cache-axis-power (vec3 1.0 1.0 1.0))))
                    (cache-x (if (and cache-p (> (swizzle cache-weights :x) 0.001))
                                 ,(cache-filter-form '(/ (swizzle world :yz) 4.0)) (vec4 0.0 0.0 1.0 0.5)))
                    (cache-y (if (and cache-p (> (swizzle cache-weights :y) 0.001))
                                 ,(cache-filter-form '(/ (swizzle world :zx) 4.0)) (vec4 0.0 0.0 1.0 0.5)))
                    (cache-z (if (and cache-p (> (swizzle cache-weights :z) 0.001))
                                 ,(cache-filter-form '(/ (swizzle world :xy) 4.0)) (vec4 0.0 0.0 1.0 0.5)))
                    (cache-height (dot (vec3 (swizzle cache-x :w) (swizzle cache-y :w) (swizzle cache-z :w))
                                       cache-weights))
                    (cache-gradient
                     (- (+ (* (vec3 0.0 (swizzle cache-x :x) (swizzle cache-x :y))
                              (/ (swizzle cache-weights :x) (max 0.1 (swizzle cache-x :z))))
                           (* (vec3 (swizzle cache-y :y) 0.0 (swizzle cache-y :x))
                              (/ (swizzle cache-weights :y) (max 0.1 (swizzle cache-y :z))))
                           (* (vec3 (swizzle cache-z :x) (swizzle cache-z :y) 0.0)
                              (/ (swizzle cache-weights :z) (max 0.1 (swizzle cache-z :z)))))))
                    (cache-variance
                     (max 0.0
                          (- 1.0 (dot cache-weights
                                      (vec3 (dot (swizzle cache-x :xyz) (swizzle cache-x :xyz))
                                            (dot (swizzle cache-y :xyz) (swizzle cache-y :xyz))
                                            (dot (swizzle cache-z :xyz) (swizzle cache-z :xyz))))))))
                  after))
    (let ((bindings (second (first body))))
      (setf (second (assoc 'height bindings))
            `(if cache-p (cache-surface-height cache-height (representation material-pattern) fade laid)
                 ,(second (assoc 'height bindings)))
            (second (assoc 'bent bindings))
            `(if cache-p (cache-relief-normal normal world strength (representation material-pattern)
                                              fade cache-height cache-gradient height)
                 ,(second (assoc 'bent bindings)))))
    ;; Preserve unresolved normal variation as roughness instead of sharpening
    ;; distant highlights when the mip average approaches a flat normal.
    (setf body (subst '(if cache-p (sqrt (min 1.0 (+ (* roughness roughness)
                                                   (* strength strength cache-variance)))) roughness)
                     'cache-roughness
                     (subst '(material-specular bent view sun cache-roughness)
                            '(material-specular bent view sun roughness) body :test #'equal)))
    (luv.shader:parse-shader-specification (second source) options body)))

(defun cache-bake-specification (level)
  (destructuring-bind (size row) (nth level *cache-levels*)
    (let* ((pitch (+ size 2))
           (extent `(vec2 ,(float (first *cache-extent*)) ,(float (second *cache-extent*))))
           (base-p (zerop level))
           (expression
             (if base-p
                 '(let* ((h (cache-periodic-height code uv))
                         (du (vec2 (swizzle bake-step :x) 0.0))
                         (dv (vec2 0.0 (swizzle bake-step :x)))
                         (dx (/ (- (cache-periodic-height code (+ uv du))
                                   (cache-periodic-height code (- uv du))) (swizzle bake-step :y)))
                         (dy (/ (- (cache-periodic-height code (+ uv dv))
                                   (cache-periodic-height code (- uv dv))) (swizzle bake-step :y))))
                    (vec4 (normalize (vec3 (* (- (swizzle bake-step :z)) dx)
                                           (* (- (swizzle bake-step :z)) dy) 1.0)) h))
                 (destructuring-bind (previous-size previous-row) (nth (1- level) *cache-levels*)
                   `(let* ((p (+ (vec2
                                  (+ (* column ,(float (+ previous-size 2))) 1.0)
                                  ,(float (+ previous-row 1)))
                                 (+ (* (fract (/ (* local 2.0) ,(float previous-size))) ,(float previous-size))
                                    (vec2 0.5 0.5))))
                           (a (sample-level previous material-cache-sampler (/ p ,extent) 0.0))
                           (b (sample-level previous material-cache-sampler (/ (+ p (vec2 1.0 0.0)) ,extent) 0.0))
                           (c (sample-level previous material-cache-sampler (/ (+ p (vec2 0.0 1.0)) ,extent) 0.0))
                           (d (sample-level previous material-cache-sampler (/ (+ p (vec2 1.0 1.0)) ,extent) 0.0)))
                      (* (+ a b c d) 0.25))))))
      (luv.shader:parse-shader-specification
       'material-cache-bake
       `(:stage :fragment :inputs ((pixel :vec4 :built-in :frag-coord))
         :outputs ((color :vec4 :location 0))
         :resources ,(if base-p
                         '((bake-control :uniform-block :binding 0 :members ((bake-step :vec4))))
                         '((previous :texture-2d :binding 9)
                           (material-cache-sampler :sampler :binding 10))))
       `((let* ((p (floor (swizzle pixel :xy)))
                (column (floor (/ (swizzle p :x) ,(float pitch))))
                (code (+ column 1.0))
                (local (- p (vec2 (+ (* column ,(float pitch)) 1.0) ,(float (+ row 1)))))
                (uv (/ (+ local (vec2 0.5 0.5)) ,(float size)))
                (active (and (< (swizzle p :x) ,(float (* 2 pitch)))
                             (>= (swizzle p :y) ,(float row))
                             (< (swizzle p :y) ,(float (+ row pitch))))))
           (set-output color
                       (if active ,expression
                           ,(if base-p '(vec4 0.0 0.0 1.0 0.5)
                                `(sample-level previous material-cache-sampler
                                               (/ (swizzle pixel :xy) ,extent) 0.0))))))))))

(in-package #:gpu-fps)

(defvar *material-caches* (make-hash-table :test #'eq))
(defvar *material-cache-view* nil)
(defvar *material-cache-sampler* nil)

(defun ensure-material-cache (renderer)
  (or (gethash renderer *material-caches*)
      (let* ((device (r::renderer-device renderer))
             (drawing (r::renderer-terrain renderer))
             (owner (make-instance 'r::gpu-resource-owner))
             (scratch (make-instance 'r::gpu-resource-owner))
             (extent luft.render.shaders::*cache-extent*)
             (start (now))
             (textures nil) (views nil) (control nil))
        (unwind-protect
             (r::with-gpu-construction (owner)
               (setf *material-cache-sampler*
                     (r::own-gpu-resource owner device (luv:make-sampler-descriptor)))
               (setf control (r::own-gpu-resource
                              scratch device (luv:make-buffer-descriptor :size 16 :usage '(:uniform :copy-dst))))
               (luv:write-buffer control (make-array 4 :element-type 'single-float
                                                      :initial-contents '(0.00375 0.03 0.09 0.0)))
               (dotimes (i 2)
                 (let* ((texture (r::own-gpu-resource
                                  scratch device
                                  (luv:make-texture-descriptor :label "material normal-height mip atlas"
                                   :dimensions :2d :size extent :format :rgba16-float
                                   :usage '(:render-attachment :texture-binding :copy-src))))
                        (view (r::own-gpu-resource scratch device
                               (luv:make-texture-view-descriptor :texture texture))))
                   (push texture textures) (push view views)))
               (loop for level below (length luft.render.shaders::*cache-levels*)
                     for destination = (mod level 2)
                     for program = (r::own-gpu-object
                                    scratch
                                    (r::make-drawing-program
                                     device :label "bake authored material"
                                     :vertex (luft.render.shaders:present-vertex-specification)
                                     :fragment (luft.render.shaders::cache-bake-specification level)
                                     :targets '((:format :rgba16-float))))
                     for binding = (r::own-gpu-object
                                    scratch
                                    (if (zerop level) (r::make-program-binding program device :bake-control control)
                                        (r::make-program-binding program device
                                         :previous (nth (- 1 destination) views)
                                         :material-cache-sampler *material-cache-sampler*)))
                     do (let* ((encoder (luv:create device (luv:make-command-encoder-descriptor)))
                               (commands nil))
                          (unwind-protect
                               (let ((pass (luv:begin-render-pass
                                            encoder
                                            (luv:make-render-pass-descriptor
                                             :color-attachments
                                             `((:view ,(nth destination views) :load-op :clear
                                                :store-op :store :clear-value (0.0 0.0 0.0 0.0)))))))
                                 (r::encode-program program pass binding (luv:make-gpu-draw-command :vertex-count 3))
                                 (luv:end-pass pass)
                                 (setf commands (luv:finish encoder))
                                 (luv:submit (luv:device-queue device) commands)
                                 (luv:submitted-work-done (luv:device-queue device)))
                            (when commands (luv:destroy commands))
                            (luv:destroy encoder)))
                     finally
                        (let ((texture (nth destination textures)) (view (nth destination views)))
                          (setf (r::owned-gpu-resources scratch)
                                (remove view (remove texture (r::owned-gpu-resources scratch))))
                          (r::own-gpu-object owner texture)
                          (r::own-gpu-object owner view)
                          (setf *material-cache-view* view)))
               (format t "~&MATERIAL CACHE ~S, ~D bytes retained, ~,2F ms generation+compile+completion~%"
                       extent (* 8 (reduce #'* extent)) (* 1000 (- (now) start)))
               (r::own-gpu-object drawing owner)
               (setf (gethash renderer *material-caches*) owner))
          (luv:destroy scratch)))))

(let ((original (symbol-function 'r::make-program-binding)))
  (setf (symbol-function 'r::make-program-binding)
        (lambda (program device &rest inputs)
          (when (find :material-cache (r::program-inputs program)
                      :key #'luv.shader:shader-resource-key)
            (setf inputs (append inputs (list :material-cache *material-cache-view*
                                             :material-cache-sampler *material-cache-sampler*))))
          (apply original program device inputs))))

(let ((original (symbol-function 'block-program)))
  (setf (symbol-function 'block-program)
        (lambda (renderer mode)
          (if (equal mode "cached")
              (progn
                (ensure-material-cache renderer)
                (let ((luft.render.shaders::*terrain-cloud-visibility-function*
                        'luft.render.shaders::luvcraft-cloud-visibility)
                      (luft.render.shaders::*surface-atmosphere-function*
                        'luft.render.shaders::luvcraft-atmospheric-surface))
                  (r::make-drawing-program
                   (r::renderer-device renderer) :label "cached grass/stone relief"
                   :vertex (luft.render.shaders:terrain-vertex-specification)
                   :fragment (luft.render.shaders::cached-fragment-specification)
                   :targets (mapcar (lambda (format) `(:format ,format))
                                    (if (r::renderer-temporal-p renderer)
                                        '(:rgba16-float :rg16-float) '(:rgba16-float)))
                   :sample-count (r::renderer-scene-sample-count renderer)
                   :depth-stencil '(:format :depth32-float :depth-write-enabled t :depth-compare :less))))
              (funcall original renderer mode)))))
