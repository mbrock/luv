;;; A first deliberately theatrical McCLIM compositor.
;;;
;;; McCLIM still paints an ordinary CPU raster.  The raster mirror uploads it
;;; to a sampled GPU texture; this object renders that texture as a
;;; perspective-spinning quad into an offscreen color attachment, after which
;;; the canvas context performs its familiar texture-to-swapchain copy.

(in-package #:mcluv)

(shader:define-shader spinning-texture-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (texture-coordinate :vec2 :location 0)))
  (let* ((two (shader:uint 2.0))
         (x-bit (mod vertex-index two))
         (y-bit (/ vertex-index two))
         (x (shader:float x-bit))
         (y (shader:float y-bit))
         (center-x (- (* x 2.0) 1.0))
         (center-y (- (* y 2.0) 1.0)))
    (shader:set-output texture-coordinate (shader:vec2 x y))
    (shader:set-output
     clip-position (+ center (* right center-x) (* up center-y)))))

(shader:define-shader spinning-texture-fragment-specification
    (:stage :fragment
     :inputs ((texture-coordinate :vec2 :location 0))
     :resources
     ((image :texture-2d :set 0 :binding 0 :sample-transfer :identity)
      (texture-sampler :sampler :set 0 :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((texel (shader:sample image texture-sampler texture-coordinate)))
    (shader:set-output color-output texel)))

(shader:define-shader lisp-machine-chassis-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (chassis-color :vec4 :location 0)))
  (let* ((one (shader:uint 1.0))
         (two (shader:uint 2.0))
         (four (shader:uint 4.0))
         (layer (/ vertex-index four))
         (local-index (mod vertex-index four))
         (x (shader:float (mod local-index two)))
         (y (shader:float (/ local-index two)))
         (shadow-p (< layer one))
         (body-p (< layer two))
         (scale-x
           (if shadow-p 1.2058824 (if body-p 1.1764706 1.0735294)))
         (scale-y
           (if shadow-p 1.2647059 (if body-p 1.2352941 1.0882353)))
         (offset-x (if shadow-p 0.05147059 0.0))
         (offset-y
           (if shadow-p 0.08088235 (if body-p 0.01764706 0.0)))
         (color-r (if shadow-p 0.035 (if body-p 0.50 0.075)))
         (color-g (if shadow-p 0.045 (if body-p 0.47 0.095)))
         (color-b (if shadow-p 0.043 (if body-p 0.38 0.085)))
         (color (shader:vec4 color-r color-g color-b 1.0))
         (layer-x (+ (* (- (* x 2.0) 1.0) scale-x) offset-x))
         (layer-y (+ (* (- (* y 2.0) 1.0) scale-y) offset-y)))
    (shader:set-output chassis-color color)
    (shader:set-output
     clip-position (+ center (* right layer-x) (* up layer-y)))))

(shader:define-shader lisp-machine-chassis-fragment-specification
    (:stage :fragment
     :inputs ((chassis-color :vec4 :location 0))
     :outputs ((color-output :vec4 :location 0)))
  (let* ()
    (shader:set-output color-output chassis-color)))

(defclass spinning-compositor-frame-state ()
  ((buffer
    :initarg :buffer
    :reader spinning-frame-state-buffer)
   (bind-group
    :initarg :bind-group
    :reader spinning-frame-state-bind-group)
   (relief-buffer
    :initform nil
    :accessor spinning-frame-state-relief-buffer)
   (relief-capacity
    :initform 0
    :accessor spinning-frame-state-relief-capacity)))

(defclass spinning-texture-compositor ()
  ((speed
    :initarg :speed
    :initform 0.12
    :reader spinning-compositor-speed)
   (start-time
    :initform nil
    :accessor spinning-compositor-start-time)
   (device
    :initform nil
    :accessor spinning-compositor-device)
   (source
    :initform nil
    :accessor spinning-compositor-source)
   (size
    :initform nil
    :accessor spinning-compositor-size)
   (depth-format
    :initform nil
    :accessor spinning-compositor-depth-format)
   (depth-compare
    :initform nil
    :accessor spinning-compositor-depth-compare)
   (target-format
    :initform nil
    :accessor spinning-compositor-target-format)
   (output
    :initform nil
    :accessor spinning-compositor-output)
   (source-view
    :initform nil
    :accessor spinning-compositor-source-view)
   (output-view
    :initform nil
    :accessor spinning-compositor-output-view)
   (sampler
    :initform nil
    :accessor spinning-compositor-sampler)
   (vertex-module
    :initform nil
    :accessor spinning-compositor-vertex-module)
   (fragment-module
    :initform nil
    :accessor spinning-compositor-fragment-module)
   (chassis-vertex-module
    :initform nil
    :accessor spinning-compositor-chassis-vertex-module)
   (chassis-fragment-module
    :initform nil
    :accessor spinning-compositor-chassis-fragment-module)
   (layout
    :initform nil
    :accessor spinning-compositor-layout)
   (pipeline
    :initform nil
    :accessor spinning-compositor-pipeline)
   (chassis-pipeline
    :initform nil
    :accessor spinning-compositor-chassis-pipeline)
   (frame-states
    :initform (make-hash-table :test #'eq)
    :reader spinning-compositor-frame-states)))

(defun spinning-compositor-resources (compositor)
  (remove nil
          (list (spinning-compositor-pipeline compositor)
                (spinning-compositor-chassis-pipeline compositor)
                (spinning-compositor-layout compositor)
                (spinning-compositor-chassis-fragment-module compositor)
                (spinning-compositor-chassis-vertex-module compositor)
                (spinning-compositor-fragment-module compositor)
                (spinning-compositor-vertex-module compositor)
                (spinning-compositor-sampler compositor)
                (spinning-compositor-output-view compositor)
                (spinning-compositor-source-view compositor)
                (spinning-compositor-output compositor))))

(defun clear-spinning-compositor-resources (compositor)
  (maphash
   (lambda (surface state)
     (declare (ignore surface))
     (alexandria:when-let
         ((buffer (spinning-frame-state-relief-buffer state)))
       (luv:destroy buffer))
     (luv:destroy (spinning-frame-state-bind-group state))
     (luv:destroy (spinning-frame-state-buffer state)))
   (spinning-compositor-frame-states compositor))
  (clrhash (spinning-compositor-frame-states compositor))
  (dolist (resource (spinning-compositor-resources compositor))
    (luv:destroy resource))
  (setf (spinning-compositor-device compositor) nil
        (spinning-compositor-source compositor) nil
        (spinning-compositor-size compositor) nil
        (spinning-compositor-depth-format compositor) nil
        (spinning-compositor-depth-compare compositor) nil
        (spinning-compositor-target-format compositor) nil
        (spinning-compositor-output compositor) nil
        (spinning-compositor-source-view compositor) nil
        (spinning-compositor-output-view compositor) nil
        (spinning-compositor-sampler compositor) nil
        (spinning-compositor-vertex-module compositor) nil
        (spinning-compositor-fragment-module compositor) nil
        (spinning-compositor-chassis-vertex-module compositor) nil
        (spinning-compositor-chassis-fragment-module compositor) nil
        (spinning-compositor-layout compositor) nil
        (spinning-compositor-pipeline compositor) nil
        (spinning-compositor-chassis-pipeline compositor) nil)
  compositor)

(defmethod release-raster-mirror-compositor
    ((compositor spinning-texture-compositor))
  (clear-spinning-compositor-resources compositor)
  (values))

(defun ensure-spinning-compositor-resources
    (compositor context source
     &key depth-format (depth-compare :less) target-format)
  (let* ((device (luv:context-device context))
         (size (luv:gpu-texture-size source))
         (format (luv:gpu-texture-format source))
         (target-format (or target-format format)))
    (unless (and (eq device (spinning-compositor-device compositor))
                 (eq source (spinning-compositor-source compositor))
                 (equal size (spinning-compositor-size compositor))
                 (eq depth-format
                     (spinning-compositor-depth-format compositor))
                 (eq depth-compare
                     (spinning-compositor-depth-compare compositor))
                 (eq target-format
                     (spinning-compositor-target-format compositor)))
      (clear-spinning-compositor-resources compositor)
      (let ((created nil)
            (completed-p nil))
        (unwind-protect
             (labels ((create-resource (descriptor)
                        (let ((resource (luv:create device descriptor)))
                          (push resource created)
                          resource)))
               (let* ((output
                      (create-resource
                       (luv:make-texture-descriptor
                        :label "McCLIM composited frame"
                        :size size :dimensions :2d :format format
                        :usage '(:render-attachment :copy-src))))
                    (source-view
                      (create-resource
                       (luv:make-texture-view-descriptor :texture source)))
                    (output-view
                      (create-resource
                       (luv:make-texture-view-descriptor :texture output)))
                    (sampler
                      (create-resource
                       (luv:make-sampler-descriptor
                        :label "McCLIM linear sampler")))
                    (vertex-module
                      (create-resource
                       (luv:make-shader-module-descriptor
                        :label "spinning McCLIM vertex shader"
                        :language :mathematical
                        :code (spinning-texture-vertex-specification))))
                    (fragment-module
                      (create-resource
                       (luv:make-shader-module-descriptor
                        :label "spinning McCLIM fragment shader"
                        :language :mathematical
                        :code (spinning-texture-fragment-specification))))
                    (chassis-vertex-module
                      (create-resource
                       (luv:make-shader-module-descriptor
                        :label "Lisp machine chassis vertex shader"
                        :language :mathematical
                        :code
                        (lisp-machine-chassis-vertex-specification))))
                    (chassis-fragment-module
                      (create-resource
                       (luv:make-shader-module-descriptor
                        :label "Lisp machine chassis fragment shader"
                        :language :mathematical
                        :code
                        (lisp-machine-chassis-fragment-specification))))
                    (layout
                      (create-resource
                       (luv:make-bind-group-layout-descriptor
                        :label "sampled McCLIM texture layout"
                        :entries '((:binding 0 :type :texture)
                                   (:binding 1 :type :sampler)
                                   (:binding 2 :type :uniform-buffer)))))
                    (pipeline
                      (create-resource
                       (luv:make-render-pipeline-descriptor
                        :label "spinning McCLIM quad"
                        :layout layout
                        :vertex `(:module ,vertex-module)
                        :fragment `(:module ,fragment-module
                                    :targets ((:format ,target-format)))
                        :depth-stencil
                        (when depth-format
                          `(:format ,depth-format
                            :depth-write-enabled nil
                            :depth-compare ,depth-compare))
                        :primitive '(:topology :triangle-strip))))
                    (chassis-pipeline
                      (create-resource
                       (luv:make-render-pipeline-descriptor
                        :label "Lisp machine terminal chassis"
                        :layout layout
                        :vertex `(:module ,chassis-vertex-module)
                        :fragment `(:module ,chassis-fragment-module
                                    :targets ((:format ,target-format)))
                        :depth-stencil
                        (when depth-format
                          `(:format ,depth-format
                            :depth-write-enabled nil
                            :depth-compare ,depth-compare))
                        :primitive '(:topology :triangle-strip)))))
                 (setf (spinning-compositor-device compositor) device
                       (spinning-compositor-source compositor) source
                       (spinning-compositor-size compositor) size
                       (spinning-compositor-depth-format compositor)
                       depth-format
                       (spinning-compositor-depth-compare compositor)
                       depth-compare
                       (spinning-compositor-target-format compositor)
                       target-format
                       (spinning-compositor-output compositor) output
                       (spinning-compositor-source-view compositor) source-view
                       (spinning-compositor-output-view compositor) output-view
                       (spinning-compositor-sampler compositor) sampler
                       (spinning-compositor-vertex-module compositor)
                       vertex-module
                       (spinning-compositor-fragment-module compositor)
                       fragment-module
                       (spinning-compositor-chassis-vertex-module compositor)
                       chassis-vertex-module
                       (spinning-compositor-chassis-fragment-module compositor)
                       chassis-fragment-module
                       (spinning-compositor-layout compositor) layout
                       (spinning-compositor-pipeline compositor) pipeline
                       (spinning-compositor-chassis-pipeline compositor)
                       chassis-pipeline
                       completed-p t)))
          (unless completed-p
            (dolist (resource created)
              (ignore-errors (luv:destroy resource))))))))
  compositor)

(defun ensure-spinning-compositor-frame-state (compositor surface)
  ;; A swapchain image is only acquired again after its previous presentation,
  ;; so its matching uniform buffer is no longer being read by the GPU.
  (or (gethash surface (spinning-compositor-frame-states compositor))
      (let* ((device (spinning-compositor-device compositor))
             (buffer nil)
             (bind-group nil)
             (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (luv:create
                      device
                      (luv:make-buffer-descriptor
                       :label "McCLIM quad transform"
                       :size 64 :usage '(:uniform)))
                     bind-group
                     (luv:create
                      device
                      (luv:make-bind-group-descriptor
                       :label "sampled McCLIM raster and uniform state"
                       :layout (spinning-compositor-layout compositor)
                       :entries
                       `((:binding 0
                          :resource
                          ,(spinning-compositor-source-view compositor))
                         (:binding 1
                          :resource ,(spinning-compositor-sampler compositor))
                         (:binding 2 :resource ,buffer)))))
               (let ((state
                       (make-instance
                        'spinning-compositor-frame-state
                        :buffer buffer :bind-group bind-group)))
                 (setf (gethash surface
                                (spinning-compositor-frame-states compositor))
                       state
                       completed-p t)
                 state))
          (unless completed-p
            (when bind-group (luv:destroy bind-group))
            (when buffer (luv:destroy buffer)))))))

(defun spinning-compositor-state
    (compositor timestamp &key (aspect-scale 1.0))
  (unless (spinning-compositor-start-time compositor)
    (setf (spinning-compositor-start-time compositor) timestamp))
  (let ((phase (* 2 pi (spinning-compositor-speed compositor)
                  (- timestamp
                     (spinning-compositor-start-time compositor)))))
    (make-array
     12 :element-type 'single-float
     :initial-contents
     (let ((sine (sin phase))
           (cosine (cos phase)))
       (mapcar
        (lambda (value) (coerce value 'single-float))
        (list (* sine 0.12) 0.0 0.45 1.18
              (* 0.68 aspect-scale cosine) 0.0
              (* 0.22 sine) (* 0.48 sine)
              0.0 0.68 0.0 0.0))))))

(defun render-spinning-mirror-frame (mirror timestamp)
  (let ((context (mirror-context mirror))
        (source (mirror-texture mirror))
        (compositor (mirror-compositor mirror)))
    (when (and context source (typep compositor 'spinning-texture-compositor)
               (eq :open (luv:canvas-state (mirror-target mirror))))
      (ensure-spinning-compositor-resources compositor context source)
      (let ((state (spinning-compositor-state compositor timestamp)))
        (luv:present-canvas-frame
         context
         (lambda (surface encoder)
           (let ((frame-state
                   (ensure-spinning-compositor-frame-state
                    compositor surface)))
             (luv:write-buffer
              (spinning-frame-state-buffer frame-state) state)
             (let ((pass
                     (luv:begin-render-pass
                      encoder
                      (luv:make-render-pass-descriptor
                       :label "McCLIM offscreen pass"
                       :color-attachments
                       `((:view ,(spinning-compositor-output-view compositor)
                          :load-op :clear :store-op :store
                          :clear-value #(0.025 0.025 0.04 1.0)))))))
               (luv:encode
                pass
                (luv:make-gpu-set-pipeline-command
                 :pipeline (spinning-compositor-chassis-pipeline compositor)))
               (luv:encode
                pass
                (luv:make-gpu-set-bind-group-command
                 :index 0
                 :bind-group
                 (spinning-frame-state-bind-group frame-state)))
               (dotimes (layer 3)
                 (luv:encode
                  pass
                  (luv:make-gpu-draw-command
                   :vertex-count 4 :first-vertex (* layer 4))))
               (luv:encode
                pass
                (luv:make-gpu-set-pipeline-command
                 :pipeline (spinning-compositor-pipeline compositor)))
               (luv:encode
                pass
                (luv:make-gpu-set-bind-group-command
                 :index 0
                 :bind-group
                 (spinning-frame-state-bind-group frame-state)))
               (luv:encode
                pass (luv:make-gpu-draw-command :vertex-count 4))
               (luv:end-pass pass))
             (luv:encode
              encoder
              (luv:make-gpu-copy-texture-command
               :source (spinning-compositor-output compositor)
               :destination surface))))))))
  mirror)

(defmethod present-raster-mirror-texture
    ((mirror luv-raster-mirror) context texture
     (compositor spinning-texture-compositor))
  (declare (ignore context texture))
  (render-spinning-mirror-frame
   mirror (float (/ (get-internal-real-time)
                    internal-time-units-per-second)
                 1.0d0)))

(defun enable-spinning-mirror
    (mirror &key (frames-per-second 60) (speed 0.12))
  "Make MIRROR orbit as a sampled texture and return its compositor."
  (check-type mirror luv-raster-mirror)
  (release-raster-mirror-compositor (mirror-compositor mirror))
  (let* ((compositor
           (make-instance 'spinning-texture-compositor :speed speed))
         (canvas (mirror-target mirror)))
    (setf (mirror-compositor mirror) compositor
          (luv:canvas-clock canvas)
          (luv:make-cadence-clock
           (lambda (native-canvas timestamp)
             (declare (ignore native-canvas))
             (render-spinning-mirror-frame mirror timestamp))
           :frames-per-second frames-per-second))
    (luv:request-canvas-frame
     canvas
     (lambda (timestamp)
       (render-spinning-mirror-frame mirror timestamp)))
    compositor))

(defun disable-spinning-mirror (mirror)
  "Return MIRROR to direct texture presentation."
  (check-type mirror luv-raster-mirror)
  (let ((canvas (mirror-target mirror)))
    (setf (luv:canvas-clock canvas) (luv:make-demand-clock))
    (release-raster-mirror-compositor (mirror-compositor mirror))
    (setf (mirror-compositor mirror) nil)
    (setf (mcclim-render:image-dirty-region mirror) +everywhere+)
    (present-mirror mirror))
  mirror)

(defun open-spinning-widget-lab
    (&key (title "McCLIM widget drifting through luv space")
          (frames-per-second 60) (speed 0.12))
  "Open WIDGET-LAB and immediately composite it as a spinning 3D quad."
  (let* ((frame (open-widget-lab :title title
                                 :server-path '(:luv-raster)))
         (sheet (frame-top-level-sheet frame))
         (mirror (sheet-direct-mirror sheet)))
    (enable-spinning-mirror
     mirror :frames-per-second frames-per-second :speed speed)
    frame))
