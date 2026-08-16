;;; Small live demos built out of the public canvas protocol.

(in-package #:luv)

(defclass clear-color-demo ()
  ((canvas
    :initarg :canvas
    :reader demo-canvas)
   (device
    :initarg :device
    :reader demo-device)
   (context
    :initarg :context
    :reader demo-context)
   (speed
    :initarg :speed
    :reader demo-speed)
   (start-time
    :initform nil
    :accessor demo-start-time))
  (:documentation "A running cadence-clock color-cycle demonstration."))

(defun clear-color-component (phase offset)
  (* 0.5 (+ 1.0 (sin (+ phase offset)))))

(defun render-clear-color-demo-frame (demo timestamp)
  (unless (demo-start-time demo)
    (setf (demo-start-time demo) timestamp))
  (let* ((tau (* 2 pi))
         (phase (* tau (demo-speed demo)
                   (- timestamp (demo-start-time demo)))))
    (render-canvas-color
     (demo-context demo)
     (clear-color-component phase 0)
     (clear-color-component phase (/ tau 3))
     (clear-color-component phase (* 2 (/ tau 3))))))

(defun start-clear-color-demo (&key
                                 (title "luv clear color demo")
                                 (width 800)
                                 (height 600)
                                 (frames-per-second 60)
                                 (speed 0.08))
  "Open and return a smoothly cycling CLEAR-COLOR-DEMO.

SPEED is the number of complete color cycles per second.  Stop the returned
object with STOP-CLEAR-COLOR-DEMO."
  (let ((canvas (make-sdl-canvas :title title :width width :height height))
        (device nil))
    (open-canvas canvas)
    (handler-case
        (progn
          (setf device
                (request-gpu-device
                 *gpu-provider*
                 (make-device-descriptor :label title)))
          (let* ((context
                   (make-canvas-context
                    canvas *gpu-provider*
                    (make-canvas-configuration :device device)))
                 (demo (make-instance 'clear-color-demo
                                      :canvas canvas
                                      :device device
                                      :context context
                                      :speed speed)))
            (setf (canvas-clock canvas)
                  (make-cadence-clock
                   (lambda (native-canvas timestamp)
                     (declare (ignore native-canvas))
                     (render-clear-color-demo-frame demo timestamp))
                   :frames-per-second frames-per-second))
            demo))
      (error (condition)
        (close-canvas canvas)
        (when device
          (destroy device))
        (error condition)))))

(defun stop-clear-color-demo (demo)
  "Stop DEMO, close its canvas, and return no values."
  (let ((canvas (demo-canvas demo)))
    (when (eq :open (canvas-state canvas))
      (setf (canvas-clock canvas) (make-demand-clock)))
    (close-canvas canvas)
    (destroy (demo-device demo)))
  (values))

(defclass compute-gradient-demo ()
  ((canvas :initarg :canvas :reader demo-canvas)
   (device :initarg :device :reader demo-device)
   (context :initarg :context :reader demo-context)
   (texture :initarg :texture :reader compute-demo-texture)
   (view :initarg :view :reader compute-demo-view)
   (module :initarg :module :reader compute-demo-module)
   (layout :initarg :layout :reader compute-demo-layout)
   (pipeline :initarg :pipeline :reader compute-demo-pipeline)
   (bind-group :initarg :bind-group :reader compute-demo-bind-group))
  (:documentation
   "A compute shader writing an intermediate texture copied to a canvas."))

(defun render-compute-gradient-demo (demo)
  (let* ((context (demo-context demo))
         (extent (canvas-extent context)))
    (present-canvas-frame
     context
     (lambda (surface-texture encoder)
       (let ((pass (begin-compute-pass encoder)))
         (encode
          pass
          (make-gpu-set-pipeline-command
           :pipeline (compute-demo-pipeline demo)))
         (encode
          pass
          (make-gpu-set-bind-group-command
           :index 0 :bind-group (compute-demo-bind-group demo)))
         (encode
          pass
          (make-gpu-dispatch-workgroups-command
           :x (ceiling (first extent) 8)
           :y (ceiling (second extent) 8)))
         (end-pass pass))
       (encode
        encoder
        (make-gpu-copy-texture-command
         :source (compute-demo-texture demo)
         :destination surface-texture))))))

(defun start-compute-gradient-demo (&key
                                      (title "luv s-expression compute shader")
                                      (width 800)
                                      (height 600))
  "Open a canvas filled by an s-expression SPIR-V compute shader."
  (let ((canvas (make-sdl-canvas :title title :width width :height height))
        (device nil)
        (context nil)
        (resources nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (progn
           (setf device
                 (request-gpu-device
                  *gpu-provider*
                  (make-device-descriptor :label title))
                 context
                 (make-canvas-context
                  canvas *gpu-provider*
                  (make-canvas-configuration :device device)))
           (let* ((extent (canvas-extent context))
                  (texture
                    (create
                     device
                     (make-texture-descriptor
                      :label "compute gradient"
                      :size extent :dimensions :2d :format :rgba8-unorm
                      :usage '(:storage-binding :copy-src))))
                  (view
                    (create device
                            (make-texture-view-descriptor :texture texture)))
                  (module
                    (create
                     device
                     (make-shader-module-descriptor
                      :code (spv:gradient-compute-shader
                             :width (first extent)
                             :height (second extent)))))
                  (layout
                    (create
                     device
                     (make-bind-group-layout-descriptor
                      :entries '((:binding 0 :type :storage-texture)))))
                  (pipeline
                    (create
                     device
                     (make-compute-pipeline-descriptor
                      :layout layout :module module)))
                  (bind-group
                    (create
                     device
                     (make-bind-group-descriptor
                      :layout layout
                      :entries `((:binding 0 :resource ,view)))))
                  (demo
                    (make-instance
                     'compute-gradient-demo
                     :canvas canvas :device device :context context
                     :texture texture
                     :view view :module module :layout layout
                     :pipeline pipeline :bind-group bind-group)))
             (setf resources
                   (list bind-group pipeline layout module view texture))
             (render-compute-gradient-demo demo)
             (setf completed-p t)
             demo))
      (unless completed-p
        (dolist (resource resources)
          (ignore-errors (destroy resource)))
        (close-canvas canvas)
        (when device
          (destroy device))))))

(defun stop-compute-gradient-demo (demo)
  "Destroy DEMO's compute resources and close its canvas."
  (dolist (resource
            (list (compute-demo-bind-group demo)
                  (compute-demo-pipeline demo)
                  (compute-demo-layout demo)
                  (compute-demo-module demo)
                  (compute-demo-view demo)
                  (compute-demo-texture demo)))
    (destroy resource))
  (close-canvas (demo-canvas demo))
  (destroy (demo-device demo))
  (values))

(defun render-metal-slug-bezier-proof (provider)
  "Render the fixed Slug outline on Metal and return pixels, width, height, format.

The shader specifications remain backend-neutral and independently lower to
SPIR-V.  This live proof uses Metal because the current no-resource pipeline
seam is available there without inventing an empty bind-group abstraction."
  (let ((device nil)
        (vertex-module nil)
        (fragment-module nil)
        (pipeline nil)
        (target nil)
        (vertices nil)
        (readback nil)
        (encoder nil)
        (command-buffer nil)
        (width 256)
        (height 256)
        (format :rgba8-unorm))
    (unwind-protect
         (progn
           (setf device (request-gpu-device provider)
                 vertex-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "Slug proof vertex"
                   :language :mathematical
                   :code (luv.slug:slug-bezier-vertex-specification)))
                 fragment-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "Slug proof fragment"
                   :language :mathematical
                   :code (luv.slug:slug-bezier-fragment-specification)))
                 pipeline
                 (create
                  device
                  (make-render-pipeline-descriptor
                   :label "Slug quadratic outline proof"
                   :layout nil
                   :vertex
                   `(:module ,vertex-module
                     :buffers
                     ((:array-stride 24
                       :attributes
                       ((:shader-location 0 :offset 0 :format :float32x3)
                        (:shader-location 1 :offset 12 :format :float32x3)))))
                   :fragment
                   `(:module ,fragment-module
                     :targets ((:format ,format)))))
                 target
                 (create
                  device
                  (make-texture-descriptor
                   :label "Slug proof target"
                   :size (list width height) :dimensions :2d :format format
                   :usage '(:render-attachment :copy-src)))
                 vertices
                 (create
                  device
                  (make-buffer-descriptor
                   :label "Slug proof quad"
                   :size (* 36 4) :usage '(:vertex :copy-dst)))
                 readback
                 (create
                  device
                  (make-buffer-descriptor
                   :label "Slug proof readback"
                   :size (* width height 4) :usage '(:copy-dst))))
           (write-buffer
            vertices
            (make-array
             36 :element-type 'single-float
             :initial-contents
             '(-0.8 -0.8 0.0  0.0 1.0 0.0
                0.8 -0.8 0.0  1.0 1.0 0.0
                0.8  0.8 0.0  1.0 0.0 0.0
               -0.8 -0.8 0.0  0.0 1.0 0.0
                0.8  0.8 0.0  1.0 0.0 0.0
               -0.8  0.8 0.0  0.0 0.0 0.0)))
           (setf encoder
                 (create device
                         (make-command-encoder-descriptor
                          :label "Slug proof commands")))
           (let ((pass
                   (begin-render-pass
                    encoder
                    (make-render-pass-descriptor
                     :color-attachments
                     `((:view ,target :load-op :clear :store-op :store
                        :clear-value #(0.0 0.0 0.0 1.0)))))))
             (set-pipeline pass pipeline)
             (set-vertex-buffer pass 0 vertices)
             (draw pass 6)
             (end-pass pass))
           (encode
            encoder
            (make-gpu-copy-texture-to-buffer-command
             :source target :destination readback))
           (setf command-buffer (finish encoder))
           (submit (device-queue device) command-buffer)
           (values (read-buffer readback) width height format))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (when readback (destroy readback))
      (when vertices (destroy vertices))
      (when target (destroy target))
      (when pipeline (destroy pipeline))
      (when fragment-module (destroy fragment-module))
      (when vertex-module (destroy vertex-module))
      (when device (destroy device)))))
