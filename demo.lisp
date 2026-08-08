;;; Small live demos built out of the public canvas protocol.

(in-package #:luv)

(defclass clear-color-demo ()
  ((canvas
    :initarg :canvas
    :reader demo-canvas)
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
  (let ((canvas (make-sdl-canvas :title title :width width :height height)))
    (open-canvas canvas)
    (handler-case
        (let* ((context (make-canvas-context canvas *gpu-provider*))
               (demo (make-instance 'clear-color-demo
                                    :canvas canvas
                                    :context context
                                    :speed speed)))
          (setf (canvas-clock canvas)
                (make-cadence-clock
                 (lambda (native-canvas timestamp)
                   (declare (ignore native-canvas))
                   (render-clear-color-demo-frame demo timestamp))
                 :frames-per-second frames-per-second))
          demo)
      (error (condition)
        (close-canvas canvas)
        (error condition)))))

(defun stop-clear-color-demo (demo)
  "Stop DEMO, close its canvas, and return no values."
  (let ((canvas (demo-canvas demo)))
    (when (eq :open (canvas-state canvas))
      (setf (canvas-clock canvas) (make-demand-clock)))
    (close-canvas canvas))
  (values))

(defclass compute-gradient-demo ()
  ((canvas :initarg :canvas :reader demo-canvas)
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
        (context nil)
        (resources nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (progn
           (setf context (make-canvas-context canvas *gpu-provider*))
           (let* ((device (context-device context))
                  (extent (canvas-extent context))
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
                     :canvas canvas :context context :texture texture
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
        (close-canvas canvas)))))

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
  (values))
