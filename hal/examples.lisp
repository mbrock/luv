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
                     ((:array-stride 36
                       :attributes
                       ((:shader-location 0 :offset 0 :format :float32x3)
                        (:shader-location 1 :offset 12 :format :float32x3)
                        (:shader-location 2 :offset 24 :format :float32x3)))))
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
                   :size (* 54 4) :usage '(:vertex :copy-dst)))
                 readback
                 (create
                  device
                  (make-buffer-descriptor
                   :label "Slug proof readback"
                   :size (* width height 4) :usage '(:copy-dst))))
           (write-buffer
            vertices
            (make-array
             54 :element-type 'single-float
             :initial-contents
             '(-0.8 -0.8 0.0  0.0 1.0 0.0  204.8 204.8 0.0
                0.8 -0.8 0.0  1.0 1.0 0.0  204.8 204.8 0.0
                0.8  0.8 0.0  1.0 0.0 0.0  204.8 204.8 0.0
               -0.8 -0.8 0.0  0.0 1.0 0.0  204.8 204.8 0.0
                0.8  0.8 0.0  1.0 0.0 0.0  204.8 204.8 0.0
               -0.8  0.8 0.0  0.0 0.0 0.0  204.8 204.8 0.0)))
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

(defun render-metal-slug-outline (provider outline)
  "Render one em-normalized quadratic OUTLINE through Slug's texture path.

The proof window is the unit em square.  OUTLINE may contain any number of
closed contours and curves that fit that window; serialization and both axis
traversals are data-driven.  Return pixels, width, height, and format."
  (let ((device nil)
        (vertex-module nil)
        (fragment-module nil)
        (layout nil)
        (pipeline nil)
        (band-texture nil)
        (band-view nil)
        (curve-texture nil)
        (curve-view nil)
        (bind-group nil)
        (target nil)
        (vertices nil)
        (readback nil)
        (encoder nil)
        (command-buffer nil)
        (width 256)
        (height 256)
        (format :rgba8-unorm)
        (serialized
          (luv.slug:serialize-slug-outline
           outline :horizontal-band-count 1 :vertical-band-count 1)))
    (unwind-protect
         (progn
           (setf device (request-gpu-device provider)
                 vertex-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "Slug outline vertex"
                   :language :mathematical
                   :code (luv.slug:slug-bezier-vertex-specification)))
                 fragment-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "Slug band traversal fragment"
                   :language :mathematical
                   :code (luv.slug:slug-banded-fragment-specification)))
                 layout
                 (create
                  device
                  (make-bind-group-layout-descriptor
                   :label "Slug outline texture layout"
                   :entries '((:binding 0 :type :texture)
                              (:binding 1 :type :texture))))
                 pipeline
                 (create
                  device
                  (make-render-pipeline-descriptor
                   :label "Slug serialized outline pipeline"
                   :layout layout
                   :vertex
                   `(:module ,vertex-module
                     :buffers
                     ((:array-stride 36
                       :attributes
                       ((:shader-location 0 :offset 0 :format :float32x3)
                        (:shader-location 1 :offset 12 :format :float32x3)
                        (:shader-location 2 :offset 24 :format :float32x3)))))
                   :fragment
                   `(:module ,fragment-module
                     :targets ((:format ,format)))))
                 band-texture
                 (create
                  device
                  (make-texture-descriptor
                   :label "Slug RG16U bands"
                   :size
                   (luv.slug:slug-serialized-outline-band-texture-size
                    serialized)
                   :dimensions :2d :format :rg16-uint
                   :usage '(:texture-binding :copy-dst)))
                 curve-texture
                 (create
                  device
                  (make-texture-descriptor
                   :label "Slug RGBA16F curves"
                   :size
                   (luv.slug:slug-serialized-outline-curve-texture-size
                    serialized)
                   :dimensions :2d :format :rgba16-float
                   :usage '(:texture-binding :copy-dst))))
           (write-texture
            (device-queue device) (make-texture-copy :texture band-texture)
            (luv.slug:slug-serialized-outline-band-upload-data serialized)
            (make-texture-data-layout
             :bytes-per-row
             (* 4 (luv.slug:slug-serialized-outline-band-width serialized))
             :rows-per-image
             (second
              (luv.slug:slug-serialized-outline-band-texture-size serialized)))
            (luv.slug:slug-serialized-outline-band-texture-size serialized))
           (write-texture
            (device-queue device) (make-texture-copy :texture curve-texture)
            (luv.slug:slug-serialized-outline-curve-upload-data serialized)
            (make-texture-data-layout
             :bytes-per-row
             (* 8 (luv.slug:slug-serialized-outline-curve-width serialized))
             :rows-per-image
             (second
              (luv.slug:slug-serialized-outline-curve-texture-size serialized)))
            (luv.slug:slug-serialized-outline-curve-texture-size serialized))
           (setf band-view
                 (create
                  device (make-texture-view-descriptor :texture band-texture))
                 curve-view
                 (create
                  device (make-texture-view-descriptor :texture curve-texture))
                 bind-group
                 (create
                  device
                  (make-bind-group-descriptor
                   :label "Slug outline textures"
                   :layout layout
                   :entries `((:binding 0 :resource ,band-view)
                              (:binding 1 :resource ,curve-view))))
                 target
                 (create
                  device
                  (make-texture-descriptor
                   :label "Slug outline target"
                   :size (list width height) :dimensions :2d :format format
                   :usage '(:render-attachment :copy-src)))
                 vertices
                 (create
                  device
                  (make-buffer-descriptor
                   :label "Slug em-square quad"
                   :size (* 54 4) :usage '(:vertex :copy-dst)))
                 readback
                 (create
                  device
                  (make-buffer-descriptor
                   :label "Slug outline readback"
                   :size (* width height 4) :usage '(:copy-dst))))
           (write-buffer
            vertices
            (make-array
             54 :element-type 'single-float
             :initial-contents
             '(-0.8 -0.8 0.0  0.0 0.0 0.0  204.8 204.8 0.0
                0.8 -0.8 0.0  1.0 0.0 0.0  204.8 204.8 0.0
                0.8  0.8 0.0  1.0 1.0 0.0  204.8 204.8 0.0
               -0.8 -0.8 0.0  0.0 0.0 0.0  204.8 204.8 0.0
                0.8  0.8 0.0  1.0 1.0 0.0  204.8 204.8 0.0
               -0.8  0.8 0.0  0.0 1.0 0.0  204.8 204.8 0.0)))
           (setf encoder
                 (create device
                         (make-command-encoder-descriptor
                          :label "Slug outline commands")))
           (let ((pass
                   (begin-render-pass
                    encoder
                    (make-render-pass-descriptor
                     :color-attachments
                     `((:view ,target :load-op :clear :store-op :store
                        :clear-value #(0.0 0.0 0.0 1.0)))))))
             (set-pipeline pass pipeline)
             (set-bind-group pass 0 bind-group)
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
      (when bind-group (destroy bind-group))
      (when curve-view (destroy curve-view))
      (when band-view (destroy band-view))
      (when curve-texture (destroy curve-texture))
      (when band-texture (destroy band-texture))
      (when pipeline (destroy pipeline))
      (when layout (destroy layout))
      (when fragment-module (destroy fragment-module))
      (when vertex-module (destroy vertex-module))
      (when device (destroy device)))))

(defun render-metal-slug-glyph (provider character font-loader)
  "Render CHARACTER from FONT-LOADER through Slug's GPU outline path."
  (render-metal-slug-outline
   provider
   (luv.slug:normalize-slug-glyph-outline
    (luv.slug:load-slug-glyph character font-loader))))

(defstruct slug-text-draw
  glyph-id serialized origin-x origin-y
  outline-min-x outline-min-y outline-max-x outline-max-y
  band-texture band-view curve-texture curve-view bind-group)

(defun make-slug-text-draws (shaped font-loader)
  "Join HarfBuzz placement records to ZPB-TTF outlines by glyph ID."
  (let* ((units-per-em (luv.slug:slug-shaped-text-units-per-em shaped))
         (unit (/ 1 units-per-em))
         (pen-x 0)
         (pen-y 0)
         draws)
    (loop for placement across (luv.slug:slug-shaped-text-glyphs shaped)
          for glyph-id = (luv.slug:slug-shaped-glyph-glyph-id placement)
          for glyph = (luv.slug:load-slug-glyph-index glyph-id font-loader)
          for outline = (luv.slug:normalize-slug-glyph-outline glyph)
          do (when (luv.slug:slug-outline-contours outline)
               (let* ((serialized
                        (luv.slug:serialize-slug-outline
                         outline :horizontal-band-count 1
                                 :vertical-band-count 1))
                      (packed
                        (luv.slug:slug-serialized-outline-packed-outline
                         serialized)))
                 (push
                  (make-slug-text-draw
                   :glyph-id glyph-id :serialized serialized
                   :origin-x (* (+ pen-x
                                   (luv.slug:slug-shaped-glyph-x-offset
                                    placement))
                                unit)
                   :origin-y (* (+ pen-y
                                   (luv.slug:slug-shaped-glyph-y-offset
                                    placement))
                                unit)
                   :outline-min-x (luv.slug:slug-packed-outline-min-x packed)
                   :outline-min-y (luv.slug:slug-packed-outline-min-y packed)
                   :outline-max-x (luv.slug:slug-packed-outline-max-x packed)
                   :outline-max-y (luv.slug:slug-packed-outline-max-y packed))
                  draws)))
             (incf pen-x (luv.slug:slug-shaped-glyph-x-advance placement))
             (incf pen-y (luv.slug:slug-shaped-glyph-y-advance placement)))
    (nreverse draws)))

(defun slug-text-extents (draws shaped font-loader)
  (let* ((unit (/ 1 (luv.slug:slug-shaped-text-units-per-em shaped)))
         (advance-x (* unit (luv.slug:slug-shaped-text-x-advance shaped)))
         (advance-y (* unit (luv.slug:slug-shaped-text-y-advance shaped))))
    (values
     (min 0.0
          (loop for draw in draws
                minimize (+ (slug-text-draw-origin-x draw)
                            (slug-text-draw-outline-min-x draw))))
     (min 0.0 (* unit (zpb-ttf:descender font-loader))
          (loop for draw in draws
                minimize (+ (slug-text-draw-origin-y draw)
                            (slug-text-draw-outline-min-y draw))))
     (max advance-x
          (loop for draw in draws
                maximize (+ (slug-text-draw-origin-x draw)
                            (slug-text-draw-outline-max-x draw))))
     (max advance-y (* unit (zpb-ttf:ascender font-loader))
          (loop for draw in draws
                maximize (+ (slug-text-draw-origin-y draw)
                            (slug-text-draw-outline-max-y draw)))))))

(defun make-slug-text-vertices
    (draws width height min-x min-y max-x max-y)
  (let* ((margin 24.0)
         (span-x (max 1/1024 (- max-x min-x)))
         (span-y (max 1/1024 (- max-y min-y)))
         (pixels-per-em
           (min 180.0
                (/ (- width (* margin 2)) span-x)
                (/ (- height (* margin 2)) span-y)))
         (x-offset (/ (- width (* span-x pixels-per-em)) 2))
         (y-offset (/ (- height (* span-y pixels-per-em)) 2))
         (padding (/ 1.0 pixels-per-em))
         (data (make-array (* 54 (length draws))
                           :element-type 'single-float)))
    (labels ((clip-x (x)
               (- (* 2 (/ (+ x-offset (* (- x min-x) pixels-per-em)) width))
                  1))
             (clip-y (y)
               (- 1
                  (* 2
                     (/ (+ y-offset (* (- y min-y) pixels-per-em)) height))))
             (write-vertex (offset clip-x clip-y outline-x outline-y)
               (let ((values (list clip-x clip-y 0.0
                                   outline-x outline-y 0.0
                                   pixels-per-em pixels-per-em 0.0)))
                 (loop for value in values
                       for index from offset
                       do (setf (aref data index)
                                (coerce value 'single-float))))))
      (loop for draw in draws
            for draw-index from 0
            for base = (* draw-index 54)
            for outline-left = (- (slug-text-draw-outline-min-x draw) padding)
            for outline-bottom = (- (slug-text-draw-outline-min-y draw) padding)
            for outline-right = (+ (slug-text-draw-outline-max-x draw) padding)
            for outline-top = (+ (slug-text-draw-outline-max-y draw) padding)
            for layout-left = (+ (slug-text-draw-origin-x draw) outline-left)
            for layout-bottom = (+ (slug-text-draw-origin-y draw) outline-bottom)
            for layout-right = (+ (slug-text-draw-origin-x draw) outline-right)
            for layout-top = (+ (slug-text-draw-origin-y draw) outline-top)
            for left = (clip-x layout-left)
            for bottom = (clip-y layout-bottom)
            for right = (clip-x layout-right)
            for top = (clip-y layout-top)
            do (write-vertex base left bottom outline-left outline-bottom)
               (write-vertex (+ base 9) right bottom outline-right outline-bottom)
               (write-vertex (+ base 18) right top outline-right outline-top)
               (write-vertex (+ base 27) left bottom outline-left outline-bottom)
               (write-vertex (+ base 36) right top outline-right outline-top)
               (write-vertex (+ base 45) left top outline-left outline-top))
      data)))

(defun create-metal-slug-texture-pair (device layout draw resources)
  (let ((serialized (slug-text-draw-serialized draw))
        (band-texture nil) (curve-texture nil)
        (band-view nil) (curve-view nil) (bind-group nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf band-texture
                 (create
                  device
                  (make-texture-descriptor
                   :label "Shaped glyph RG16U bands"
                   :size (luv.slug:slug-serialized-outline-band-texture-size
                          serialized)
                   :dimensions :2d :format :rg16-uint
                   :usage '(:texture-binding :copy-dst)))
                 curve-texture
                 (create
                  device
                  (make-texture-descriptor
                   :label "Shaped glyph RGBA16F curves"
                   :size (luv.slug:slug-serialized-outline-curve-texture-size
                          serialized)
                   :dimensions :2d :format :rgba16-float
                   :usage '(:texture-binding :copy-dst)))
                 band-view
                 (create
                  device (make-texture-view-descriptor :texture band-texture))
                 curve-view
                 (create
                  device (make-texture-view-descriptor :texture curve-texture))
                 bind-group
                 (create
                  device
                  (make-bind-group-descriptor
                   :label "Shaped Slug glyph"
                   :layout layout
                   :entries `((:binding 0 :resource ,band-view)
                              (:binding 1 :resource ,curve-view)))))
           (write-texture
            (device-queue device) (make-texture-copy :texture band-texture)
            (luv.slug:slug-serialized-outline-band-upload-data serialized)
            (make-texture-data-layout
             :bytes-per-row
             (* 4 (luv.slug:slug-serialized-outline-band-width serialized))
             :rows-per-image
             (second
              (luv.slug:slug-serialized-outline-band-texture-size serialized)))
            (luv.slug:slug-serialized-outline-band-texture-size serialized))
           (write-texture
            (device-queue device) (make-texture-copy :texture curve-texture)
            (luv.slug:slug-serialized-outline-curve-upload-data serialized)
            (make-texture-data-layout
             :bytes-per-row
             (* 8 (luv.slug:slug-serialized-outline-curve-width serialized))
             :rows-per-image
             (second
              (luv.slug:slug-serialized-outline-curve-texture-size serialized)))
            (luv.slug:slug-serialized-outline-curve-texture-size serialized))
           (setf (slug-text-draw-band-texture draw) band-texture
                 (slug-text-draw-band-view draw) band-view
                 (slug-text-draw-curve-texture draw) curve-texture
                 (slug-text-draw-curve-view draw) curve-view
                 (slug-text-draw-bind-group draw) bind-group
                 completed-p t)
           (append
            (list bind-group curve-view band-view curve-texture band-texture)
            resources))
      (unless completed-p
        (dolist (resource
                  (remove nil
                          (list bind-group curve-view band-view
                                curve-texture band-texture)))
          (destroy resource))))))

(defun render-metal-slug-text (provider string font-pathname)
  "Shape STRING with HarfBuzz and render one Slug quad per drawable glyph.

The proof target is 768 by 256 pixels.  HarfBuzz owns glyph selection,
ligatures, clusters, advances, and offsets; ZPB-TTF supplies outlines by the
resulting glyph IDs.  Return pixels, width, height, format, and shaped text.
#4G7064"
  (let ((device nil) (vertex-module nil) (fragment-module nil)
        (layout nil) (pipeline nil) (target nil) (vertices nil)
        (readback nil) (encoder nil) (command-buffer nil)
        (glyph-resources nil)
        (width 768) (height 256) (format :rgba8-unorm)
        (shaped (luv.slug:shape-slug-text string font-pathname)))
    (zpb-ttf:with-font-loader (font-loader font-pathname)
      (let ((draws (make-slug-text-draws shaped font-loader)))
        (unless draws
          (error 'luv.slug:slug-shaping-error :reason :no-drawable-glyphs
                 :details string))
        (multiple-value-bind (min-x min-y max-x max-y)
            (slug-text-extents draws shaped font-loader)
          (let ((vertex-data
                  (make-slug-text-vertices
                   draws width height min-x min-y max-x max-y)))
            (unwind-protect
                 (progn
                   (setf device (request-gpu-device provider)
                         vertex-module
                         (create
                          device
                          (make-shader-module-descriptor
                           :label "Shaped Slug vertex" :language :mathematical
                           :code (luv.slug:slug-bezier-vertex-specification)))
                         fragment-module
                         (create
                          device
                          (make-shader-module-descriptor
                           :label "Shaped Slug fragment" :language :mathematical
                           :code (luv.slug:slug-banded-fragment-specification)))
                         layout
                         (create
                          device
                          (make-bind-group-layout-descriptor
                           :label "Shaped Slug outline textures"
                           :entries '((:binding 0 :type :texture)
                                      (:binding 1 :type :texture))))
                         pipeline
                         (create
                          device
                          (make-render-pipeline-descriptor
                           :label "HarfBuzz shaped Slug text"
                           :layout layout
                           :vertex
                           `(:module ,vertex-module
                             :buffers
                             ((:array-stride 36
                               :attributes
                               ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                (:shader-location 1 :offset 12
                                  :format :float32x3)
                                (:shader-location 2 :offset 24
                                  :format :float32x3)))))
                           :fragment
                           `(:module ,fragment-module
                             :targets
                             ((:format ,format
                               :blend :premultiplied-alpha)))))
                         target
                         (create
                          device
                          (make-texture-descriptor
                           :label "Shaped Slug text target"
                           :size (list width height) :dimensions :2d
                           :format format :usage '(:render-attachment :copy-src)))
                         vertices
                         (create
                          device
                          (make-buffer-descriptor
                           :label "Shaped Slug glyph quads"
                           :size (* (length vertex-data) 4)
                           :usage '(:vertex :copy-dst)))
                         readback
                         (create
                          device
                          (make-buffer-descriptor
                           :label "Shaped Slug text readback"
                           :size (* width height 4) :usage '(:copy-dst))))
                   (write-buffer vertices vertex-data)
                   (dolist (draw draws)
                     (setf glyph-resources
                           (create-metal-slug-texture-pair
                            device layout draw glyph-resources)))
                   (setf encoder
                         (create
                          device
                          (make-command-encoder-descriptor
                           :label "Shaped Slug text commands")))
                   (let ((pass
                           (begin-render-pass
                            encoder
                            (make-render-pass-descriptor
                             :color-attachments
                             `((:view ,target :load-op :clear :store-op :store
                                :clear-value #(0.04 0.12 0.20 1.0)))))))
                     (set-pipeline pass pipeline)
                     (set-vertex-buffer pass 0 vertices)
                     (loop for draw in draws
                           for first-vertex from 0 by 6
                           do (set-bind-group
                               pass 0 (slug-text-draw-bind-group draw))
                              (draw pass 6 1 first-vertex))
                     (end-pass pass))
                   (encode
                    encoder
                    (make-gpu-copy-texture-to-buffer-command
                     :source target :destination readback))
                   (setf command-buffer (finish encoder))
                   (submit (device-queue device) command-buffer)
                   (values (read-buffer readback) width height format shaped))
              (when command-buffer (destroy command-buffer))
              (when encoder (destroy encoder))
              (dolist (resource glyph-resources) (destroy resource))
              (when readback (destroy readback))
              (when vertices (destroy vertices))
              (when target (destroy target))
              (when pipeline (destroy pipeline))
              (when layout (destroy layout))
              (when fragment-module (destroy fragment-module))
              (when vertex-module (destroy vertex-module))
              (when device (destroy device)))))))))
