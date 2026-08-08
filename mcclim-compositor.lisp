;;; A first deliberately theatrical McCLIM compositor.
;;;
;;; McCLIM still paints an ordinary CPU raster.  The raster mirror uploads it
;;; to a sampled GPU texture; this object renders that texture as a
;;; perspective-spinning quad into an offscreen color attachment, after which
;;; the canvas context performs its familiar texture-to-swapchain copy.

(in-package #:luv.mcclim)

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
   (layout
    :initform nil
    :accessor spinning-compositor-layout)
   (pipeline
    :initform nil
    :accessor spinning-compositor-pipeline)
   (bind-group
    :initform nil
    :accessor spinning-compositor-bind-group)))

(defun spinning-compositor-resources (compositor)
  (remove nil
          (list (spinning-compositor-bind-group compositor)
                (spinning-compositor-pipeline compositor)
                (spinning-compositor-layout compositor)
                (spinning-compositor-fragment-module compositor)
                (spinning-compositor-vertex-module compositor)
                (spinning-compositor-sampler compositor)
                (spinning-compositor-output-view compositor)
                (spinning-compositor-source-view compositor)
                (spinning-compositor-output compositor))))

(defun clear-spinning-compositor-resources (compositor)
  (dolist (resource (spinning-compositor-resources compositor))
    (luv:destroy resource))
  (setf (spinning-compositor-device compositor) nil
        (spinning-compositor-source compositor) nil
        (spinning-compositor-size compositor) nil
        (spinning-compositor-output compositor) nil
        (spinning-compositor-source-view compositor) nil
        (spinning-compositor-output-view compositor) nil
        (spinning-compositor-sampler compositor) nil
        (spinning-compositor-vertex-module compositor) nil
        (spinning-compositor-fragment-module compositor) nil
        (spinning-compositor-layout compositor) nil
        (spinning-compositor-pipeline compositor) nil
        (spinning-compositor-bind-group compositor) nil)
  compositor)

(defmethod release-raster-mirror-compositor
    ((compositor spinning-texture-compositor))
  (clear-spinning-compositor-resources compositor)
  (values))

(defun ensure-spinning-compositor-resources
    (compositor context source)
  (let* ((device (luv:context-device context))
         (size (luv:gpu-texture-size source))
         (format (luv:gpu-texture-format source)))
    (unless (and (eq device (spinning-compositor-device compositor))
                 (eq source (spinning-compositor-source compositor))
                 (equal size (spinning-compositor-size compositor)))
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
                        :code (spv:spinning-texture-vertex-shader))))
                    (fragment-module
                      (create-resource
                       (luv:make-shader-module-descriptor
                        :label "spinning McCLIM fragment shader"
                        :code (spv:spinning-texture-fragment-shader))))
                    (layout
                      (create-resource
                       (luv:make-bind-group-layout-descriptor
                        :label "sampled McCLIM texture layout"
                        :entries '((:binding 0 :type :texture)
                                   (:binding 1 :type :sampler)))))
                    (pipeline
                      (create-resource
                       (luv:make-render-pipeline-descriptor
                        :label "spinning McCLIM quad"
                        :layout layout
                        :vertex `(:module ,vertex-module
                                  :entry-point "main")
                        :fragment `(:module ,fragment-module
                                    :entry-point "main"
                                    :targets ((:format ,format)))
                        :primitive '(:topology :triangle-strip))))
                    (bind-group
                      (create-resource
                       (luv:make-bind-group-descriptor
                        :label "sampled McCLIM raster"
                        :layout layout
                        :entries `((:binding 0 :resource ,source-view)
                                   (:binding 1 :resource ,sampler))))))
               (setf (spinning-compositor-device compositor) device
                     (spinning-compositor-source compositor) source
                     (spinning-compositor-size compositor) size
                     (spinning-compositor-output compositor) output
                     (spinning-compositor-source-view compositor) source-view
                     (spinning-compositor-output-view compositor) output-view
                     (spinning-compositor-sampler compositor) sampler
                     (spinning-compositor-vertex-module compositor)
                     vertex-module
                     (spinning-compositor-fragment-module compositor)
                     fragment-module
                     (spinning-compositor-layout compositor) layout
                     (spinning-compositor-pipeline compositor) pipeline
                     (spinning-compositor-bind-group compositor) bind-group
                     completed-p t)))
          (unless completed-p
            (dolist (resource created)
              (ignore-errors (luv:destroy resource))))))))
  compositor)

(defun linear-to-srgb (value)
  (if (<= value 0.0031308)
      (* 12.92 value)
      (- (* 1.055 (expt value (/ 1.0 2.4))) 0.055)))

(defun normalized-animation-byte (value srgb-p)
  (let ((normalized (/ (+ value 1.0) 2.0)))
    (max 0
         (min 255
              (round (* 255 (if srgb-p
                                (linear-to-srgb normalized)
                                normalized)))))))

(defun pack-spinning-state-texel (format sine cosine)
  (let* ((srgb-p (member format '(:rgba8-unorm-srgb :bgra8-unorm-srgb)))
         (s (normalized-animation-byte sine srgb-p))
         (c (normalized-animation-byte cosine srgb-p)))
    (ecase format
      ((:rgba8-unorm :rgba8-unorm-srgb)
       (logior s (ash c 8) (ash #xff 24)))
      ((:bgra8-unorm :bgra8-unorm-srgb)
       (logior (ash c 8) (ash s 16) (ash #xff 24))))))

(defun update-spinning-compositor-state
    (compositor context source timestamp)
  (unless (spinning-compositor-start-time compositor)
    (setf (spinning-compositor-start-time compositor) timestamp))
  (let* ((phase (* 2 pi (spinning-compositor-speed compositor)
                   (- timestamp
                      (spinning-compositor-start-time compositor))))
         (pixel
           (make-array
            '(1 1) :element-type '(unsigned-byte 32)
            :initial-element
            (pack-spinning-state-texel
             (luv:gpu-texture-format source) (sin phase) (cos phase)))))
    (luv:write-texture
     (luv:device-queue (luv:context-device context))
     (luv:make-texture-copy :texture source :origin '(0 0 0))
     pixel
     (luv:make-texture-data-layout :bytes-per-row 4 :rows-per-image 1)
     '(1 1))))

(defun render-spinning-mirror-frame (mirror timestamp)
  (let ((context (mirror-context mirror))
        (source (mirror-texture mirror))
        (compositor (mirror-compositor mirror)))
    (when (and context source (typep compositor 'spinning-texture-compositor)
               (eq :open (luv:canvas-state (mirror-target mirror))))
      (ensure-spinning-compositor-resources compositor context source)
      (update-spinning-compositor-state compositor context source timestamp)
      (luv:present-canvas-frame
       context
       (lambda (surface encoder)
         (let ((pass
                 (luv:begin-render-pass
                  encoder
                  (luv:make-render-pass-descriptor
                   :label "McCLIM offscreen pass"
                   :color-attachments
                   `((:view ,(spinning-compositor-output-view compositor)
                      :load-op :clear :store-op :store
                      :clear-value #(0.025 0.025 0.04 1.0)))))))
           (luv:set-pipeline pass
                             (spinning-compositor-pipeline compositor))
           (luv:set-bind-group pass 0
                               (spinning-compositor-bind-group compositor))
           (luv:draw pass 4)
           (luv:end-pass pass))
         (luv:encode
          encoder
          (luv:make-gpu-copy-texture-command
           :source (spinning-compositor-output compositor)
           :destination surface))))))
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
  (let* ((frame (open-widget-lab :title title))
         (sheet (frame-top-level-sheet frame))
         (mirror (sheet-direct-mirror sheet)))
    (enable-spinning-mirror
     mirror :frames-per-second frames-per-second :speed speed)
    frame))
