(in-package #:luft.render)

;;; Exposure turns scene radiance into a viewable brightness. The renderer
;;; consumes a value before drawing, then offers the finished HDR image for
;;; measurement. Fixed exposure does no GPU work; automatic exposure measures
;;; a small image and adapts to completed measurements without waiting.
;;;
;;; Read this file from the interface, through its two implementations, to
;;; the automatic meter's construction and numerical details. See #I4PRMD.

(defclass exposure-control () ()
  (:documentation "A renderer-owned brightness policy with an independent lifetime."))

(defgeneric exposure-value (control)
  (:documentation "The current positive exposure multiplier."))

(defgeneric advance-exposure (control)
  (:documentation "Consume at most one completed measurement and return exposure."))

(defgeneric make-exposure-binding (control device source-view sampler)
  (:documentation
   "Return a fresh GPU bind group for SOURCE-VIEW, or NIL when none is needed.
The caller owns this binding and releases it before the view or CONTROL.
CONTROL borrows neither argument beyond the lifetime of that binding. Resize
can stage a new binding without changing the installed control or its queue."))

(defgeneric encode-exposure (control encoder binding frame-index)
  (:documentation
   "Measure the HDR source named by BINDING after its rendering has finished.
FRAME-INDEX increases each frame. This operation must never wait for readback."))

(defgeneric release-exposure (control)
  (:documentation
   "Release owned resources; repeated calls retry failures and otherwise do nothing.
The caller must release source bindings first and stop using CONTROL."))

;;; A fixed policy is also the way to omit the entire measurement subsystem.

(defclass fixed-exposure (exposure-control)
  ((value :initarg :value :reader exposure-value)))

(defun checked-exposure-value (value)
  "Normalize one exposure multiplier before constructing any GPU resources."
  (unless (and (realp value) (> value 0)
               (<= value most-positive-single-float))
    (error "Exposure must be a positive finite single-float value, not ~S." value))
  (let ((single (coerce value 'single-float)))
    (unless (plusp single)
      (error "Exposure ~S is too small to represent." value))
    single))

(defun make-fixed-exposure (value)
  "Make a constant exposure control without allocating GPU resources."
  (make-instance 'fixed-exposure :value (checked-exposure-value value)))

(defmethod advance-exposure ((control fixed-exposure))
  (exposure-value control))

(defmethod make-exposure-binding ((control fixed-exposure) device source-view sampler)
  (declare (ignore device source-view sampler))
  nil)

(defmethod encode-exposure ((control fixed-exposure) encoder binding frame-index)
  (declare (ignore encoder binding frame-index))
  (values))

(defmethod release-exposure ((control fixed-exposure))
  (values))

;;; Automatic exposure owns one small render target and a bounded readback
;;; queue. Each queue entry keeps its buffer and submission age together.

(defconstant +exposure-probe-width+ 32)
(defconstant +exposure-probe-height+ 16)
(defconstant +exposure-probe-buffer-count+ 3)
(defconstant +exposure-probe-byte-count+
  (* 4 +exposure-probe-width+ +exposure-probe-height+))

(defstruct exposure-readback
  buffer
  (frame nil :type (or null (integer 0 *))))

(defclass automatic-exposure (exposure-control)
  ((value :initform 1.0f0 :accessor exposure-value)
   (layout :initform nil :accessor exposure-layout)
   (texture :initform nil :accessor exposure-texture)
   (view :initform nil :accessor exposure-view)
   (pipeline :initform nil :accessor exposure-pipeline)
   (readbacks :initform #() :accessor exposure-readbacks)
   ;; This is the sole release inventory, populated at allocation time. Named
   ;; fields above are the operational view; no caller enumerates either.
   (resources :initform nil :accessor exposure-resources)))

(defmethod advance-exposure ((control automatic-exposure))
  (let ((oldest nil))
    (loop for entry across (exposure-readbacks control)
          for frame = (exposure-readback-frame entry)
          when (and frame
                    (or (null oldest)
                        (< frame (exposure-readback-frame oldest))))
            do (setf oldest entry))
    (when oldest
      (multiple-value-bind (bytes ready-p)
          (read-buffer-if-ready (exposure-readback-buffer oldest))
        (when ready-p
          ;; Consume one measurement per frame, even after a CPU pause.
          (setf (exposure-value control)
                (adapted-exposure (exposure-value control)
                                  (exposure-probe-average-luminance bytes))
                (exposure-readback-frame oldest) nil)))))
  (exposure-value control))

(defmethod make-exposure-binding
    ((control automatic-exposure) device source-view sampler)
  (create device
          (make-bind-group-descriptor
           :label "luft exposure probe source"
           :layout (exposure-layout control)
           :entries `((:binding 0 :resource ,source-view)
                      (:binding 1 :resource ,sampler)))))

(defmethod encode-exposure
    ((control automatic-exposure) encoder binding frame-index)
  (let* ((readbacks (exposure-readbacks control))
         (entry (aref readbacks (mod frame-index (length readbacks)))))
    ;; A slow GPU leaves this slot occupied. Retain the last value rather than
    ;; overwrite work in flight or stall the renderer.
    (unless (exposure-readback-frame entry)
      (let ((pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft exposure probe"
                :color-attachments
                `((:view ,(exposure-view control)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))))))
        (set-pipeline pass (exposure-pipeline control))
        (set-bind-group pass 0 binding)
        (draw pass 3)
        (end-pass pass))
      (encode encoder
              (make-gpu-copy-texture-to-buffer-command
               :source (exposure-texture control)
               :destination (exposure-readback-buffer entry)))
      (setf (exposure-readback-frame entry) frame-index))))

(defmethod release-exposure ((control automatic-exposure))
  (with-release-report
    (let ((retained nil))
      (dolist (resource (exposure-resources control))
        (let ((released-p nil))
          (releasing :exposure-resource
            (destroy resource)
            (setf released-p t))
          (unless released-p (push resource retained))))
      (setf (exposure-resources control) (nreverse retained))))
  (values))

;;; Construction records each allocation immediately, including each buffer
;;; in the queue. The same release operation handles partial construction.

(defun make-automatic-exposure (device)
  "Make a complete automatic meter, rolling back every partial allocation."
  (let ((control (make-instance 'automatic-exposure))
        (completed-p nil))
    (flet ((own (descriptor)
             (let ((resource (create device descriptor)))
               (push resource (exposure-resources control))
               resource)))
      (unwind-protect
           (progn
             (setf (exposure-layout control)
                   (own (make-bind-group-layout-descriptor
                         :label "luft exposure probe layout"
                         :entries '((:binding 0 :type :texture)
                                    (:binding 1 :type :sampler))))
                   (exposure-texture control)
                   (own (make-texture-descriptor
                         :label "luft exposure log luminance"
                         :size (list +exposure-probe-width+ +exposure-probe-height+)
                         :dimensions :2d :format :rgba8-unorm
                         :usage '(:render-attachment :copy-src)))
                   (exposure-view control)
                   (own (make-texture-view-descriptor
                         :texture (exposure-texture control)))
                   (exposure-readbacks control)
                   (coerce
                    (loop repeat +exposure-probe-buffer-count+
                          collect
                          (make-exposure-readback
                           :buffer (own (make-buffer-descriptor
                                         :label "luft exposure readback"
                                         :size +exposure-probe-byte-count+
                                         :usage '(:copy-dst)))))
                    'vector))
             (let ((vertex
                     (own (make-shader-module-descriptor
                           :label "luft exposure vertex"
                           :language :mathematical
                           :code (shaders:present-vertex-specification))))
                   (fragment
                     (own (make-shader-module-descriptor
                           :label "luft exposure probe fragment"
                           :language :mathematical
                           :code (shaders:exposure-probe-fragment-specification)))))
               (setf (exposure-pipeline control)
                     (own (make-render-pipeline-descriptor
                           :label "luft exposure probe pipeline"
                           :layout (exposure-layout control)
                           :vertex `(:module ,vertex)
                           :fragment `(:module ,fragment :targets ((:format :rgba8-unorm)))
                           :primitive '(:topology :triangle-list)))))
             (setf completed-p t)
             control)
        (unless completed-p
          (with-release-warnings
            (releasing :exposure-construction (release-exposure control))))))))

;;; The probe stores log luminance, so averaging bytes approximates a
;;; geometric mean. Adaptation is intentionally slower toward a brighter
;;; exposure than toward a darker one.

(defun exposure-probe-average-luminance (bytes)
  (unless (= (length bytes) +exposure-probe-byte-count+)
    (error "LUFT exposure probe returned ~D bytes, expected ~D."
           (length bytes) +exposure-probe-byte-count+))
  (let ((sum 0d0))
    (loop for index from 0 below (length bytes) by 4
          do (incf sum (aref bytes index)))
    (let* ((count (* +exposure-probe-width+ +exposure-probe-height+))
           (encoded (/ sum (* count 255d0)))
           (average-log (- (* encoded 11.98293d0) 9.21034d0)))
      (exp average-log))))

(defun adapted-exposure (current average-luminance)
  (let* ((target (max 0.55f0
                      (min 1.9f0
                           (/ 0.16f0 (coerce average-luminance 'single-float)))))
         (rate (if (< target current) 0.10f0 0.04f0)))
    (+ current (* (- target current) rate))))

(in-package #:luft.render.shaders)

(define-live-shader exposure-probe-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity
                        :sample-components
                        ((:xyz :quantity quantities:scene-radiance
                          :unit :one)))
                 (scene-sampler :sampler :binding 1)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         ;; Match Moppe's broad five-tap probe footprint before the 32x16
         ;; reduction. Encoding log luminance into UNORM makes the geometric
         ;; mean portable through the HAL's compact RGBA8 readback contract.
         (offset (vec2 0.008 0.014))
         (average
           (* (+ (swizzle (sample scene scene-sampler uv) :xyz)
                 (swizzle (sample scene scene-sampler (+ uv offset)) :xyz)
                 (swizzle (sample scene scene-sampler (- uv offset)) :xyz)
                 (swizzle
                  (sample scene scene-sampler
                          (+ uv (vec2 (swizzle offset :x)
                                      (- (swizzle offset :y))))) :xyz)
                 (swizzle
                  (sample scene scene-sampler
                          (+ uv (vec2 (- (swizzle offset :x))
                                      (swizzle offset :y)))) :xyz))
              0.2))
         (luminance
           (max
            (scene-relative-luminance average)
            (quantity 0.0001 :quantity quantities:scene-luminance
                             :unit :one)))
         ;; SCENE-LUMINANCE is relative to reference white 1.0.  LOG is the
         ;; explicit nonlinear encoding boundary, so only its normalized
         ;; representation enters the portable UNORM reduction.
         (encoded
           (clamp (/ (+ (log (representation luminance)) 9.21034)
                     11.98293)
                  0.0 1.0)))
    (set-output color-output (vec4 encoded encoded encoded 1.0))))
