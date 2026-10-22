;;; Temporal antialiasing for the block world.
;;;
;;; The scene is drawn with a subpixel camera jitter at the frame pacer's
;;; render extent, and a resolve accumulates it into the colour image
;;; presentation reads, at the full-budget extent.  When the pacer sheds
;;; resolution the resolve upscales; at full size it is plain antialiasing.
;;; The shader side, and why motion comes from depth, is in
;;; TEMPORAL-SHADERS.LISP.

(in-package #:luvcraft)

(defparameter *luvcraft-temporal-p* t
  "Whether the block world draws jittered and resolves temporally.

Takes effect at the next reframing, when the frame-sized images are rebuilt.")

(defparameter *luvcraft-temporal-history-weight* 0.9
  "How much of a still pixel's colour comes from accumulated history.")

(defconstant +luvcraft-temporal-uniform-floats+ 48)

(defun luvcraft-temporal-halton (index base)
  (loop with fraction = (/ 1.0 base)
        with value = 0.0
        while (plusp index)
        do (incf value (* fraction (mod index base)))
           (setf index (floor index base)
                 fraction (/ fraction base))
        finally (return value)))

(defun luvcraft-temporal-jitter-at (frame-index width height)
  "The FRAME-INDEX offset of a sixteen-phase Halton(2,3) sequence, in clip units."
  (let ((phase (1+ (mod frame-index 16))))
    (list (/ (* 2.0 (- (luvcraft-temporal-halton phase 2) 0.5)) (max width 1))
          (/ (* 2.0 (- (luvcraft-temporal-halton phase 3) 0.5)) (max height 1)))))

(defun shear-luvcraft-camera-lanes (lanes jitter-x jitter-y)
  "Return camera LANES whose projection lands JITTER clip units aside.

Every block-world vertex stage projects with clip x = (relative . right) X-SCALE
and clip y = -(relative . up) Y-SCALE over w = relative . forward.  Adding
forward to right and up in proportion to the jitter therefore displaces every
projected point by exactly that much, without touching a shader."
  (let ((lanes (copy-seq lanes))
        (x-share (/ jitter-x (aref lanes 16)))
        (y-share (- (/ jitter-y (aref lanes 17)))))
    (dotimes (axis 3)
      (let ((forward (aref lanes (+ 12 axis))))
        (incf (aref lanes (+ 4 axis)) (* x-share forward))
        (incf (aref lanes (+ 8 axis)) (* y-share forward))))
    lanes))

(defun luvcraft-camera-lanes-continuous-p (then now)
  "Whether the camera moved from THEN to NOW rather than cut."
  (and then
       (< (loop for axis below 3
                sum (expt (- (aref now axis) (aref then axis)) 2))
          64.0)
       (> (loop for axis below 3
                sum (* (aref now (+ 12 axis)) (aref then (+ 12 axis))))
          0.5)))

(defclass luvcraft-temporal ()
  ((neighbourhood-layout :initarg :neighbourhood-layout
                         :accessor luvcraft-temporal-neighbourhood-layout)
   (resolve-layout :initarg :resolve-layout
                   :accessor luvcraft-temporal-resolve-layout)
   (neighbourhood-pipeline :initarg :neighbourhood-pipeline
                           :accessor luvcraft-temporal-neighbourhood-pipeline)
   (resolve-pipeline :initarg :resolve-pipeline
                     :accessor luvcraft-temporal-resolve-pipeline)
   ;; Each presentation slot owns its uniform buffer and the bind groups
   ;; naming the attachments they were made for.
   (slots :initform (make-canvas-frame-resource-cache)
          :reader luvcraft-temporal-slots)
   (frame-index :initform 0 :accessor luvcraft-temporal-frame-index)
   ;; This frame's unjittered camera, and the last resolved frame's.
   (lanes :initform nil :accessor luvcraft-temporal-lanes)
   (previous-lanes :initform nil :accessor luvcraft-temporal-previous-lanes)
   (jitter :initform '(0.0 0.0) :accessor luvcraft-temporal-jitter)
   ;; History is only history of the image it was resolved into.  A
   ;; scene-only reframing carries that image over, and with it the history.
   (history-texture :initform nil
                    :accessor luvcraft-temporal-history-texture))
  (:documentation
   "Owns the block world's temporal pipelines, bindings, and history state."))

(defstruct (luvcraft-temporal-slot
            (:constructor make-luvcraft-temporal-slot (uniform-buffer)))
  uniform-buffer
  attachments
  neighbourhood-bind-group
  resolve-bind-group)

(defmethod luvcraft-overlay-stage ((temporal luvcraft-temporal))
  (declare (ignore temporal))
  :none)

(defmethod luvcraft-overlay-live-shader-pipelines ((temporal luvcraft-temporal))
  (remove nil (list (luvcraft-temporal-neighbourhood-pipeline temporal)
                    (luvcraft-temporal-resolve-pipeline temporal))))

(defun destroy-luvcraft-temporal-slot-bindings (slot)
  (dolist (group (list (luvcraft-temporal-slot-neighbourhood-bind-group slot)
                       (luvcraft-temporal-slot-resolve-bind-group slot)))
    (when group (destroy group)))
  (setf (luvcraft-temporal-slot-neighbourhood-bind-group slot) nil
        (luvcraft-temporal-slot-resolve-bind-group slot) nil
        (luvcraft-temporal-slot-attachments slot) nil))

(defun destroy-luvcraft-temporal-slot (slot)
  (destroy-luvcraft-temporal-slot-bindings slot)
  (destroy (luvcraft-temporal-slot-uniform-buffer slot)))

(defmethod release-luvcraft-overlay ((temporal luvcraft-temporal))
  (destroy-canvas-frame-resource-cache
   (luvcraft-temporal-slots temporal) #'destroy-luvcraft-temporal-slot)
  (dolist (pipeline (luvcraft-overlay-live-shader-pipelines temporal))
    (release-live-shader-pipeline pipeline))
  (dolist (layout (list (luvcraft-temporal-neighbourhood-layout temporal)
                        (luvcraft-temporal-resolve-layout temporal)))
    (when layout (destroy layout)))
  (setf (luvcraft-temporal-neighbourhood-pipeline temporal) nil
        (luvcraft-temporal-resolve-pipeline temporal) nil
        (luvcraft-temporal-neighbourhood-layout temporal) nil
        (luvcraft-temporal-resolve-layout temporal) nil)
  (values))

(defun luvcraft-session-temporal (session)
  (find-if (lambda (overlay) (typep overlay 'luvcraft-temporal))
           (luvcraft-session-overlays session)))

(defun luvcraft-temporal-active-p (session)
  "Whether this frame's attachments were built for temporal resolve."
  (and (luvcraft-session-temporal session)
       (getf (luvcraft-renderer-frame-attachments
              (luvcraft-session-renderer session))
             :scene-view)))

(defun attach-luvcraft-temporal (session)
  "Create SESSION's temporal pipelines and register their owner."
  (let* ((device (luvcraft-session-device session))
         (temporal (make-instance 'luvcraft-temporal
                                  :neighbourhood-layout nil :resolve-layout nil
                                  :neighbourhood-pipeline nil :resolve-pipeline nil))
         (completed-p nil)
         (vertex-buffers
           '((:array-stride 12
              :attributes ((:shader-location 0 :offset 0 :format :float32x3))))))
    (unwind-protect
         (progn
           (setf (luvcraft-temporal-neighbourhood-layout temporal)
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "block world temporal neighbourhood layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :texture)
                                     (:binding 2 :type :texture)
                                     (:binding 3 :type :uniform-buffer))))
                 (luvcraft-temporal-resolve-layout temporal)
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "block world temporal resolve layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :texture)
                                     (:binding 2 :type :texture)
                                     (:binding 3 :type :texture)
                                     (:binding 4 :type :sampler)
                                     (:binding 5 :type :uniform-buffer))))
                 (luvcraft-temporal-neighbourhood-pipeline temporal)
                 (make-live-shader-pipeline
                  :role :temporal-neighbourhood :vertex-role :focus-post
                  :label "block world temporal neighbourhood pipeline"
                  :device device
                  :layout (luvcraft-temporal-neighbourhood-layout temporal)
                  :vertex-buffers vertex-buffers
                  :target-format (list +luvcraft-scene-color-format+
                                       +luvcraft-scene-color-format+)
                  :primitive '(:topology :triangle-list)
                  :depth-stencil nil)
                 (luvcraft-temporal-resolve-pipeline temporal)
                 (make-live-shader-pipeline
                  :role :temporal-resolve :vertex-role :focus-post
                  :label "block world temporal resolve pipeline"
                  :device device
                  :layout (luvcraft-temporal-resolve-layout temporal)
                  :vertex-buffers vertex-buffers
                  :target-format +luvcraft-scene-color-format+
                  :primitive '(:topology :triangle-list)
                  :depth-stencil nil))
           (setf completed-p t)
           (add-luvcraft-overlay session temporal)
           temporal)
      (unless completed-p
        (ignore-errors (release-luvcraft-overlay temporal))))))

(defun luvcraft-temporal-camera-lanes (session width height)
  "Return this frame's jittered camera lanes, remembering the unjittered ones."
  (let* ((temporal (luvcraft-session-temporal session))
         (lanes (camera-uniform-data (luvcraft-session-camera session)
                                     width height))
         (jitter (luvcraft-temporal-jitter-at
                  (luvcraft-temporal-frame-index temporal) width height)))
    (setf (luvcraft-temporal-lanes temporal) lanes
          (luvcraft-temporal-jitter temporal) jitter)
    (shear-luvcraft-camera-lanes lanes (first jitter) (second jitter))))

(defun luvcraft-temporal-history-valid-p (temporal attachments)
  (and (eq (getf attachments :history-texture)
           (luvcraft-temporal-history-texture temporal))
       (luvcraft-camera-lanes-continuous-p
        (luvcraft-temporal-previous-lanes temporal)
        (luvcraft-temporal-lanes temporal))))

(defun luvcraft-temporal-uniform-data (temporal attachments valid-p)
  (let* ((now (luvcraft-temporal-lanes temporal))
         (then (if valid-p (luvcraft-temporal-previous-lanes temporal) now))
         (render (getf attachments :render-extent))
         (output (getf attachments :output-extent))
         (data (make-array +luvcraft-temporal-uniform-floats+
                           :element-type 'single-float :initial-element 0.0)))
    (replace data now)
    (setf (aref data 3) (if valid-p
                            (coerce *luvcraft-temporal-history-weight*
                                    'single-float)
                            0.0))
    (replace data then :start1 20)
    (setf (aref data 23) 0.0
          (aref data 40) (coerce (first (luvcraft-temporal-jitter temporal))
                                 'single-float)
          (aref data 41) (coerce (second (luvcraft-temporal-jitter temporal))
                                 'single-float)
          (aref data 44) (coerce (first render) 'single-float)
          (aref data 45) (coerce (second render) 'single-float)
          (aref data 46) (coerce (first output) 'single-float)
          (aref data 47) (coerce (second output) 'single-float))
    data))

(defun luvcraft-temporal-slot-for (session temporal surface-texture attachments)
  "Acquire this presentation slot's buffer and bindings for ATTACHMENTS."
  (let* ((device (luvcraft-session-device session))
         (slot (canvas-frame-resource
                (luvcraft-temporal-slots temporal)
                (luvcraft-session-context session) surface-texture
                (lambda (key surface)
                  (declare (ignore key surface))
                  (make-luvcraft-temporal-slot
                   (create device
                           (make-buffer-descriptor
                            :label "block world temporal uniform"
                            :size (* 4 +luvcraft-temporal-uniform-floats+)
                            :usage '(:uniform))))))))
    (unless (eq attachments (luvcraft-temporal-slot-attachments slot))
      ;; This slot's last frame has completed, so its old bindings are free.
      (destroy-luvcraft-temporal-slot-bindings slot)
      (let ((buffer (luvcraft-temporal-slot-uniform-buffer slot)))
        (setf (luvcraft-temporal-slot-neighbourhood-bind-group slot)
              (create device
                      (make-bind-group-descriptor
                       :label "block world temporal neighbourhood bindings"
                       :layout (luvcraft-temporal-neighbourhood-layout temporal)
                       :entries
                       `((:binding 0 :resource ,(getf attachments :scene-view))
                         (:binding 1 :resource ,(getf attachments :world-view))
                         (:binding 2 :resource ,(getf attachments :depth-view))
                         (:binding 3 :resource ,buffer))))
              (luvcraft-temporal-slot-resolve-bind-group slot)
              (create device
                      (make-bind-group-descriptor
                       :label "block world temporal resolve bindings"
                       :layout (luvcraft-temporal-resolve-layout temporal)
                       :entries
                       `((:binding 0 :resource ,(getf attachments :scene-view))
                         (:binding 1 :resource ,(getf attachments :bounds-low-view))
                         (:binding 2 :resource ,(getf attachments :bounds-high-view))
                         (:binding 3 :resource ,(getf attachments :history-view))
                         (:binding 4 :resource ,(luvcraft-session-linear-sampler
                                                 session))
                         (:binding 5 :resource ,buffer))))
              (luvcraft-temporal-slot-attachments slot) attachments)))
    slot))

(defun encode-luvcraft-temporal-resolve (session encoder surface-texture)
  "Resolve the jittered scene into the colour image presentation reads."
  (let* ((temporal (luvcraft-session-temporal session))
         (attachments (luvcraft-renderer-frame-attachments
                       (luvcraft-session-renderer session)))
         (slot (luvcraft-temporal-slot-for session temporal surface-texture
                                           attachments))
         (valid-p (luvcraft-temporal-history-valid-p temporal attachments))
         (sky (luvcraft-session-sky-vertex-buffer session)))
    (write-buffer (luvcraft-temporal-slot-uniform-buffer slot)
                  (luvcraft-temporal-uniform-data temporal attachments valid-p))
    (dolist (key '(:scene-texture :world-texture :depth-texture))
      (prepare-texture encoder (getf attachments key) :texture-binding))
    (flet ((fullscreen (label targets pipeline bind-group)
             (let ((pass
                     (begin-render-pass
                      encoder
                      (make-render-pass-descriptor
                       :label label
                       :color-attachments
                       (loop for view in targets
                             collect `(:view ,view :load-op :clear
                                       :store-op :store
                                       :clear-value #(0.0 0.0 0.0 0.0)))
                       :depth-stencil-attachment nil))))
               (set-pipeline pass (live-shader-pipeline-native-pipeline pipeline))
               (set-bind-group pass 0 bind-group)
               (set-vertex-buffer pass 0 sky)
               (draw pass 3)
               (end-pass pass))))
      (fullscreen "block world temporal neighbourhood"
                  (list (getf attachments :bounds-low-view)
                        (getf attachments :bounds-high-view))
                  (luvcraft-temporal-neighbourhood-pipeline temporal)
                  (luvcraft-temporal-slot-neighbourhood-bind-group slot))
      (prepare-texture encoder (getf attachments :bounds-low-texture)
                       :texture-binding)
      (prepare-texture encoder (getf attachments :bounds-high-texture)
                       :texture-binding)
      ;; Weight zero discards history, but never-written memory may hold
      ;; NaNs, which survive any weight.
      (unless valid-p
        (encode encoder
                (make-gpu-clear-texture-command
                 :texture (getf attachments :history-texture)
                 :color #(0.0 0.0 0.0 0.0))))
      (prepare-texture encoder (getf attachments :history-texture)
                       :texture-binding)
      (fullscreen "block world temporal resolve"
                  (list (getf attachments :color-view))
                  (luvcraft-temporal-resolve-pipeline temporal)
                  (luvcraft-temporal-slot-resolve-bind-group slot)))
    ;; The resolved image is presentation's input and next frame's history.
    (encode encoder
            (make-gpu-copy-texture-command
             :source (getf attachments :color-texture)
             :destination (getf attachments :history-texture)))
    (setf (luvcraft-temporal-previous-lanes temporal)
          (luvcraft-temporal-lanes temporal)
          (luvcraft-temporal-history-texture temporal)
          (getf attachments :history-texture))
    (incf (luvcraft-temporal-frame-index temporal))
    temporal))
