;;; HarfBuzz-shaped Slug text as world-owned render geometry.
;;;
;;; A WORLD-TEXT-RUN is the semantic placement boundary.  A per-device cache
;;; retains HarfBuzz results and normalized outlines.  Each distinct glyph set
;;; is packed into two device atlases and shared by matching runs.

(in-package #:luvcraft)

(defclass world-text-run ()
  ((string :initarg :string :accessor world-text-run-string)
   (font-pathname :initarg :font-pathname
                  :reader world-text-run-font-pathname)
   (shaped-text :initarg :shaped-text :reader world-text-run-shaped-text)
   (glyphs :initarg :glyphs :accessor world-text-run-glyphs)
   (atlas :initarg :atlas :accessor world-text-run-atlas)
   (center :initarg :center :reader world-text-run-center)
   (world-units-per-em :initarg :world-units-per-em
                       :reader world-text-run-world-units-per-em)
   (vertex-data :initarg :vertex-data :reader world-text-run-vertex-data)
   (vertex-buffer :initarg :vertex-buffer
                  :reader world-text-run-vertex-buffer)
   (instance-data :initarg :instance-data
                  :accessor world-text-run-instance-data)
   (instance-buffer :initarg :instance-buffer
                    :accessor world-text-run-instance-buffer)
   (layout :initarg :layout :reader world-text-run-layout)
   (pipeline :initarg :pipeline :reader world-text-run-pipeline)
   (resources :initarg :resources :accessor world-text-run-resources)))

(defun world-text-point (center right up x y scale)
  (make-vec3
   (+ (vec3-x center)
      (* scale (+ (* x (vec3-x right)) (* y (vec3-x up)))))
   (+ (vec3-y center)
      (* scale (+ (* x (vec3-y right)) (* y (vec3-y up)))))
   (+ (vec3-z center)
      (* scale (+ (* x (vec3-z right)) (* y (vec3-z up)))))))

(defun make-world-text-quad-vertices ()
  (make-array
   18 :element-type 'single-float
      :initial-contents
      '(0.0 0.0 0.0  1.0 0.0 0.0  1.0 1.0 0.0
        0.0 0.0 0.0  1.0 1.0 0.0  0.0 1.0 0.0)))

(defun make-world-text-instances
    (glyphs atlas center right up scale min-x min-y max-x max-y
     &key (ink '(0.96 0.32 0.48)))
  "Build one dense model-and-atlas record per drawable glyph occurrence.

INK is the linear RGB carried in the record's three spare lanes."
  (let* ((padding 0.035)
         (middle-x (/ (+ min-x max-x) 2))
         (middle-y (/ (+ min-y max-y) 2))
         (data (make-array (* 24 (length glyphs))
                           :element-type 'single-float)))
    (labels ((difference (end start)
               (make-vec3 (- (vec3-x end) (vec3-x start))
                          (- (vec3-y end) (vec3-y start))
                          (- (vec3-z end) (vec3-z start))))
             (write-values (offset values)
               (loop for value in values
                     for index from offset
                     do (setf (aref data index)
                              (coerce value 'single-float)))))
      (loop for glyph in glyphs
            for base from 0 by 24
            for outline-left = (- (luv.slug:slug-glyph-placement-outline-min-x glyph)
                                  padding)
            for outline-bottom = (- (luv.slug:slug-glyph-placement-outline-min-y glyph)
                                    padding)
            for outline-right = (+ (luv.slug:slug-glyph-placement-outline-max-x glyph)
                                   padding)
            for outline-top = (+ (luv.slug:slug-glyph-placement-outline-max-y glyph)
                                 padding)
            for layout-left = (+ (luv.slug:slug-glyph-placement-origin-x glyph)
                                 outline-left)
            for layout-bottom = (+ (luv.slug:slug-glyph-placement-origin-y glyph)
                                   outline-bottom)
            for layout-right = (+ (luv.slug:slug-glyph-placement-origin-x glyph)
                                  outline-right)
            for layout-top = (+ (luv.slug:slug-glyph-placement-origin-y glyph) outline-top)
            for origin = (world-text-point
                          center right up (- layout-left middle-x)
                          (- layout-bottom middle-y) scale)
            for right-edge = (world-text-point
                              center right up (- layout-right middle-x)
                              (- layout-bottom middle-y) scale)
            for top-edge = (world-text-point
                            center right up (- layout-left middle-x)
                            (- layout-top middle-y) scale)
            for atlas-location =
              (gethash (luv.slug:slug-glyph-placement-resource glyph)
                       (luv.slug:slug-glyph-atlas-locations atlas))
            for serialized =
              (luv.slug:slug-device-glyph-serialized
               (luv.slug:slug-glyph-placement-resource glyph))
            do (write-values
                base
                (list (vec3-x origin) (vec3-y origin) (vec3-z origin)
                      (vec3-x (difference right-edge origin))
                      (vec3-y (difference right-edge origin))
                      (vec3-z (difference right-edge origin))
                      (vec3-x (difference top-edge origin))
                      (vec3-y (difference top-edge origin))
                      (vec3-z (difference top-edge origin))
                      outline-left outline-bottom
                      (luv.slug:slug-serialized-outline-horizontal-band-count
                       serialized)
                      outline-right outline-top
                      (luv.slug:slug-serialized-outline-vertical-band-count
                       serialized)
                      (first atlas-location) (second atlas-location)
                      (first ink)
                      (luv.slug:slug-glyph-placement-outline-min-x glyph)
                      (luv.slug:slug-glyph-placement-outline-min-y glyph)
                      (second ink)
                      (luv.slug:slug-glyph-placement-outline-max-x glyph)
                      (luv.slug:slug-glyph-placement-outline-max-y glyph)
                      (third ink))))
      data)))

(defun world-text-center-before-camera (camera distance lift)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (declare (ignore right))
    (world-text-point (camera-position camera) forward up distance lift 1.0)))

(defun make-world-text-run-from-instances
    (device target-format string font-pathname shaped glyphs atlas center
     world-units-per-em instance-data &key (label "world HarfBuzz Slug text"))
  "Create one owned Slug draw batch from caller-positioned GLYPHS.

The caller owns the semantic placement policy and supplies the dense instance
records.  This is the shared GPU boundary for ordinary shaped runs and the
unified terminal surface in #7ZM22R."
  (let ((vertex-data (make-world-text-quad-vertices))
        (resources nil)
        (pipeline nil)
        (completed-p nil))
    (flet ((keep (resource) (push resource resources) resource))
      (unwind-protect
           (let* ((layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "world Slug text layout"
                       :entries '((:binding 0 :type :texture)
                                  (:binding 1 :type :texture)
                                  (:binding 2 :type :uniform-buffer))))))
                  (vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "world Slug glyph quads"
                       :size (* 4 (length vertex-data))
                       :usage '(:vertex :copy-dst)))))
                  (instance-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "world Slug glyph instances"
                       :size (max 4 (* 4 (length instance-data)))
                       :usage '(:vertex :copy-dst))))))
             (setf pipeline
                   (make-live-shader-pipeline
                    :role :slug-world-text
                    :vertex-role :slug-world-text
                    :label label
                    :device device :layout layout
                    :vertex-buffers
                    '((:array-stride 12
                       :attributes
                       ((:shader-location 0 :offset 0 :format :float32x3)))
                      (:array-stride 96 :step-mode :instance
                       :attributes
                       ((:shader-location 1 :offset 0 :format :float32x3)
                        (:shader-location 2 :offset 12 :format :float32x3)
                        (:shader-location 3 :offset 24 :format :float32x3)
                        (:shader-location 4 :offset 36 :format :float32x3)
                        (:shader-location 5 :offset 48 :format :float32x3)
                        (:shader-location 6 :offset 60 :format :float32x3)
                        (:shader-location 7 :offset 72 :format :float32x3)
                        (:shader-location 8 :offset 84 :format :float32x3))))
                    :target-format target-format
                    :target-blend :premultiplied-alpha
                    :primitive '(:topology :triangle-list)
                    :depth-stencil
                    '(:format :depth32-float
                      :depth-write-enabled nil
                      :depth-compare :less)))
             (write-buffer vertex-buffer vertex-data)
             (when (plusp (length instance-data))
               (write-buffer instance-buffer instance-data))
             (let ((run
                     (make-instance
                      'world-text-run
                      :string string :font-pathname font-pathname
                      :shaped-text shaped :glyphs glyphs :atlas atlas
                      :center center :world-units-per-em world-units-per-em
                      :vertex-data vertex-data :vertex-buffer vertex-buffer
                      :instance-data instance-data
                      :instance-buffer instance-buffer :layout layout
                      :pipeline pipeline :resources resources)))
               (setf completed-p t)
               run))
        (unless completed-p
          (when pipeline
            (release-live-shader-pipeline pipeline))
          (dolist (resource resources)
            (ignore-errors (destroy resource))))))))

(defun make-world-text-run
    (device glyph-cache camera target-format string font-pathname
     &key (distance 8.0) (lift 3.0) (world-units-per-em 0.55))
  "Shape STRING once and create a depth-tested world text run on DEVICE.

The run owns dense placement/model data and its live pipeline; GLYPH-CACHE owns
font-and-glyph device resources reusable across runs.  See #QW7P96."
  (let* ((shaped (luv.slug:cached-slug-shaped-text
                  glyph-cache font-pathname string))
         (center (world-text-center-before-camera camera distance lift)))
    (zpb-ttf:with-font-loader (font-loader font-pathname)
      (let ((glyphs
              (luv.slug:make-slug-glyph-placements
               shaped font-loader glyph-cache font-pathname)))
        (unless glyphs
          (error 'luv.slug:slug-shaping-error
                 :reason :no-drawable-glyphs :details string))
        (multiple-value-bind (min-x min-y max-x max-y)
            (luv.slug:slug-text-extents glyphs shaped font-loader)
          (multiple-value-bind (right up forward) (camera-basis camera)
            (declare (ignore forward))
            (let* ((atlas (luv.slug:slug-glyph-atlas-for glyph-cache glyphs))
                   (instance-data
                     (make-world-text-instances
                      glyphs atlas center right up world-units-per-em
                      min-x min-y max-x max-y)))
              (make-world-text-run-from-instances
               device target-format string font-pathname shaped glyphs atlas
               center world-units-per-em instance-data))))))))

(defun make-world-text-frame-bind-groups (run device uniform-buffer)
  "Bind one shared glyph atlas to one drawable-frame uniform."
  (let* ((atlas (world-text-run-atlas run))
         (groups (make-array 1 :initial-element nil))
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (aref groups 0)
                 (create
                  device
                  (make-bind-group-descriptor
                   :label "world glyph atlas frame bindings"
                   :layout (world-text-run-layout run)
                   :entries
                   `((:binding 0
                      :resource ,(luv.slug:slug-glyph-atlas-band-view atlas))
                     (:binding 1
                      :resource ,(luv.slug:slug-glyph-atlas-curve-view atlas))
                     (:binding 2 :resource ,uniform-buffer)))))
           (setf completed-p t)
           groups)
      (unless completed-p
        (when (aref groups 0) (destroy (aref groups 0)))))))

(defun world-text-run-native-pipeline (run)
  (live-shader-pipeline-native-pipeline (world-text-run-pipeline run)))

(defun replace-world-text-run-instances
    (run device string glyphs atlas instance-data)
  "Publish one complete replacement instance population into RUN.

The caller owns the semantic candidate.  This function creates and fills its
new GPU buffer before installing the new string, glyph, atlas, CPU-data, and
buffer cohort.  Native destruction of the predecessor remains submission-aware
through the backend's ordinary DESTROY contract."
  (let ((buffer nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf buffer
                 (create
                  device
                  (make-buffer-descriptor
                   :label "world Slug glyph instances"
                   :size (max 4 (* 4 (length instance-data)))
                   :usage '(:vertex :copy-dst))))
           (when (plusp (length instance-data))
             (write-buffer buffer instance-data))
           (let ((old-buffer (world-text-run-instance-buffer run))
                 (old-atlas (world-text-run-atlas run)))
             (setf (world-text-run-string run) string
                   (world-text-run-glyphs run) glyphs
                   (world-text-run-atlas run) atlas
                   (world-text-run-instance-data run) instance-data
                   (world-text-run-instance-buffer run) buffer
                   (world-text-run-resources run)
                   (cons buffer
                         (delete old-buffer
                                 (world-text-run-resources run) :test #'eq))
                   completed-p t)
             (destroy old-buffer)
             (values run (not (eq old-atlas atlas)))))
      (unless completed-p
        (when buffer (ignore-errors (destroy buffer)))))))

(defun release-world-text-run (run)
  (release-live-shader-pipeline (world-text-run-pipeline run))
  (dolist (resource (world-text-run-resources run))
    (destroy resource))
  (values))
