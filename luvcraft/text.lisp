;;; HarfBuzz-shaped Slug text as world-owned render geometry.
;;;
;;; A WORLD-TEXT-RUN is the semantic placement boundary.  A per-device cache
;;; retains HarfBuzz results and normalized outlines.  Each distinct glyph set
;;; is packed into two device atlases and shared by matching runs.

(in-package #:luvcraft)

(defstruct world-text-glyph
  glyph-id resource origin-x origin-y
  outline-min-x outline-min-y outline-max-x outline-max-y)

(defstruct world-text-glyph-resource
  key glyph-id serialized)

(defstruct world-text-glyph-atlas
  key locations band-texel-count curve-texel-count
  band-texture band-view curve-texture curve-view)

(defclass world-text-glyph-cache ()
  ((device :initarg :device :reader world-text-glyph-cache-device)
   (entries :initform (make-hash-table :test #'equal)
            :reader world-text-glyph-cache-entries)
   (atlases :initform (make-hash-table :test #'equal)
            :reader world-text-glyph-cache-atlases)
   (shaped-texts :initform (make-hash-table :test #'equal)
                 :reader world-text-glyph-cache-shaped-texts)))

(defclass world-text-run ()
  ((string :initarg :string :reader world-text-run-string)
   (font-pathname :initarg :font-pathname
                  :reader world-text-run-font-pathname)
   (shaped-text :initarg :shaped-text :reader world-text-run-shaped-text)
   (glyphs :initarg :glyphs :reader world-text-run-glyphs)
   (atlas :initarg :atlas :reader world-text-run-atlas)
   (center :initarg :center :reader world-text-run-center)
   (world-units-per-em :initarg :world-units-per-em
                       :reader world-text-run-world-units-per-em)
   (vertex-data :initarg :vertex-data :reader world-text-run-vertex-data)
   (vertex-buffer :initarg :vertex-buffer
                  :reader world-text-run-vertex-buffer)
   (instance-data :initarg :instance-data :reader world-text-run-instance-data)
   (instance-buffer :initarg :instance-buffer
                    :reader world-text-run-instance-buffer)
   (layout :initarg :layout :reader world-text-run-layout)
   (pipeline :initarg :pipeline :reader world-text-run-pipeline)
   (resources :initarg :resources :reader world-text-run-resources)))

(defun make-world-text-glyph-cache (device)
  (make-instance 'world-text-glyph-cache :device device))

(defun world-text-font-key (font-pathname)
  (namestring (truename font-pathname)))

(defun world-text-shaped-text-for (cache font-pathname string)
  "Return one durable HarfBuzz result for a canonical font and exact string."
  (let* ((key (list (world-text-font-key font-pathname) string))
         (shaped-texts (world-text-glyph-cache-shaped-texts cache)))
    (or (gethash key shaped-texts)
        (setf (gethash key shaped-texts)
              (luv.slug:shape-slug-text string font-pathname)))))

(defun create-world-text-glyph-resource
    (cache key glyph-id font-loader)
  (declare (ignore cache))
  (let* ((glyph (luv.slug:load-slug-glyph-index glyph-id font-loader))
         (outline (luv.slug:normalize-slug-glyph-outline glyph)))
    (when (luv.slug:slug-outline-contours outline)
      (make-world-text-glyph-resource
       :key key :glyph-id glyph-id
       :serialized
       (luv.slug:serialize-slug-outline outline)))))

(defun world-text-glyph-resource-for
    (cache font-pathname glyph-id font-loader)
  "Return one device resource for a canonical font and HarfBuzz glyph ID."
  (let* ((key (list (world-text-font-key font-pathname) glyph-id))
         (entries (world-text-glyph-cache-entries cache)))
    (multiple-value-bind (value present-p) (gethash key entries)
      (if present-p
          (unless (eq value :empty) value)
          (let ((resource
                  (create-world-text-glyph-resource
                   cache key glyph-id font-loader)))
            (setf (gethash key entries) (or resource :empty))
            resource)))))

(defun world-text-glyph-cache-resource-count (cache)
  (loop for resource being the hash-values of
          (world-text-glyph-cache-entries cache)
        count (not (eq resource :empty))))

(defun world-text-atlas-upload-data (resources count-reader data-reader
                                     element-type)
  (let* ((width 4096)
         (texel-count (loop for resource in resources
                            sum (funcall count-reader
                                         (world-text-glyph-resource-serialized
                                          resource))))
         (height (max 1 (ceiling texel-count width)))
         (data (make-array (list height width)
                           :element-type element-type :initial-element 0))
         (offset 0))
    (dolist (resource resources)
      (let* ((serialized (world-text-glyph-resource-serialized resource))
             (count (funcall count-reader serialized))
             (source (funcall data-reader serialized)))
        (dotimes (index count)
          (setf (row-major-aref data (+ offset index))
                (row-major-aref source index)))
        (incf offset count)))
    (values data texel-count)))

(defun create-world-text-glyph-atlas (cache key resources)
  "Pack RESOURCES tightly into one RG16U band and one RGBA16F curve texture."
  (let ((locations (make-hash-table :test #'eq))
        (band-offset 0)
        (curve-offset 0))
    (dolist (resource resources)
      (setf (gethash resource locations) (list band-offset curve-offset))
      (incf band-offset
            (luv.slug:slug-serialized-outline-band-texel-count
             (world-text-glyph-resource-serialized resource)))
      (incf curve-offset
            (luv.slug:slug-serialized-outline-curve-texel-count
             (world-text-glyph-resource-serialized resource))))
    (multiple-value-bind (band-data band-count)
        (world-text-atlas-upload-data
         resources #'luv.slug:slug-serialized-outline-band-texel-count
         #'luv.slug:slug-serialized-outline-band-upload-data
         '(unsigned-byte 32))
      (multiple-value-bind (curve-data curve-count)
          (world-text-atlas-upload-data
           resources #'luv.slug:slug-serialized-outline-curve-texel-count
           #'luv.slug:slug-serialized-outline-curve-upload-data
           '(unsigned-byte 64))
        (let* ((device (world-text-glyph-cache-device cache))
               (band-size (list 4096 (array-dimension band-data 0)))
               (curve-size (list 4096 (array-dimension curve-data 0)))
               (band-texture nil) (curve-texture nil)
               (band-view nil) (curve-view nil) (completed-p nil))
          (unwind-protect
               (progn
                 (setf band-texture
                       (create
                        device
                        (make-texture-descriptor
                         :label "world glyph RG16U atlas" :size band-size
                         :dimensions :2d :format :rg16-uint
                         :usage '(:texture-binding :copy-dst)))
                       curve-texture
                       (create
                        device
                        (make-texture-descriptor
                         :label "world glyph RGBA16F atlas" :size curve-size
                         :dimensions :2d :format :rgba16-float
                         :usage '(:texture-binding :copy-dst)))
                       band-view
                       (create device (make-texture-view-descriptor
                                       :texture band-texture))
                       curve-view
                       (create device (make-texture-view-descriptor
                                       :texture curve-texture)))
                 (write-texture
                  (device-queue device) (make-texture-copy :texture band-texture)
                  band-data
                  (make-texture-data-layout :bytes-per-row (* 4 4096)
                                            :rows-per-image (second band-size))
                  band-size)
                 (write-texture
                  (device-queue device)
                  (make-texture-copy :texture curve-texture) curve-data
                  (make-texture-data-layout :bytes-per-row (* 8 4096)
                                            :rows-per-image (second curve-size))
                  curve-size)
                 (let ((atlas
                         (make-world-text-glyph-atlas
                          :key key :locations locations
                          :band-texel-count band-count
                          :curve-texel-count curve-count
                          :band-texture band-texture :band-view band-view
                          :curve-texture curve-texture :curve-view curve-view)))
                   (setf completed-p t)
                   atlas))
            (unless completed-p
              (dolist (resource
                        (remove nil (list curve-view band-view
                                          curve-texture band-texture)))
                (destroy resource)))))))))

(defun world-text-glyph-atlas-for (cache glyphs)
  (let* ((resources
           (remove-duplicates
            (mapcar #'world-text-glyph-resource glyphs) :test #'eq))
         (key (mapcar #'world-text-glyph-resource-key resources))
         (atlases (world-text-glyph-cache-atlases cache)))
    (or (gethash key atlases)
        (setf (gethash key atlases)
              (create-world-text-glyph-atlas cache key resources)))))

(defun release-world-text-glyph-atlas (atlas)
  (destroy (world-text-glyph-atlas-curve-view atlas))
  (destroy (world-text-glyph-atlas-band-view atlas))
  (destroy (world-text-glyph-atlas-curve-texture atlas))
  (destroy (world-text-glyph-atlas-band-texture atlas)))

(defun release-world-text-glyph-cache (cache)
  (maphash (lambda (key atlas)
             (declare (ignore key))
             (release-world-text-glyph-atlas atlas))
           (world-text-glyph-cache-atlases cache))
  (clrhash (world-text-glyph-cache-atlases cache))
  (clrhash (world-text-glyph-cache-entries cache))
  (clrhash (world-text-glyph-cache-shaped-texts cache))
  (values))

(defun make-world-text-glyphs (shaped font-loader cache font-pathname)
  "Join HarfBuzz placements to normalized Slug outlines by selected glyph ID."
  (let* ((unit (/ 1 (luv.slug:slug-shaped-text-units-per-em shaped)))
         (pen-x 0)
         (pen-y 0)
         glyphs)
    (loop for placement across (luv.slug:slug-shaped-text-glyphs shaped)
          for glyph-id = (luv.slug:slug-shaped-glyph-glyph-id placement)
          for resource = (world-text-glyph-resource-for
                          cache font-pathname glyph-id font-loader)
          do (when resource
               (let* ((serialized
                        (world-text-glyph-resource-serialized resource))
                      (packed
                        (luv.slug:slug-serialized-outline-packed-outline
                         serialized)))
                 (push
                  (make-world-text-glyph
                   :glyph-id glyph-id :resource resource
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
                  glyphs)))
             (incf pen-x (luv.slug:slug-shaped-glyph-x-advance placement))
             (incf pen-y (luv.slug:slug-shaped-glyph-y-advance placement)))
    (nreverse glyphs)))

(defun world-text-extents (glyphs shaped font-loader)
  (let* ((unit (/ 1 (luv.slug:slug-shaped-text-units-per-em shaped)))
         (advance-x (* unit (luv.slug:slug-shaped-text-x-advance shaped)))
         (advance-y (* unit (luv.slug:slug-shaped-text-y-advance shaped))))
    (values
     (min 0.0
          (loop for glyph in glyphs
                minimize (+ (world-text-glyph-origin-x glyph)
                            (world-text-glyph-outline-min-x glyph))))
     (min 0.0 (* unit (zpb-ttf:descender font-loader))
          (loop for glyph in glyphs
                minimize (+ (world-text-glyph-origin-y glyph)
                            (world-text-glyph-outline-min-y glyph))))
     (max advance-x
          (loop for glyph in glyphs
                maximize (+ (world-text-glyph-origin-x glyph)
                            (world-text-glyph-outline-max-x glyph))))
     (max advance-y (* unit (zpb-ttf:ascender font-loader))
          (loop for glyph in glyphs
                maximize (+ (world-text-glyph-origin-y glyph)
                            (world-text-glyph-outline-max-y glyph)))))))

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
    (glyphs atlas center right up scale min-x min-y max-x max-y)
  "Build one dense model-and-atlas record per drawable glyph occurrence."
  (let* ((padding 0.035)
         (middle-x (/ (+ min-x max-x) 2))
         (middle-y (/ (+ min-y max-y) 2))
         (data (make-array (* 18 (length glyphs))
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
            for base from 0 by 18
            for outline-left = (- (world-text-glyph-outline-min-x glyph)
                                  padding)
            for outline-bottom = (- (world-text-glyph-outline-min-y glyph)
                                    padding)
            for outline-right = (+ (world-text-glyph-outline-max-x glyph)
                                   padding)
            for outline-top = (+ (world-text-glyph-outline-max-y glyph)
                                 padding)
            for layout-left = (+ (world-text-glyph-origin-x glyph)
                                 outline-left)
            for layout-bottom = (+ (world-text-glyph-origin-y glyph)
                                   outline-bottom)
            for layout-right = (+ (world-text-glyph-origin-x glyph)
                                  outline-right)
            for layout-top = (+ (world-text-glyph-origin-y glyph) outline-top)
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
              (gethash (world-text-glyph-resource glyph)
                       (world-text-glyph-atlas-locations atlas))
            for serialized =
              (world-text-glyph-resource-serialized
               (world-text-glyph-resource glyph))
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
                      (first atlas-location) (second atlas-location) 0.0)))
      data)))

(defun world-text-center-before-camera (camera distance lift)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (declare (ignore right))
    (world-text-point (camera-position camera) forward up distance lift 1.0)))

(defun make-world-text-run
    (device glyph-cache camera target-format string font-pathname
     &key (distance 8.0) (lift 3.0) (world-units-per-em 0.55))
  "Shape STRING once and create a depth-tested world text run on DEVICE.

The run owns dense placement/model data and its live pipeline; GLYPH-CACHE owns
font-and-glyph device resources reusable across runs.  See #QW7P96."
  (let* ((shaped (world-text-shaped-text-for
                  glyph-cache font-pathname string))
         (center (world-text-center-before-camera camera distance lift))
         (resources nil)
         (pipeline nil)
         (completed-p nil))
    (zpb-ttf:with-font-loader (font-loader font-pathname)
      (let ((glyphs
              (make-world-text-glyphs
               shaped font-loader glyph-cache font-pathname)))
        (unless glyphs
          (error 'luv.slug:slug-shaping-error
                 :reason :no-drawable-glyphs :details string))
        (multiple-value-bind (min-x min-y max-x max-y)
            (world-text-extents glyphs shaped font-loader)
          (multiple-value-bind (right up forward) (camera-basis camera)
            (declare (ignore forward))
            (let* ((atlas (world-text-glyph-atlas-for glyph-cache glyphs))
                   (vertex-data (make-world-text-quad-vertices))
                   (instance-data
                     (make-world-text-instances
                      glyphs atlas center right up world-units-per-em
                      min-x min-y max-x max-y))
                   (layout nil)
                   (vertex-buffer nil)
                   (instance-buffer nil))
              (flet ((keep (resource) (push resource resources) resource))
                (unwind-protect
                     (progn
                       (setf layout
                             (keep
                              (create
                               device
                               (make-bind-group-layout-descriptor
                                :label "world Slug text layout"
                                :entries '((:binding 0 :type :texture)
                                           (:binding 1 :type :texture)
                                           (:binding 2
                                            :type :uniform-buffer)))))
                             vertex-buffer
                             (keep
                              (create
                               device
                               (make-buffer-descriptor
                                :label "world Slug glyph quads"
                                :size (* 4 (length vertex-data))
                                :usage '(:vertex :copy-dst))))
                             instance-buffer
                             (keep
                              (create
                               device
                               (make-buffer-descriptor
                                :label "world Slug glyph instances"
                                :size (* 4 (length instance-data))
                                :usage '(:vertex :copy-dst))))
                             pipeline
                             (make-live-shader-pipeline
                              :role :slug-world-text
                              :vertex-role :slug-world-text
                              :label "world HarfBuzz Slug text"
                              :device device :layout layout
                              :vertex-buffers
                              '((:array-stride 12
                                 :attributes
                                 ((:shader-location 0 :offset 0
                                   :format :float32x3)))
                                (:array-stride 72 :step-mode :instance
                                 :attributes
                                 ((:shader-location 1 :offset 0
                                   :format :float32x3)
                                  (:shader-location 2 :offset 12
                                   :format :float32x3)
                                  (:shader-location 3 :offset 24
                                   :format :float32x3)
                                  (:shader-location 4 :offset 36
                                   :format :float32x3)
                                  (:shader-location 5 :offset 48
                                   :format :float32x3)
                                  (:shader-location 6 :offset 60
                                   :format :float32x3))))
                              :target-format target-format
                              :target-blend :premultiplied-alpha
                              :primitive '(:topology :triangle-list)
                              :depth-stencil
                              '(:format :depth32-float
                                :depth-write-enabled nil
                                :depth-compare :less)))
                       (write-buffer vertex-buffer vertex-data)
                       (write-buffer instance-buffer instance-data)
                       (let ((run
                               (make-instance
                                'world-text-run
                                :string string :font-pathname font-pathname
                                :shaped-text shaped :glyphs glyphs :atlas atlas
                                :center center
                                :world-units-per-em world-units-per-em
                                :vertex-data vertex-data
                                :vertex-buffer vertex-buffer
                                :instance-data instance-data
                                :instance-buffer instance-buffer :layout layout
                                :pipeline pipeline :resources resources)))
                         (setf completed-p t)
                         run))
                  (unless completed-p
                    (when pipeline
                      (release-live-shader-pipeline pipeline))
                    (dolist (resource resources)
                      (ignore-errors (destroy resource)))))))))))))

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
                      :resource ,(world-text-glyph-atlas-band-view atlas))
                     (:binding 1
                      :resource ,(world-text-glyph-atlas-curve-view atlas))
                     (:binding 2 :resource ,uniform-buffer)))))
           (setf completed-p t)
           groups)
      (unless completed-p
        (when (aref groups 0) (destroy (aref groups 0)))))))

(defun world-text-run-native-pipeline (run)
  (live-shader-pipeline-native-pipeline (world-text-run-pipeline run)))

(defun release-world-text-run (run)
  (release-live-shader-pipeline (world-text-run-pipeline run))
  (dolist (resource (world-text-run-resources run))
    (destroy resource))
  (values))
