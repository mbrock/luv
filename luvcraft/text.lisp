;;; HarfBuzz-shaped Slug text as world-owned render geometry.
;;;
;;; A WORLD-TEXT-RUN is the semantic placement boundary.  A per-device cache
;;; retains HarfBuzz results and one uploaded outline per font/glyph pair until
;;; the shared-atlas iteration packs those resources more tightly.

(in-package #:luvcraft)

(defstruct world-text-glyph
  glyph-id resource origin-x origin-y
  outline-min-x outline-min-y outline-max-x outline-max-y)

(defstruct world-text-glyph-resource
  key glyph-id serialized band-texture band-view curve-texture curve-view)

(defclass world-text-glyph-cache ()
  ((device :initarg :device :reader world-text-glyph-cache-device)
   (entries :initform (make-hash-table :test #'equal)
            :reader world-text-glyph-cache-entries)
   (shaped-texts :initform (make-hash-table :test #'equal)
                 :reader world-text-glyph-cache-shaped-texts)))

(defclass world-text-run ()
  ((string :initarg :string :reader world-text-run-string)
   (font-pathname :initarg :font-pathname
                  :reader world-text-run-font-pathname)
   (shaped-text :initarg :shaped-text :reader world-text-run-shaped-text)
   (glyphs :initarg :glyphs :reader world-text-run-glyphs)
   (center :initarg :center :reader world-text-run-center)
   (world-units-per-em :initarg :world-units-per-em
                       :reader world-text-run-world-units-per-em)
   (vertex-data :initarg :vertex-data :reader world-text-run-vertex-data)
   (vertex-buffer :initarg :vertex-buffer
                  :reader world-text-run-vertex-buffer)
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
  (let* ((device (world-text-glyph-cache-device cache))
         (glyph (luv.slug:load-slug-glyph-index glyph-id font-loader))
         (outline (luv.slug:normalize-slug-glyph-outline glyph)))
    (when (luv.slug:slug-outline-contours outline)
      (let* ((serialized
               (luv.slug:serialize-slug-outline
                outline :horizontal-band-count 1 :vertical-band-count 1))
             (band-texture nil)
             (curve-texture nil)
             (band-view nil)
             (curve-view nil)
             (completed-p nil))
        (unwind-protect
             (progn
               (setf band-texture
                     (create
                      device
                      (make-texture-descriptor
                       :label "cached world glyph RG16U bands"
                       :size
                       (luv.slug:slug-serialized-outline-band-texture-size
                        serialized)
                       :dimensions :2d :format :rg16-uint
                       :usage '(:texture-binding :copy-dst)))
                     curve-texture
                     (create
                      device
                      (make-texture-descriptor
                       :label "cached world glyph RGBA16F curves"
                       :size
                       (luv.slug:slug-serialized-outline-curve-texture-size
                        serialized)
                       :dimensions :2d :format :rgba16-float
                       :usage '(:texture-binding :copy-dst)))
                     band-view
                     (create
                      device
                      (make-texture-view-descriptor :texture band-texture))
                     curve-view
                     (create
                      device
                      (make-texture-view-descriptor :texture curve-texture)))
               (write-texture
                (device-queue device)
                (make-texture-copy :texture band-texture)
                (luv.slug:slug-serialized-outline-band-upload-data serialized)
                (make-texture-data-layout
                 :bytes-per-row
                 (* 4
                    (luv.slug:slug-serialized-outline-band-width serialized))
                 :rows-per-image
                 (second
                  (luv.slug:slug-serialized-outline-band-texture-size
                   serialized)))
                (luv.slug:slug-serialized-outline-band-texture-size serialized))
               (write-texture
                (device-queue device)
                (make-texture-copy :texture curve-texture)
                (luv.slug:slug-serialized-outline-curve-upload-data serialized)
                (make-texture-data-layout
                 :bytes-per-row
                 (* 8
                    (luv.slug:slug-serialized-outline-curve-width serialized))
                 :rows-per-image
                 (second
                  (luv.slug:slug-serialized-outline-curve-texture-size
                   serialized)))
                (luv.slug:slug-serialized-outline-curve-texture-size
                 serialized))
               (let ((resource
                       (make-world-text-glyph-resource
                        :key key :glyph-id glyph-id :serialized serialized
                        :band-texture band-texture :band-view band-view
                        :curve-texture curve-texture :curve-view curve-view)))
                 (setf completed-p t)
                 resource))
          (unless completed-p
            (dolist (resource
                      (remove nil
                              (list curve-view band-view
                                    curve-texture band-texture)))
              (destroy resource))))))))

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

(defun release-world-text-glyph-cache (cache)
  (maphash
   (lambda (key resource)
     (declare (ignore key))
     (unless (eq resource :empty)
       (destroy (world-text-glyph-resource-curve-view resource))
       (destroy (world-text-glyph-resource-band-view resource))
       (destroy (world-text-glyph-resource-curve-texture resource))
       (destroy (world-text-glyph-resource-band-texture resource))))
   (world-text-glyph-cache-entries cache))
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

(defun make-world-text-vertices
    (glyphs center right up scale min-x min-y max-x max-y)
  "Apply one text-run model transform and retain em coordinates per vertex."
  (let* ((padding 0.035)
         (middle-x (/ (+ min-x max-x) 2))
         (middle-y (/ (+ min-y max-y) 2))
         (data (make-array (* 54 (length glyphs))
                           :element-type 'single-float)))
    (labels ((write-vertex (offset layout-x layout-y outline-x outline-y)
               (let ((world
                       (world-text-point
                        center right up (- layout-x middle-x)
                        (- layout-y middle-y) scale)))
                 (loop for value in (list (vec3-x world) (vec3-y world)
                                          (vec3-z world)
                                          outline-x outline-y 0.0
                                          1.0 1.0 0.0)
                       for index from offset
                       do (setf (aref data index)
                                (coerce value 'single-float))))))
      (loop for glyph in glyphs
            for glyph-index from 0
            for base = (* glyph-index 54)
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
            do (write-vertex base layout-left layout-bottom
                             outline-left outline-bottom)
               (write-vertex (+ base 9) layout-right layout-bottom
                             outline-right outline-bottom)
               (write-vertex (+ base 18) layout-right layout-top
                             outline-right outline-top)
               (write-vertex (+ base 27) layout-left layout-bottom
                             outline-left outline-bottom)
               (write-vertex (+ base 36) layout-right layout-top
                             outline-right outline-top)
               (write-vertex (+ base 45) layout-left layout-top
                             outline-left outline-top))
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
            (let* ((vertex-data
                     (make-world-text-vertices
                      glyphs center right up world-units-per-em
                      min-x min-y max-x max-y))
                   (layout nil)
                   (vertex-buffer nil))
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
                             pipeline
                             (make-live-shader-pipeline
                              :role :slug-world-text
                              :vertex-role :slug-world-text
                              :label "world HarfBuzz Slug text"
                              :device device :layout layout
                              :vertex-buffers
                              '((:array-stride 36
                                 :attributes
                                 ((:shader-location 0 :offset 0
                                   :format :float32x3)
                                  (:shader-location 1 :offset 12
                                   :format :float32x3)
                                  (:shader-location 2 :offset 24
                                   :format :float32x3))))
                              :target-format target-format
                              :target-blend :premultiplied-alpha
                              :primitive '(:topology :triangle-list)
                              :depth-stencil
                              '(:format :depth32-float
                                :depth-write-enabled nil
                                :depth-compare :less)))
                       (write-buffer vertex-buffer vertex-data)
                       (let ((run
                               (make-instance
                                'world-text-run
                                :string string :font-pathname font-pathname
                                :shaped-text shaped :glyphs glyphs
                                :center center
                                :world-units-per-em world-units-per-em
                                :vertex-data vertex-data
                                :vertex-buffer vertex-buffer :layout layout
                                :pipeline pipeline :resources resources)))
                         (setf completed-p t)
                         run))
                  (unless completed-p
                    (when pipeline
                      (release-live-shader-pipeline pipeline))
                    (dolist (resource resources)
                      (ignore-errors (destroy resource)))))))))))))

(defun make-world-text-frame-bind-groups (run device uniform-buffer)
  "Bind each temporary glyph texture pair to one drawable-frame uniform."
  (let* ((glyphs (world-text-run-glyphs run))
         (groups (make-array (length glyphs) :initial-element nil))
         (groups-by-resource (make-hash-table :test #'eq))
         (completed-p nil))
    (unwind-protect
         (progn
           (loop for glyph in glyphs
                 for index from 0
                 for resource = (world-text-glyph-resource glyph)
                 do (setf (aref groups index)
                          (or (gethash resource groups-by-resource)
                              (setf
                               (gethash resource groups-by-resource)
                               (create
                                device
                                (make-bind-group-descriptor
                                 :label "cached world glyph frame bindings"
                                 :layout (world-text-run-layout run)
                                 :entries
                                 `((:binding 0
                                    :resource ,(world-text-glyph-resource-band-view
                                                resource))
                                   (:binding 1
                                    :resource ,(world-text-glyph-resource-curve-view
                                                resource))
                                   (:binding 2
                                    :resource ,uniform-buffer))))))))
           (setf completed-p t)
           groups)
      (unless completed-p
        (maphash (lambda (resource group)
                   (declare (ignore resource))
                   (destroy group))
                 groups-by-resource)))))

(defun world-text-projected-pixels-per-em (run camera viewport-height)
  "Approximate projected em scale at RUN's center for the current camera."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (declare (ignore right up))
    (let* ((center (world-text-run-center run))
           (camera-position (camera-position camera))
           (relative
             (make-vec3 (- (vec3-x center) (vec3-x camera-position))
                        (- (vec3-y center) (vec3-y camera-position))
                        (- (vec3-z center) (vec3-z camera-position))))
           (view-distance (max 0.1 (vec3-dot relative forward)))
           (focal
             (/ (tan (/ +luvcraft-camera-vertical-field-of-view+ 2.0)))))
      (coerce
       (/ (* (world-text-run-world-units-per-em run)
             focal viewport-height 0.5)
          view-distance)
       'single-float))))

(defun update-world-text-projected-scale (run camera viewport-height)
  "Update the temporary per-vertex scale until fragment derivatives own it."
  (let ((pixels-per-em
          (world-text-projected-pixels-per-em run camera viewport-height))
        (data (world-text-run-vertex-data run)))
    (loop for vertex below (* 6 (length (world-text-run-glyphs run)))
          for base = (* vertex 9)
          do (setf (aref data (+ base 6)) pixels-per-em
                   (aref data (+ base 7)) pixels-per-em))
    (write-buffer (world-text-run-vertex-buffer run) data)
    pixels-per-em))

(defun world-text-run-native-pipeline (run)
  (live-shader-pipeline-native-pipeline (world-text-run-pipeline run)))

(defun release-world-text-run (run)
  (release-live-shader-pipeline (world-text-run-pipeline run))
  (dolist (resource (world-text-run-resources run))
    (destroy resource))
  (values))
