;;; Direct GPU McCLIM output.
;;;
;;; The medium records painter-ordered triangles.  It never allocates a pixel
;;; image and the mirror presents those vertices directly into its luv canvas.

(in-package #:mcluv)

(define-condition gpu-medium-unsupported-design (error)
  ((design :initarg :design :reader unsupported-gpu-design))
  (:report (lambda (condition stream)
             (format stream "The direct GPU medium does not support ink ~S."
                     (unsupported-gpu-design condition)))))

(defstruct gpu-solid-command first-vertex vertex-count clip)

(defstruct gpu-analytic-command first-vertex vertex-count clip)

(defstruct gpu-relief-analytic-command first-vertex vertex-count clip)

(defstruct gpu-gradient-analytic-command first-vertex vertex-count clip)

(defstruct gpu-image-command design first-vertex vertex-count clip)

(defstruct gpu-prepared-image-command paint first-vertex vertex-count clip)

(defstruct gpu-text-command
  string x y font-pathname size color align-x align-y clip)

(defstruct gpu-prepared-text-command
  atlas first-vertex vertex-count clip)

(defgeneric gpu-command-rasterized-p (compositor command)
  (:documentation
   "Whether COMPOSITOR wants prepared COMMAND in the mirror texture.

A compositor which replays a command directly into its final render pass can
return false, avoiding a lower-resolution copy underneath the direct draw."))

(defmethod gpu-command-rasterized-p (compositor command)
  (declare (ignore compositor command))
  t)

(defstruct surface-relief x1 y1 x2 y2 radius height)

(defvar *gpu-medium-fallback-source* nil
  "The semantic primitive currently being decomposed by BASIC-MEDIUM.")

(defvar *suppress-luv-mirror-visibility* nil
  "When true, frame realization and repaint remain drawable-only and hidden.")

(defun note-gpu-medium-fallback (medium primitive field &optional (amount 1))
  (let* ((statistics (gpu-medium-fallback-statistics medium))
         (entry (copy-list (gethash primitive statistics))))
    (incf (getf entry field 0) amount)
    (setf (gethash primitive statistics) entry)))

(defun clear-gpu-medium-fallback-statistics (medium)
  "Forget McCLIM fallback activity previously observed by MEDIUM."
  (check-type medium luv-gpu-medium)
  (clrhash (gpu-medium-fallback-statistics medium))
  medium)

(defun gpu-medium-fallback-report (medium)
  "Describe which BASIC-MEDIUM fallbacks fed MEDIUM's GPU polygon leaf.

Each entry reports semantic calls, polygon points produced by McCLIM, and GPU
triangles emitted by luv. Direct polygon calls are named :DIRECT-POLYGON."
  (check-type medium luv-gpu-medium)
  (let (entries)
    (maphash (lambda (primitive statistics)
               (push (list* :primitive primitive statistics) entries))
             (gpu-medium-fallback-statistics medium))
    (sort entries #'string< :key (lambda (entry)
                                  (symbol-name (getf entry :primitive))))))

(defmacro with-gpu-medium-fallback ((medium primitive) &body body)
  `(progn
     (note-gpu-medium-fallback ,medium ,primitive :calls)
     (let ((*gpu-medium-fallback-source*
             (or *gpu-medium-fallback-source* ,primitive)))
       ,@body)))

(defclass gpu-mirror-frame-state ()
  ((view :initarg :view :initform nil :accessor gpu-frame-state-view)
   (vertex-buffer :initarg :vertex-buffer :initform nil
                  :accessor gpu-frame-state-vertex-buffer)
   (vertex-capacity :initarg :vertex-capacity :initform 0
                    :accessor gpu-frame-state-vertex-capacity)
   (analytic-buffer :initarg :analytic-buffer :initform nil
                    :accessor gpu-frame-state-analytic-buffer)
   (analytic-capacity :initarg :analytic-capacity :initform 0
                      :accessor gpu-frame-state-analytic-capacity)
   (relief-buffer :initarg :relief-buffer :initform nil
                  :accessor gpu-frame-state-relief-buffer)
   (relief-capacity :initarg :relief-capacity :initform 0
                    :accessor gpu-frame-state-relief-capacity)
   (gradient-buffer :initarg :gradient-buffer :initform nil
                    :accessor gpu-frame-state-gradient-buffer)
   (gradient-capacity :initarg :gradient-capacity :initform 0
                      :accessor gpu-frame-state-gradient-capacity)
   (image-buffer :initarg :image-buffer :initform nil
                 :accessor gpu-frame-state-image-buffer)
   (image-capacity :initarg :image-capacity :initform 0
                   :accessor gpu-frame-state-image-capacity)
   (text-buffer :initarg :text-buffer :initform nil
                :accessor gpu-frame-state-text-buffer)
   (text-capacity :initarg :text-capacity :initform 0
                  :accessor gpu-frame-state-text-capacity)))

(defclass gpu-cached-image-paint ()
  ((texture :initarg :texture :reader gpu-image-paint-texture)
   (view :initarg :view :reader gpu-image-paint-view)
   (bind-group :initarg :bind-group :initform nil
               :reader gpu-image-paint-bind-group)
   (width :initarg :width :reader gpu-image-paint-width)
   (height :initarg :height :reader gpu-image-paint-height)))

(spv:define-shader-method spv:shader-specification-for
    mcluv-solid-vertex-specification
    ((role (eql :mcluv-solid)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((position-opacity :vec3 :location 0)
              (color-input :vec3 :location 1))
     :outputs ((clip-position :vec4 :built-in :position)
               (color-output :vec4 :location 0)))
  (let* ((clip (spv:vec4 (spv:swizzle position-opacity :xy) 0.0 1.0))
         (color (spv:vec4 color-input
                          (spv:swizzle position-opacity :z))))
    (spv:set-output clip-position clip)
    (spv:set-output color-output color)))

(spv:define-shader-method spv:shader-specification-for
    mcluv-solid-fragment-specification
    ((role (eql :mcluv-solid)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((color-input :vec4 :location 0))
     :outputs ((color-output :vec4 :location 0)))
  (spv:set-output color-output color-input))

(spv:define-shader-method spv:shader-specification-for
    mcluv-slug-vertex-specification
    ((role (eql :mcluv-slug)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((position-alpha :vec3 :location 0)
              (outline-horizontal :vec3 :location 1)
              (atlas-vertical :vec3 :location 2)
              (band-low :vec3 :location 3)
              (band-high :vec3 :location 4)
              (color-input :vec3 :location 5))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-atlas-base :vec2 :location 1)
               (render-band-bounds :vec4 :location 2)
               (render-band-counts :vec2 :location 3)
               (render-color :vec4 :location 4)))
  (let* ()
    (spv:set-output
     clip-position
     (spv:vec4 (spv:swizzle position-alpha :xy) 0.0 1.0))
    (spv:set-output render-coordinate
                    (spv:swizzle outline-horizontal :xy))
    (spv:set-output render-atlas-base (spv:swizzle atlas-vertical :xy))
    (spv:set-output
     render-band-bounds
     (spv:vec4 (spv:swizzle band-low :xy)
               (spv:swizzle band-high :xy)))
    (spv:set-output
     render-band-counts
     (spv:vec2 (spv:swizzle outline-horizontal :z)
               (spv:swizzle atlas-vertical :z)))
    (spv:set-output
     render-color
     (spv:vec4 color-input (spv:swizzle position-alpha :z)))))

(defmethod spv:shader-specification-for
    ((role (eql :mcluv-slug)) (stage (eql :fragment)))
  (declare (ignore role stage))
  (luv.slug:slug-atlas-fragment-specification))

(defun gpu-medium-color (medium)
  (handler-case
      (multiple-value-list (color-rgba (medium-ink medium)))
    (error ()
      (error 'gpu-medium-unsupported-design :design (medium-ink medium)))))

(defgeneric gpu-medium-append-analytic-design
    (design medium center x-axis y-axis half-width half-height radius)
  (:documentation
   "Append one analytical shape painted by DESIGN, or return NIL."))

(defun gpu-medium-size (medium)
  (let ((mirror (medium-drawable medium)))
    (if mirror
        (with-bounding-rectangle* (:width width :height height)
            (mirror-sheet mirror)
          ;; Window events synchronize this region before repaint. Drawing must
          ;; not call back into the native canvas thread while McCLIM holds its
          ;; mirror lock.
          (values (max 1 width) (max 1 height)))
        (values 1 1))))

(defun gpu-medium-clip-rectangle (medium)
  "Return MEDIUM's rectangular clip in logical device coordinates, or NIL."
  (let ((region (medium-clipping-region medium)))
    (unless (region-equal region +everywhere+)
      (let ((transformation
              (if (climi::medium-sheet medium)
                  (compose-transformations
                   (medium-device-transformation medium)
                   (medium-transformation medium))
                  (medium-device-transformation medium))))
        (with-bounding-rectangle* (left top right bottom)
            (transform-region transformation region)
          (multiple-value-bind (width height) (gpu-medium-size medium)
            (let ((left (max 0.0 (min width left)))
                  (top (max 0.0 (min height top)))
                  (right (max 0.0 (min width right)))
                  (bottom (max 0.0 (min height bottom))))
              (list left top (max left right) (max top bottom)))))))))

(defun gpu-medium-push-vertex (medium x y color)
  (multiple-value-bind (device-x device-y)
      (transform-position (medium-device-transformation medium) x y)
    (multiple-value-bind (width height) (gpu-medium-size medium)
      (let ((vertices (gpu-medium-vertices medium)))
        (flet ((push-value (value)
                 (vector-push-extend (coerce value 'single-float) vertices)))
          ;; The mathematical shader vocabulary uses Vulkan's downward viewport.
          ;; Metal lowering flips clip Y at the target boundary.
          (push-value (- (* 2 (/ device-x width)) 1))
          (push-value (- (* 2 (/ device-y height)) 1))
          ;; Both native backends currently expose float32x3 vertex attributes.
          ;; Carry opacity in position.z and store premultiplied RGB separately.
          (let ((alpha (fourth color)))
            (push-value alpha)
            (dolist (component (subseq color 0 3))
              (push-value (* component alpha)))))))))

(defun gpu-medium-push-triangle (medium a b c color)
  (dolist (point (list a b c))
    (gpu-medium-push-vertex medium (first point) (second point) color)))

(defun gpu-medium-analytic-padding (medium x-axis y-axis)
  "Return enough local expansion for two device pixels around an affine quad."
  (let ((transformation (medium-device-transformation medium)))
    (multiple-value-bind (axx axy)
        (transform-distance transformation (first x-axis) (second x-axis))
      (multiple-value-bind (ayx ayy)
          (transform-distance transformation (first y-axis) (second y-axis))
        (let ((determinant (- (* axx ayy) (* ayx axy))))
          (if (< (abs determinant) 1.0e-8)
              nil
              (* 2.0
                 (max (/ (sqrt (+ (* ayy ayy) (* ayx ayx)))
                         (abs determinant))
                      (/ (sqrt (+ (* axy axy) (* axx axx)))
                         (abs determinant))))))))))

(defun gpu-medium-push-analytic-vertex
    (medium center x-axis y-axis local-x local-y
     half-width half-height radius color)
  (let ((x (+ (first center)
              (* local-x (first x-axis))
              (* local-y (first y-axis))))
        (y (+ (second center)
              (* local-x (second x-axis))
              (* local-y (second y-axis)))))
    (multiple-value-bind (device-x device-y)
        (transform-position (medium-device-transformation medium) x y)
      (multiple-value-bind (width height) (gpu-medium-size medium)
        (let ((vertices (gpu-medium-analytic-vertices medium)))
          (flet ((push-value (value)
                   (vector-push-extend (coerce value 'single-float) vertices)))
            (push-value (- (* 2 (/ device-x width)) 1))
            (push-value (- (* 2 (/ device-y height)) 1))
            (push-value (fourth color))
            (push-value local-x)
            (push-value local-y)
            (push-value 0.0)
            (push-value half-width)
            (push-value half-height)
            (push-value radius)
            (dolist (component (subseq color 0 3))
              (push-value component))))))))

(defun gpu-medium-append-analytic-quad
    (medium center x-axis y-axis half-width half-height radius color)
  (alexandria:when-let
      ((padding (gpu-medium-analytic-padding medium x-axis y-axis)))
    (let* ((vertices (gpu-medium-analytic-vertices medium))
           (first-vertex (/ (length vertices) 12))
           (left (- (+ half-width padding)))
           (right (+ half-width padding))
           (top (- (+ half-height padding)))
           (bottom (+ half-height padding)))
      (flet ((vertex (x y)
               (gpu-medium-push-analytic-vertex
                medium center x-axis y-axis x y
                half-width half-height radius color)))
        (vertex left bottom)
        (vertex right bottom)
        (vertex right top)
        (vertex left bottom)
        (vertex right top)
        (vertex left top))
      (vector-push-extend
       (make-gpu-analytic-command
        :first-vertex first-vertex :vertex-count 6
        :clip (gpu-medium-clip-rectangle medium))
       (gpu-medium-commands medium))
      t)))

(defun gpu-medium-push-relief-analytic-vertex
    (medium center x-axis y-axis local-x local-y
     half-width half-height radius color height)
  (let ((x (+ (first center)
              (* local-x (first x-axis))
              (* local-y (first y-axis))))
        (y (+ (second center)
              (* local-x (second x-axis))
              (* local-y (second y-axis)))))
    (multiple-value-bind (device-x device-y)
        (transform-position (medium-device-transformation medium) x y)
      (multiple-value-bind (width height-in-pixels) (gpu-medium-size medium)
        (let ((vertices (gpu-medium-relief-vertices medium)))
          (flet ((push-value (value)
                   (vector-push-extend (coerce value 'single-float) vertices)))
            (dolist (value
                      (list (- (* 2 (/ device-x width)) 1)
                            (- (* 2 (/ device-y height-in-pixels)) 1)
                            (fourth color)
                            local-x local-y 0.0
                            half-width half-height radius
                            (first color) (second color) (third color)
                            height 0.0 0.0))
              (push-value value))))))))

(defun gpu-medium-append-relief-analytic-quad
    (medium center x-axis y-axis half-width half-height radius color height)
  (alexandria:when-let
      ((padding (gpu-medium-analytic-padding medium x-axis y-axis)))
    (let* ((vertices (gpu-medium-relief-vertices medium))
           (first-vertex (/ (length vertices) 15))
           (left (- (+ half-width padding)))
           (right (+ half-width padding))
           (top (- (+ half-height padding)))
           (bottom (+ half-height padding)))
      (flet ((vertex (x y)
               (gpu-medium-push-relief-analytic-vertex
                medium center x-axis y-axis x y half-width half-height radius
                color height)))
        (vertex left bottom)
        (vertex right bottom)
        (vertex right top)
        (vertex left bottom)
        (vertex right top)
        (vertex left top))
      (vector-push-extend
       (make-gpu-relief-analytic-command
        :first-vertex first-vertex :vertex-count 6
        :clip (gpu-medium-clip-rectangle medium))
       (gpu-medium-commands medium))
      t)))

(defmethod gpu-medium-append-analytic-design
    ((design relief-design) medium center x-axis y-axis
     half-width half-height radius)
  (let ((color
          (multiple-value-list
           (color-rgba (design-ink (relief-albedo design) 0 0)))))
    ;; A construction-time pane may briefly have a singular device transform.
    ;; Consume the relief primitive in that state instead of decomposing it
    ;; into polygons that cannot preserve its height channel.
    (or (gpu-medium-append-relief-analytic-quad
         medium center x-axis y-axis half-width half-height radius color
         (relief-height design))
        t)))

(defun gpu-medium-push-gradient-analytic-vertex
    (medium gradient center x-axis y-axis local-x local-y
     half-width half-height radius)
  (let ((x (+ (first center)
              (* local-x (first x-axis))
              (* local-y (first y-axis))))
        (y (+ (second center)
              (* local-x (second x-axis))
              (* local-y (second y-axis)))))
    (multiple-value-bind (device-x device-y)
        (transform-position (medium-device-transformation medium) x y)
      (multiple-value-bind (width height) (gpu-medium-size medium)
        (multiple-value-bind (paint-x paint-y paint-kind)
            (gradient-render-coordinate gradient x y)
          (multiple-value-bind (r1 g1 b1 a1)
              (color-rgba (gradient-start-color gradient))
            (multiple-value-bind (r2 g2 b2 a2)
                (color-rgba (gradient-end-color gradient))
              (let ((vertices (gpu-medium-gradient-vertices medium)))
                (flet ((push-value (value)
                         (vector-push-extend
                          (coerce value 'single-float) vertices)))
                  (dolist (value
                            (list (- (* 2 (/ device-x width)) 1)
                                  (- (* 2 (/ device-y height)) 1)
                                  0.0
                                  local-x local-y 0.0
                                  half-width half-height radius
                                  paint-x paint-y paint-kind
                                  r1 g1 b1 r2 g2 b2 a1 a2 0.0))
                    (push-value value)))))))))))

(defun gpu-medium-append-gradient-analytic-quad
    (medium gradient center x-axis y-axis half-width half-height radius)
  (alexandria:when-let
      ((padding (gpu-medium-analytic-padding medium x-axis y-axis)))
    (let* ((vertices (gpu-medium-gradient-vertices medium))
           (first-vertex (/ (length vertices) 21))
           (left (- (+ half-width padding)))
           (right (+ half-width padding))
           (top (- (+ half-height padding)))
           (bottom (+ half-height padding)))
      (flet ((vertex (x y)
               (gpu-medium-push-gradient-analytic-vertex
                medium gradient center x-axis y-axis x y
                half-width half-height radius)))
        (vertex left bottom)
        (vertex right bottom)
        (vertex right top)
        (vertex left bottom)
        (vertex right top)
        (vertex left top))
      (vector-push-extend
       (make-gpu-gradient-analytic-command
        :first-vertex first-vertex :vertex-count 6
        :clip (gpu-medium-clip-rectangle medium))
       (gpu-medium-commands medium))
      t)))

(defmethod gpu-medium-append-analytic-design
    ((design gradient-design) medium center x-axis y-axis
     half-width half-height radius)
  (gpu-medium-append-gradient-analytic-quad
   medium design center x-axis y-axis half-width half-height radius))

(defun gpu-image-paint-source (design)
  "Return the immutable pixel-bearing design shared by transformed paints."
  (loop while (typep design 'climi::transformed-design)
        do (setf design (climi::transformed-design-design design))
        finally (return design)))

(defun gpu-image-paint-p (design)
  (let ((source (gpu-image-paint-source design)))
    (or (typep source 'pattern)
        (and (typep source 'climi::masked-compositum)
             (not (typep source 'climi::uniform-compositum))))))

(defun gpu-image-paint-coordinate (design x y)
  "Map a point in drawing coordinates back into DESIGN's source image."
  (if (typep design 'climi::transformed-design)
      (multiple-value-bind (source-x source-y)
          (transform-position
           (invert-transformation
            (climi::transformed-design-transformation design))
           x y)
        (gpu-image-paint-coordinate
         (climi::transformed-design-design design) source-x source-y))
      (with-bounding-rectangle* (left top right bottom)
          (bounding-rectangle design)
        (values (/ (- x left) (max 1 (- right left)))
                (/ (- y top) (max 1 (- bottom top)))))))

(defun gpu-medium-push-image-vertex
    (medium design center x-axis y-axis local-x local-y
     half-width half-height radius)
  (let ((x (+ (first center)
              (* local-x (first x-axis))
              (* local-y (first y-axis))))
        (y (+ (second center)
              (* local-x (second x-axis))
              (* local-y (second y-axis)))))
    (multiple-value-bind (device-x device-y)
        (transform-position (medium-device-transformation medium) x y)
      (multiple-value-bind (width height) (gpu-medium-size medium)
        (multiple-value-bind (u v)
            (gpu-image-paint-coordinate design x y)
          (let ((vertices (gpu-medium-image-vertices medium)))
            (flet ((push-value (value)
                     (vector-push-extend
                      (coerce value 'single-float) vertices)))
              (dolist (value
                        (list (- (* 2 (/ device-x width)) 1)
                              (- (* 2 (/ device-y height)) 1)
                              0.0
                              local-x local-y 0.0
                              half-width half-height radius
                              u v 1.0))
                (push-value value)))))))))

(defun gpu-medium-append-image-analytic-quad
    (medium design center x-axis y-axis half-width half-height radius)
  (alexandria:when-let
      ((padding (gpu-medium-analytic-padding medium x-axis y-axis)))
    (let* ((vertices (gpu-medium-image-vertices medium))
           (first-vertex (/ (length vertices) 12))
           (left (- (+ half-width padding)))
           (right (+ half-width padding))
           (top (- (+ half-height padding)))
           (bottom (+ half-height padding)))
      (flet ((vertex (x y)
               (gpu-medium-push-image-vertex
                medium design center x-axis y-axis x y
                half-width half-height radius)))
        (vertex left bottom)
        (vertex right bottom)
        (vertex right top)
        (vertex left bottom)
        (vertex right top)
        (vertex left top))
      (vector-push-extend
      (make-gpu-image-command
        :design design :first-vertex first-vertex :vertex-count 6
        :clip (gpu-medium-clip-rectangle medium))
       (gpu-medium-commands medium))
      t)))

(defmethod gpu-medium-append-analytic-design
    ((design pattern) medium center x-axis y-axis
     half-width half-height radius)
  (gpu-medium-append-image-analytic-quad
   medium design center x-axis y-axis half-width half-height radius))

(defmethod gpu-medium-append-analytic-design
    ((design climi::masked-compositum) medium center x-axis y-axis
     half-width half-height radius)
  (if (typep design 'climi::uniform-compositum)
      (call-next-method)
      (gpu-medium-append-image-analytic-quad
       medium design center x-axis y-axis half-width half-height radius)))

(defmethod gpu-medium-append-analytic-design
    ((design climi::transformed-design) medium center x-axis y-axis
     half-width half-height radius)
  (if (gpu-image-paint-p design)
      (gpu-medium-append-image-analytic-quad
       medium design center x-axis y-axis half-width half-height radius)
      (call-next-method)))

(defmethod gpu-medium-append-analytic-design
    (design medium center x-axis y-axis half-width half-height radius)
  (handler-case
      (gpu-medium-append-analytic-quad
       medium center x-axis y-axis half-width half-height radius
       (multiple-value-list (color-rgba design)))
    (error ()
      (error 'gpu-medium-unsupported-design :design design))))

(defun coordinate-pairs (coordinates)
  (loop for (x y) on (coerce coordinates 'list) by #'cddr
        while y
        collect (list x y)))

(defun gpu-medium-fill-convex-polygon (medium points color)
  (when (>= (length points) 3)
    (let ((origin (first points)))
      (loop for tail on (rest points)
            while (second tail)
            do (gpu-medium-push-triangle
                medium origin (first tail) (second tail) color)))))

(defgeneric gpu-medium-append-painted-polygon (design medium points)
  (:documentation
   "Append one convex polygon using DESIGN's native paint path, or NIL."))

(defmethod gpu-medium-append-painted-polygon (design medium points)
  (declare (ignore design medium points))
  nil)

(defun gpu-medium-append-gradient-polygon (medium gradient points)
  (when (>= (length points) 3)
    (let* ((vertices (gpu-medium-gradient-vertices medium))
           (first-vertex (/ (length vertices) 21))
           (origin (first points)))
      (flet ((vertex (point)
               (gpu-medium-push-gradient-analytic-vertex
                medium gradient '(0.0 0.0) '(1.0 0.0) '(0.0 1.0)
                (first point) (second point) 0.0 0.0 -1.0)))
        (loop for tail on (rest points)
              while (second tail)
              do (vertex origin)
                 (vertex (first tail))
                 (vertex (second tail))))
      (let ((vertex-count
              (- (/ (length vertices) 21) first-vertex)))
        (vector-push-extend
         (make-gpu-gradient-analytic-command
          :first-vertex first-vertex :vertex-count vertex-count
          :clip (gpu-medium-clip-rectangle medium))
         (gpu-medium-commands medium))
        vertex-count))))

(defmethod gpu-medium-append-painted-polygon
    ((design gradient-design) medium points)
  (gpu-medium-append-gradient-polygon medium design points))

(defun gpu-medium-append-image-polygon (medium design points)
  (when (>= (length points) 3)
    (let* ((vertices (gpu-medium-image-vertices medium))
           (first-vertex (/ (length vertices) 12))
           (origin (first points)))
      (flet ((vertex (point)
               (gpu-medium-push-image-vertex
                medium design '(0.0 0.0) '(1.0 0.0) '(0.0 1.0)
                (first point) (second point) 0.0 0.0 -1.0)))
        (loop for tail on (rest points)
              while (second tail)
              do (vertex origin)
                 (vertex (first tail))
                 (vertex (second tail))))
      (let ((vertex-count (- (/ (length vertices) 12) first-vertex)))
        (vector-push-extend
         (make-gpu-image-command
          :design design :first-vertex first-vertex
          :vertex-count vertex-count
          :clip (gpu-medium-clip-rectangle medium))
         (gpu-medium-commands medium))
        vertex-count))))

(defmethod gpu-medium-append-painted-polygon
    ((design pattern) medium points)
  (gpu-medium-append-image-polygon medium design points))

(defmethod gpu-medium-append-painted-polygon
    ((design climi::masked-compositum) medium points)
  (unless (typep design 'climi::uniform-compositum)
    (gpu-medium-append-image-polygon medium design points)))

(defmethod gpu-medium-append-painted-polygon
    ((design climi::transformed-design) medium points)
  (when (gpu-image-paint-p design)
    (gpu-medium-append-image-polygon medium design points)))

(defun gpu-medium-stroke-segment (medium start end thickness color)
  (let* ((dx (- (first end) (first start)))
         (dy (- (second end) (second start)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (unless (zerop length)
      (let* ((scale (/ (* 0.5 thickness) length))
             (nx (* (- dy) scale))
             (ny (* dx scale))
             (a (list (+ (first start) nx) (+ (second start) ny)))
             (b (list (- (first start) nx) (- (second start) ny)))
             (c (list (- (first end) nx) (- (second end) ny)))
             (d (list (+ (first end) nx) (+ (second end) ny))))
        (gpu-medium-push-triangle medium a b c color)
        (gpu-medium-push-triangle medium a c d color)))))

;;; BASIC-MEDIUM deliberately implements these primitives in terms of its
;;; polygon leaf. The around methods retain that useful behavior while making
;;; the otherwise invisible decomposition measurable by the gallery.

(defmethod medium-draw-point* :around
    ((medium luv-gpu-medium) x y)
  (with-gpu-medium-fallback (medium :point)
    (call-next-method medium x y)))

(defmethod medium-draw-line* :around
    ((medium luv-gpu-medium) x1 y1 x2 y2)
  (with-gpu-medium-fallback (medium :line)
    (call-next-method medium x1 y1 x2 y2)))

(defmethod medium-draw-rectangle*
    ((medium luv-gpu-medium) x1 y1 x2 y2 filled)
  (if filled
      (let* ((left (min x1 x2))
             (right (max x1 x2))
             (top (min y1 y2))
             (bottom (max y1 y2))
             (half-width (* 0.5 (- right left)))
             (half-height (* 0.5 (- bottom top))))
        (unless (gpu-medium-append-analytic-design
                 (medium-ink medium) medium
                 (list (* 0.5 (+ left right)) (* 0.5 (+ top bottom)))
                 '(1.0 0.0) '(0.0 1.0)
                 half-width half-height 0.0)
          (with-gpu-medium-fallback (medium :rectangle)
            (call-next-method medium x1 y1 x2 y2 filled))))
      (with-gpu-medium-fallback (medium :rectangle)
        (call-next-method medium x1 y1 x2 y2 filled)))
  medium)

(defun complete-ellipse-p (eta1 eta2)
  (>= (abs (- eta2 eta1)) (- (* 2 pi) 1.0e-6)))

(defmethod medium-draw-ellipse*
    ((medium luv-gpu-medium)
     cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled)
  (if (and filled (complete-ellipse-p eta1 eta2)
           (gpu-medium-append-analytic-design
            (medium-ink medium) medium
            (list cx cy) (list rdx1 rdy1) (list rdx2 rdy2)
            1.0 1.0 1.0))
      medium
      (with-gpu-medium-fallback (medium :ellipse)
        (call-next-method
         medium cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled))))

(defmethod climi::medium-draw-circle*
    ((medium luv-gpu-medium) cx cy radius eta1 eta2 filled)
  (if (and filled (complete-ellipse-p eta1 eta2)
           (gpu-medium-append-analytic-design
            (medium-ink medium) medium
            (list cx cy) (list radius 0.0) (list 0.0 radius)
            1.0 1.0 1.0))
      medium
      (with-gpu-medium-fallback (medium :circle)
        (call-next-method medium cx cy radius eta1 eta2 filled))))

(defgeneric medium-draw-analytic-rounded-rectangle*
    (medium x1 y1 x2 y2 radius filled)
  (:documentation
   "Draw the uniform-radius roundrect primitive when MEDIUM supports it."))

(defmethod medium-draw-analytic-rounded-rectangle*
    (medium x1 y1 x2 y2 radius filled)
  (draw-rounded-rectangle*
   medium x1 y1 x2 y2 :radius radius :filled filled))

(defun clear-raster-medium-reliefs (medium)
  (setf (fill-pointer (raster-medium-reliefs medium)) 0)
  medium)

(defmethod medium-draw-analytic-rounded-rectangle*
    ((medium luv-raster-medium) x1 y1 x2 y2 radius filled)
  (when (and filled (typep (medium-ink medium) 'relief-design))
    (let ((transformation (medium-device-transformation medium)))
      (with-bounding-rectangle* (left top right bottom)
          (transform-region transformation (make-rectangle* x1 y1 x2 y2))
        (multiple-value-bind (radius-xx radius-xy)
            (transform-distance transformation radius 0)
          (multiple-value-bind (radius-yx radius-yy)
              (transform-distance transformation 0 radius)
            (vector-push-extend
             (make-surface-relief
              :x1 left :y1 top :x2 right :y2 bottom
              :radius
              (min (sqrt (+ (* radius-xx radius-xx)
                            (* radius-xy radius-xy)))
                   (sqrt (+ (* radius-yx radius-yx)
                            (* radius-yy radius-yy))))
              :height (relief-height (medium-ink medium)))
             (raster-medium-reliefs medium)))))))
  (call-next-method))

(defmethod medium-draw-analytic-rounded-rectangle*
    ((medium luv-gpu-medium) x1 y1 x2 y2 radius filled)
  (if filled
      (let* ((left (min x1 x2))
             (right (max x1 x2))
             (top (min y1 y2))
             (bottom (max y1 y2))
             (half-width (* 0.5 (- right left)))
             (half-height (* 0.5 (- bottom top))))
        (if (gpu-medium-append-analytic-design
             (medium-ink medium) medium
             (list (* 0.5 (+ left right)) (* 0.5 (+ top bottom)))
             '(1.0 0.0) '(0.0 1.0) half-width half-height radius)
            medium
            (call-next-method)))
      (call-next-method)))

;; McCLIM's own DRAW-ROUNDED-RECTANGLE* is a convenience function whose
;; decomposition loses the semantic primitive. Give this extension a real
;; displayed output record so ordinary application panes retain and replay it
;; as one command. DEF-GRECORDING supplies the standard ink/transformation
;; capture and replay behavior used by McCLIM's built-in drawing operations.
(climi::def-grecording draw-analytic-rounded-rectangle
    (climi::gs-transformation-mixin)
    (x1 y1 x2 y2 radius filled)
  (with-bounding-rectangle* (left top right bottom)
      (transform-region
       (medium-transformation stream)
       (make-rectangle* x1 y1 x2 y2))
    (values left top right bottom)))

;; DEF-GRECORDING normally reaches a built-in medium operation through
;; McCLIM's stream-forwarding methods. This is a new generic, so provide the
;; equivalent drawing leg explicitly while retaining the generated recording
;; leg and output-record class.
(defmethod medium-draw-analytic-rounded-rectangle* :around
    ((stream output-recording-stream) x1 y1 x2 y2 radius filled)
  (cond
    ((stream-recording-p stream)
     (let ((record
             (make-instance
              'draw-analytic-rounded-rectangle-output-record
              :stream stream :x1 x1 :y1 y1 :x2 x2 :y2 y2
              :radius radius :filled filled)))
       (stream-add-output-record stream record)))
    ((stream-drawing-p stream)
     (with-sheet-medium (medium stream)
       (medium-draw-analytic-rounded-rectangle*
        medium x1 y1 x2 y2 radius filled)))))

(defun draw-analytic-rounded-rectangle*
    (sheet x1 y1 x2 y2 &rest options
     &key (radius 7) (filled t) &allow-other-keys)
  "Draw a roundrect as one backend primitive, with portable decomposition fallback."
  (apply #'invoke-with-drawing-options sheet
         (lambda (medium)
           (medium-draw-analytic-rounded-rectangle*
            medium x1 y1 x2 y2 radius filled))
         options))

(defmethod medium-draw-bezigon* :around
    ((medium luv-gpu-medium) coordinates closed filled)
  (with-gpu-medium-fallback (medium :bezigon)
    (call-next-method medium coordinates closed filled)))

(defmethod medium-draw-pattern*
    ((medium luv-gpu-medium) pattern x y)
  (let ((width (pattern-width pattern))
        (height (pattern-height pattern)))
    (gpu-medium-append-analytic-design
     (transform-region (make-translation-transformation x y) pattern)
     medium
     (list (+ x (* 0.5 width)) (+ y (* 0.5 height)))
     '(1.0 0.0) '(0.0 1.0)
     (* 0.5 width) (* 0.5 height) 0.0))
  medium)

(defmethod medium-draw-polygon*
    ((medium luv-gpu-medium) coordinates closed filled)
  (let ((points (coordinate-pairs coordinates)))
    (note-gpu-medium-fallback
     medium (or *gpu-medium-fallback-source* :direct-polygon)
     :polygon-points (length points))
    (let ((painted-vertex-count
            (and filled
                 (gpu-medium-append-painted-polygon
                  (medium-ink medium) medium points)))
          (first-vertex (/ (length (gpu-medium-vertices medium)) 6)))
      (unless painted-vertex-count
        (let ((color (gpu-medium-color medium)))
          (if filled
              (gpu-medium-fill-convex-polygon medium points color)
              (let ((thickness
                      (line-style-effective-thickness
                       (medium-line-style medium) medium)))
                (loop for (start end) on points
                      while end
                      do (gpu-medium-stroke-segment
                          medium start end thickness color))
                (when (and closed (> (length points) 2))
                  (gpu-medium-stroke-segment
                   medium (car (last points)) (first points)
                   thickness color))))))
      (let ((vertex-count
              (or painted-vertex-count
                  (- (/ (length (gpu-medium-vertices medium)) 6)
                     first-vertex))))
        (when (plusp vertex-count)
          (note-gpu-medium-fallback
           medium (or *gpu-medium-fallback-source* :direct-polygon)
           :gpu-triangles (/ vertex-count 3))
          (unless painted-vertex-count
            (vector-push-extend
             (make-gpu-solid-command
              :first-vertex first-vertex :vertex-count vertex-count
              :clip (gpu-medium-clip-rectangle medium))
             (gpu-medium-commands medium)))))))
  medium)

(defmethod invoke-with-output-buffered
    ((medium luv-gpu-medium) continuation &optional buffered-p)
  (declare (ignore medium buffered-p))
  ;; Pane-local buffering is not a frame boundary. McCLIM gives every pane a
  ;; distinct medium, so only REPAINT-GPU-MIRROR may clear, join, and publish
  ;; their retained streams in painter order.
  (funcall continuation))

(defun gpu-sheet-paint-order (sheet)
  "Return SHEET and descendants in McCLIM's repaint painter order."
  (cons sheet
        (mapcan #'gpu-sheet-paint-order
                (reverse (sheet-children sheet)))))

(defun gpu-sheet-presentation-medium (sheet)
  "Return SHEET's actual drawing medium, outside any recording context."
  (if (typep sheet 'output-recording-stream)
      (with-output-recording-options (sheet :record nil :draw t)
        (sheet-medium sheet))
      (sheet-medium sheet)))

(defun compose-gpu-mirror-media (mirror)
  "Join MIRROR's current pane-local drawing streams in painter order."
  (luv:with-cpu-trace-zone (:mcluv/compose)
    (let* ((sheet (mirror-sheet mirror))
           (media
             (remove-duplicates
              (remove-if-not
               (lambda (medium) (typep medium 'luv-gpu-medium))
               (mapcar #'gpu-sheet-presentation-medium
                       (gpu-sheet-paint-order sheet)))
              :test #'eq))
           (target-medium (gpu-sheet-presentation-medium sheet))
           (snapshots
             (mapcar
              (lambda (medium)
                (list (copy-seq (gpu-medium-vertices medium))
                      (copy-seq (gpu-medium-analytic-vertices medium))
                      (copy-seq (gpu-medium-relief-vertices medium))
                      (copy-seq (gpu-medium-gradient-vertices medium))
                      (copy-seq (gpu-medium-image-vertices medium))
                      (copy-seq (gpu-medium-commands medium))))
              media)))
      (setf (fill-pointer (gpu-medium-vertices target-medium)) 0
            (fill-pointer (gpu-medium-analytic-vertices target-medium)) 0
            (fill-pointer (gpu-medium-relief-vertices target-medium)) 0
            (fill-pointer (gpu-medium-gradient-vertices target-medium)) 0
            (fill-pointer (gpu-medium-image-vertices target-medium)) 0
            (fill-pointer (gpu-medium-commands target-medium)) 0)
      (dolist (snapshot snapshots)
        (let ((vertex-offset
                (/ (length (gpu-medium-vertices target-medium)) 6))
              (analytic-offset
                (/ (length (gpu-medium-analytic-vertices target-medium)) 12))
              (relief-offset
                (/ (length (gpu-medium-relief-vertices target-medium)) 15))
              (gradient-offset
                (/ (length (gpu-medium-gradient-vertices target-medium)) 21))
              (image-offset
                (/ (length (gpu-medium-image-vertices target-medium)) 12)))
          (loop for command across (sixth snapshot)
                do (vector-push-extend
                    (etypecase command
                      (gpu-solid-command
                      (make-gpu-solid-command
                        :first-vertex
                        (+ vertex-offset
                           (gpu-solid-command-first-vertex command))
                        :vertex-count
                        (gpu-solid-command-vertex-count command)
                        :clip (gpu-solid-command-clip command)))
                      (gpu-analytic-command
                       (make-gpu-analytic-command
                        :first-vertex
                        (+ analytic-offset
                           (gpu-analytic-command-first-vertex command))
                        :vertex-count
                        (gpu-analytic-command-vertex-count command)
                        :clip (gpu-analytic-command-clip command)))
                      (gpu-relief-analytic-command
                       (make-gpu-relief-analytic-command
                        :first-vertex
                        (+ relief-offset
                           (gpu-relief-analytic-command-first-vertex command))
                        :vertex-count
                        (gpu-relief-analytic-command-vertex-count command)
                        :clip (gpu-relief-analytic-command-clip command)))
                      (gpu-gradient-analytic-command
                       (make-gpu-gradient-analytic-command
                        :first-vertex
                        (+ gradient-offset
                           (gpu-gradient-analytic-command-first-vertex
                            command))
                        :vertex-count
                        (gpu-gradient-analytic-command-vertex-count command)
                        :clip (gpu-gradient-analytic-command-clip command)))
                      (gpu-image-command
                       (make-gpu-image-command
                        :design (gpu-image-command-design command)
                        :first-vertex
                        (+ image-offset
                           (gpu-image-command-first-vertex command))
                        :vertex-count
                        (gpu-image-command-vertex-count command)
                        :clip (gpu-image-command-clip command)))
                      (gpu-text-command command))
                    (gpu-medium-commands target-medium))))
        (loop for value across (first snapshot)
              do (vector-push-extend
                  value (gpu-medium-vertices target-medium)))
        (loop for value across (second snapshot)
              do (vector-push-extend
                  value (gpu-medium-analytic-vertices target-medium)))
        (loop for value across (third snapshot)
              do (vector-push-extend
                  value (gpu-medium-relief-vertices target-medium)))
        (loop for value across (fourth snapshot)
              do (vector-push-extend
                  value (gpu-medium-gradient-vertices target-medium)))
        (loop for value across (fifth snapshot)
              do (vector-push-extend
                  value (gpu-medium-image-vertices target-medium))))
      target-medium)))

(defun repaint-gpu-mirror (mirror &key (present-p t))
  "Rebuild MIRROR's retained triangle stream as one complete McCLIM frame."
  (luv:with-cpu-trace-zone (:mcluv/repaint)
    (let* ((sheet (mirror-sheet mirror))
           (sheets (gpu-sheet-paint-order sheet))
           (media
             (remove-duplicates
              (remove-if-not
               (lambda (medium) (typep medium 'luv-gpu-medium))
               (mapcar #'gpu-sheet-presentation-medium sheets))
              :test #'eq)))
      (dolist (medium media)
        (setf (fill-pointer (gpu-medium-vertices medium)) 0)
        (setf (fill-pointer (gpu-medium-analytic-vertices medium)) 0)
        (setf (fill-pointer (gpu-medium-relief-vertices medium)) 0)
        (setf (fill-pointer (gpu-medium-gradient-vertices medium)) 0)
        (setf (fill-pointer (gpu-medium-image-vertices medium)) 0)
        (setf (fill-pointer (gpu-medium-commands medium)) 0)
        (incf (gpu-medium-buffering-depth medium)))
      (unwind-protect
           (repaint-sheet sheet +everywhere+)
        (dolist (medium media)
          (decf (gpu-medium-buffering-depth medium))))
      ;; Each pane owns a semantic medium, but one mirror owns the ordered GPU
      ;; frame. The top-level stream is its compact presentation buffer.
      (compose-gpu-mirror-media mirror)
      (when present-p
        (present-mirror mirror)))))

(defmethod service-luv-frame-events ((mirror luv-gpu-mirror))
  (let* ((sheet (mirror-sheet mirror))
         (frame (pane-frame sheet)))
    (unless (climi::frame-process frame)
      (drain-luv-frame-events sheet)
      (repaint-gpu-mirror mirror))))

(defmethod enable-mirror
    ((port luv-gpu-port) (sheet mirrored-sheet-mixin))
  (declare (ignore port))
  (alexandria:when-let ((mirror (sheet-direct-mirror sheet)))
    (unless (or (mirror-embedded-p mirror)
                *suppress-luv-mirror-visibility*)
      (luv:show-canvas (mirror-target mirror)))))

;;; Which TrueType file a CLIM text style means.  DejaVu ships with the
;;; system and is always there; a nicer face takes over the :SANS-SERIF
;;; family -- the default face of every McCLIM pane here -- when it can be
;;; found.  The checkout bundles Iosevka Aile (OFL, subset to the Latin,
;;; Greek, Cyrillic, punctuation, arrow, and symbol ranges a game UI needs)
;;; beside Monaspace in FONTS/; a face may also be dropped into the user's
;;; own fonts, and DejaVu stands in otherwise.

(defparameter *bundled-fonts-directory*
  (asdf:system-relative-pathname "luv/mcclim" "fonts/")
  "The checkout's bundled fonts, captured while the system is loaded.")

(defun user-font-pathname (name)
  "The font file NAME from the bundled fonts, else the user's own fonts,
else NIL."
  (or (probe-file (merge-pathnames name *bundled-fonts-directory*))
      (probe-file (merge-pathnames (format nil "Library/Fonts/~A" name)
                                   (user-homedir-pathname)))))

(defparameter *sans-serif-font-preferences*
  '(("Iosevka Aile" "IosevkaAile-Regular.ttf" "IosevkaAile-Bold.ttf"
     "IosevkaAile-Italic.ttf" "IosevkaAile-BoldItalic.ttf")
    ("Input Sans" "InputSans-Regular.ttf" "InputSans-Bold.ttf"
     "InputSans-Italic.ttf" "InputSans-BoldItalic.ttf"))
  "Families to try for :SANS-SERIF, best first: a name and the regular,
bold, italic, and bold-italic files looked for in the bundled and then the
user's fonts.  Italics are optional; the upright stands in.")

(defparameter *gpu-sans-serif-fonts*
  (cons (cl-dejavu:font-pathname "DejaVuSans.ttf")
        (cl-dejavu:font-pathname "DejaVuSans-Bold.ttf"))
  "The regular and bold files behind the :SANS-SERIF family on the GPU
text path; ADOPT-USER-SANS-SERIF-FONTS retargets them.")

(defvar *adopted-sans-serif-family* "DejaVu Sans"
  "The name of the family :SANS-SERIF currently resolves to.")

(defun adopt-user-sans-serif-fonts ()
  "Point both text paths' :SANS-SERIF at the best installed preference.

The GPU medium reads *GPU-SANS-SERIF-FONTS*; the raster medium goes through
MCCLIM-RENDER's *FAMILIES/FACES* table, so that is retargeted too, and any
raster port already open forgets the faces it had cached.  Returns the
family name adopted."
  (loop for (name regular bold italic bold-italic)
          in *sans-serif-font-preferences*
        for regular-file = (user-font-pathname regular)
        for bold-file = (user-font-pathname bold)
        when (and regular-file bold-file)
          do (setf *gpu-sans-serif-fonts* (cons regular-file bold-file)
                   *adopted-sans-serif-family* name)
             (flet ((retarget (key pathname)
                      (when pathname
                        (let ((entry (assoc key mcclim-truetype:*families/faces*
                                            :test #'equal)))
                          (if entry
                              (setf (cdr entry) pathname)
                              (push (cons key pathname)
                                    mcclim-truetype:*families/faces*))))))
               (retarget '(:sans-serif :roman) regular-file)
               (retarget '(:sans-serif :bold) bold-file)
               (retarget '(:sans-serif :italic)
                         (or (user-font-pathname italic) regular-file))
               (retarget '(:sans-serif (:bold :italic))
                         (or (user-font-pathname bold-italic) bold-file))
               (retarget '(:sans-serif (:italic :bold))
                         (or (user-font-pathname bold-italic) bold-file)))
             (dolist (port climi::*all-ports*)
               (when (typep port 'mcclim-truetype:ttf-port-mixin)
                 (mcclim-truetype::invalidate-port-font-cache port)
                 ;; The basic port keeps its own memo of style to font on top
                 ;; of the TrueType caches; without clearing it the old faces
                 ;; keep being served.
                 (clrhash (climi::port-text-style-mappings port))))
             (return name)
        finally (return *adopted-sans-serif-family*)))

(adopt-user-sans-serif-fonts)

(defun gpu-text-font-pathname (text-style)
  (multiple-value-bind (family face size)
      (text-style-components (climb:parse-text-style* text-style))
    (declare (ignore size))
    (let ((bold-p (member :bold (if (listp face) face (list face)))))
      (cond
        ((eq family :fix) (cl-dejavu:font-pathname "DejaVuSansMono.ttf"))
        ((eq family :serif)
         (cl-dejavu:font-pathname
          (if bold-p "DejaVuSerif-Bold.ttf" "DejaVuSerif.ttf")))
        (bold-p (cdr *gpu-sans-serif-fonts*))
        (t (car *gpu-sans-serif-fonts*))))))

(defun gpu-text-style-size (text-style)
  (nth-value 2
             (text-style-components (climb:parse-text-style* text-style))))

(defun gpu-font-metric (text-style reader)
  (let ((pathname (gpu-text-font-pathname text-style))
        (size (gpu-text-style-size text-style)))
    (zpb-ttf:with-font-loader (font pathname)
      (* size (/ (funcall reader font) (zpb-ttf:units/em font))))))

(defmethod text-style-ascent (text-style (medium luv-gpu-medium))
  (declare (ignore medium))
  (gpu-font-metric text-style #'zpb-ttf:ascender))

(defmethod text-style-descent (text-style (medium luv-gpu-medium))
  (declare (ignore medium))
  (- (gpu-font-metric text-style #'zpb-ttf:descender)))

(defmethod text-style-character-width
    (text-style (medium luv-gpu-medium) character)
  (nth-value 0 (text-size medium (string character) :text-style text-style)))

(defmethod text-size
    ((medium luv-gpu-medium) string &key text-style (start 0) end)
  (declare (ignore medium))
  (let* ((text-style
           (climb:parse-text-style*
            (merge-text-styles text-style *default-text-style*)))
         (text (subseq (string string) start end))
         (font (gpu-text-font-pathname text-style))
         (size (gpu-text-style-size text-style))
         (shaped (luv.slug:shape-slug-text text font))
         (unit (/ size (luv.slug:slug-shaped-text-units-per-em shaped)))
         (width (* unit (luv.slug:slug-shaped-text-x-advance shaped)))
         (ascent (gpu-font-metric text-style #'zpb-ttf:ascender))
         (descent (- (gpu-font-metric text-style #'zpb-ttf:descender))))
    (values width (+ ascent descent) width 0 ascent)))

(defmethod text-bounding-rectangle*
    ((medium luv-gpu-medium) string &key text-style (start 0) end)
  (multiple-value-bind (width height cursor-dx cursor-dy baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (values 0 (- baseline) width (- height baseline)
            cursor-dx cursor-dy)))

(defmethod medium-draw-text*
    ((medium luv-gpu-medium) string x y start end align-x align-y
     toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (let* ((string (string string))
         (end (min (or end (length string)) (length string)))
         (text (subseq string start end))
         (style
           (climb:parse-text-style*
            (merge-text-styles
             (medium-text-style medium) *default-text-style*)))
         (color (gpu-medium-color medium)))
    (multiple-value-bind (device-x device-y)
        (transform-position (medium-device-transformation medium) x y)
      (unless (zerop (length text))
        (vector-push-extend
         (make-gpu-text-command
          :string text :x device-x :y device-y
          :font-pathname (gpu-text-font-pathname style)
          :size (gpu-text-style-size style) :color color
          :align-x align-x :align-y align-y
          :clip (gpu-medium-clip-rectangle medium))
         (gpu-medium-commands medium)))))
  nil)

(defun ensure-gpu-mirror-context (mirror &key readback-p)
  (when (mirror-embedded-p mirror)
    (return-from ensure-gpu-mirror-context (mirror-context mirror)))
  (let* ((target (mirror-target mirror))
         (usage (if readback-p
                    '(:render-attachment :copy-src)
                    '(:render-attachment)))
         (device
           (or (mirror-device mirror)
               (setf (mirror-device mirror)
                     (luv:request-gpu-device luv:*gpu-provider*))))
         (context
           (or (mirror-context mirror)
               (setf (mirror-context mirror)
                     (luv:make-canvas-context
                     target luv:*gpu-provider*
                      (luv:make-canvas-configuration
                       :device device :usage usage))))))
    (multiple-value-bind (width height) (luv:canvas-size target)
      (unless (and
               (equal (list width height) (luv:canvas-extent context))
               (equal usage
                      (luv:canvas-configuration-usage
                       (luv::canvas-context-configuration context))))
        (luv:configure-canvas-context
         context
         (luv:make-canvas-configuration
          :device device :format (luv:canvas-format context)
          :usage usage))))
    context))

(defun release-gpu-mirror-pipeline (mirror)
  (maphash
   (lambda (design paint)
     (declare (ignore design))
     (alexandria:when-let ((group (gpu-image-paint-bind-group paint)))
       (luv:destroy group))
     (luv:destroy (gpu-image-paint-view paint))
     (luv:destroy (gpu-image-paint-texture paint)))
   (gpu-mirror-image-paints mirror))
  (clrhash (gpu-mirror-image-paints mirror))
  (maphash (lambda (atlas group)
             (declare (ignore atlas))
             (luv:destroy group))
           (gpu-mirror-text-bind-groups mirror))
  (clrhash (gpu-mirror-text-bind-groups mirror))
  (alexandria:when-let ((cache (gpu-mirror-slug-cache mirror)))
    (luv.slug:release-slug-glyph-cache cache))
  (dolist (resource
            (list (gpu-mirror-pipeline mirror)
                  (gpu-mirror-analytic-pipeline mirror)
                  (gpu-mirror-relief-pipeline mirror)
                  (gpu-mirror-gradient-analytic-pipeline mirror)
                  (gpu-mirror-image-pipeline mirror)
                  (gpu-mirror-text-pipeline mirror)
                  (gpu-mirror-bind-group mirror)
                  (gpu-mirror-uniform-buffer mirror)
                  (gpu-mirror-text-fragment-module mirror)
                  (gpu-mirror-text-vertex-module mirror)
                  (gpu-mirror-text-layout mirror)
                  (gpu-mirror-fragment-module mirror)
                  (gpu-mirror-vertex-module mirror)
                  (gpu-mirror-analytic-fragment-module mirror)
                  (gpu-mirror-analytic-vertex-module mirror)
                  (gpu-mirror-relief-fragment-module mirror)
                  (gpu-mirror-relief-vertex-module mirror)
                  (gpu-mirror-gradient-analytic-fragment-module mirror)
                  (gpu-mirror-gradient-analytic-vertex-module mirror)
                  (gpu-mirror-image-sampler mirror)
                  (gpu-mirror-image-fragment-module mirror)
                  (gpu-mirror-image-vertex-module mirror)
                  (gpu-mirror-image-layout mirror)
                  (gpu-mirror-layout mirror)))
    (when resource (luv:destroy resource)))
  (setf (gpu-mirror-pipeline mirror) nil
        (gpu-mirror-fragment-module mirror) nil
        (gpu-mirror-vertex-module mirror) nil
        (gpu-mirror-analytic-pipeline mirror) nil
        (gpu-mirror-analytic-fragment-module mirror) nil
        (gpu-mirror-analytic-vertex-module mirror) nil
        (gpu-mirror-relief-pipeline mirror) nil
        (gpu-mirror-relief-fragment-module mirror) nil
        (gpu-mirror-relief-vertex-module mirror) nil
        (gpu-mirror-gradient-analytic-pipeline mirror) nil
        (gpu-mirror-gradient-analytic-fragment-module mirror) nil
        (gpu-mirror-gradient-analytic-vertex-module mirror) nil
        (gpu-mirror-image-pipeline mirror) nil
        (gpu-mirror-image-sampler mirror) nil
        (gpu-mirror-image-fragment-module mirror) nil
        (gpu-mirror-image-vertex-module mirror) nil
        (gpu-mirror-image-layout mirror) nil
        (gpu-mirror-layout mirror) nil
        (gpu-mirror-uniform-buffer mirror) nil
        (gpu-mirror-bind-group mirror) nil
        (gpu-mirror-format mirror) nil
        (gpu-mirror-slug-cache mirror) nil
        (gpu-mirror-text-pipeline mirror) nil
        (gpu-mirror-text-fragment-module mirror) nil
        (gpu-mirror-text-vertex-module mirror) nil
        (gpu-mirror-text-layout mirror) nil))

(defun ensure-gpu-mirror-analytic-pipeline (mirror device format)
  (unless (gpu-mirror-analytic-pipeline mirror)
    (let ((vertex nil) (fragment nil) (pipeline nil) (completed-p nil))
      (unwind-protect
           (progn
             (setf vertex
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM analytic vertex" :language :mathematical
                     :code (luv.analytic:roundrect-vertex-specification)))
                   fragment
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM analytic fragment" :language :mathematical
                     :code (luv.analytic:roundrect-fragment-specification)))
                   pipeline
                   (luv:create
                    device
                    (luv:make-render-pipeline-descriptor
                     :label "direct McCLIM analytic shapes"
                     :layout (gpu-mirror-layout mirror)
                     :vertex
                     `(:module ,vertex
                       :buffers
                       ((:array-stride 48
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)
                          (:shader-location 1 :offset 12 :format :float32x3)
                          (:shader-location 2 :offset 24 :format :float32x3)
                          (:shader-location 3 :offset 36 :format :float32x3)))))
                     :fragment
                     `(:module ,fragment
                       :targets
                       ((:format ,format :blend :premultiplied-alpha)))
                     :primitive '(:topology :triangle-list))))
             (setf (gpu-mirror-analytic-vertex-module mirror) vertex
                   (gpu-mirror-analytic-fragment-module mirror) fragment
                   (gpu-mirror-analytic-pipeline mirror) pipeline
                   completed-p t))
        (unless completed-p
          (dolist (resource (remove nil (list pipeline fragment vertex)))
            (luv:destroy resource)))))))

(defun ensure-gpu-mirror-relief-pipeline (mirror device format)
  (unless (gpu-mirror-relief-pipeline mirror)
    (let ((vertex nil) (fragment nil) (pipeline nil) (completed-p nil))
      (unwind-protect
           (progn
             (setf vertex
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM relief vertex" :language :mathematical
                     :code (relief-roundrect-vertex-specification)))
                   fragment
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM relief fragment" :language :mathematical
                     :code (relief-roundrect-fragment-specification)))
                   pipeline
                   (luv:create
                    device
                    (luv:make-render-pipeline-descriptor
                     :label "direct McCLIM analytical relief"
                     :layout (gpu-mirror-layout mirror)
                     :vertex
                     `(:module ,vertex
                       :buffers
                       ((:array-stride 60
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)
                          (:shader-location 1 :offset 12 :format :float32x3)
                          (:shader-location 2 :offset 24 :format :float32x3)
                          (:shader-location 3 :offset 36 :format :float32x3)
                          (:shader-location 4 :offset 48 :format :float32x3)))))
                     :fragment
                     `(:module ,fragment
                       :targets
                       ((:format ,format :blend :premultiplied-alpha)))
                     :primitive '(:topology :triangle-list))))
             (setf (gpu-mirror-relief-vertex-module mirror) vertex
                   (gpu-mirror-relief-fragment-module mirror) fragment
                   (gpu-mirror-relief-pipeline mirror) pipeline
                   completed-p t))
        (unless completed-p
          (dolist (resource (remove nil (list pipeline fragment vertex)))
            (luv:destroy resource)))))))

(defun ensure-gpu-mirror-image-pipeline (mirror device format)
  (unless (gpu-mirror-image-pipeline mirror)
    (let ((vertex nil) (fragment nil) (layout nil) (sampler nil)
          (pipeline nil) (completed-p nil))
      (unwind-protect
           (progn
             (setf vertex
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM image analytic vertex"
                     :language :mathematical
                     :code (image-roundrect-vertex-specification)))
                   fragment
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM image analytic fragment"
                     :language :mathematical
                     :code (image-roundrect-fragment-specification)))
                   layout
                   (luv:create
                    device
                    (luv:make-bind-group-layout-descriptor
                     :label "McCLIM image paint layout"
                     :entries '((:binding 0 :type :texture)
                                (:binding 1 :type :sampler))))
                   sampler
                   (luv:create
                    device
                    (luv:make-sampler-descriptor
                     :label "McCLIM image paint linear sampler"))
                   pipeline
                   (luv:create
                    device
                    (luv:make-render-pipeline-descriptor
                     :label "direct McCLIM image paints"
                     :layout layout
                     :vertex
                     `(:module ,vertex
                       :buffers
                       ((:array-stride 48
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)
                          (:shader-location 1 :offset 12 :format :float32x3)
                          (:shader-location 2 :offset 24 :format :float32x3)
                          (:shader-location 3 :offset 36 :format :float32x3)))))
                     :fragment
                     `(:module ,fragment
                       :targets
                       ((:format ,format :blend :premultiplied-alpha)))
                     :primitive '(:topology :triangle-list))))
             (setf (gpu-mirror-image-vertex-module mirror) vertex
                   (gpu-mirror-image-fragment-module mirror) fragment
                   (gpu-mirror-image-layout mirror) layout
                   (gpu-mirror-image-sampler mirror) sampler
                   (gpu-mirror-image-pipeline mirror) pipeline
                   completed-p t))
        (unless completed-p
          (dolist (resource
                    (remove nil
                            (list pipeline sampler layout fragment vertex)))
            (luv:destroy resource)))))))

(defun ensure-gpu-mirror-gradient-analytic-pipeline (mirror device format)
  (unless (gpu-mirror-gradient-analytic-pipeline mirror)
    (let ((vertex nil) (fragment nil) (pipeline nil) (completed-p nil))
      (unwind-protect
           (progn
             (setf vertex
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM gradient analytic vertex"
                     :language :mathematical
                     :code (gradient-roundrect-vertex-specification)))
                   fragment
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM gradient analytic fragment"
                     :language :mathematical
                     :code (gradient-roundrect-fragment-specification)))
                   pipeline
                   (luv:create
                    device
                    (luv:make-render-pipeline-descriptor
                     :label "direct McCLIM gradient analytic shapes"
                     :layout (gpu-mirror-layout mirror)
                     :vertex
                     `(:module ,vertex
                       :buffers
                       ((:array-stride 84
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)
                          (:shader-location 1 :offset 12 :format :float32x3)
                          (:shader-location 2 :offset 24 :format :float32x3)
                          (:shader-location 3 :offset 36 :format :float32x3)
                          (:shader-location 4 :offset 48 :format :float32x3)
                          (:shader-location 5 :offset 60 :format :float32x3)
                          (:shader-location 6 :offset 72 :format :float32x3)))))
                     :fragment
                     `(:module ,fragment
                       :targets
                       ((:format ,format :blend :premultiplied-alpha)))
                     :primitive '(:topology :triangle-list))))
             (setf (gpu-mirror-gradient-analytic-vertex-module mirror) vertex
                   (gpu-mirror-gradient-analytic-fragment-module mirror)
                   fragment
                   (gpu-mirror-gradient-analytic-pipeline mirror) pipeline
                   completed-p t))
        (unless completed-p
          (dolist (resource (remove nil (list pipeline fragment vertex)))
            (luv:destroy resource)))))))

(defun ensure-gpu-mirror-text-pipeline (mirror device format)
  (unless (gpu-mirror-text-pipeline mirror)
    (let ((vertex nil) (fragment nil) (layout nil) (pipeline nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (setf vertex
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM Slug vertex" :language :mathematical
                     :code (spv:shader-specification-for
                            :mcluv-slug :vertex)))
                   fragment
                   (luv:create
                    device
                    (luv:make-shader-module-descriptor
                     :label "McCLIM Slug fragment" :language :mathematical
                     :code (spv:shader-specification-for
                            :mcluv-slug :fragment)))
                   layout
                   (luv:create
                    device
                    (luv:make-bind-group-layout-descriptor
                     :label "McCLIM Slug atlases"
                     :entries '((:binding 0 :type :texture)
                                (:binding 1 :type :texture))))
                   pipeline
                   (luv:create
                    device
                    (luv:make-render-pipeline-descriptor
                     :label "direct McCLIM Slug text"
                     :layout layout
                     :vertex
                     `(:module ,vertex
                       :buffers
                       ((:array-stride 72
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)
                          (:shader-location 1 :offset 12 :format :float32x3)
                          (:shader-location 2 :offset 24 :format :float32x3)
                          (:shader-location 3 :offset 36 :format :float32x3)
                          (:shader-location 4 :offset 48 :format :float32x3)
                          (:shader-location 5 :offset 60
                           :format :float32x3)))))
                     :fragment
                     `(:module ,fragment
                       :targets
                       ((:format ,format :blend :premultiplied-alpha)))
                     :primitive '(:topology :triangle-list))))
             (setf (gpu-mirror-text-vertex-module mirror) vertex
                   (gpu-mirror-text-fragment-module mirror) fragment
                   (gpu-mirror-text-layout mirror) layout
                   (gpu-mirror-text-pipeline mirror) pipeline
                   (gpu-mirror-slug-cache mirror)
                   (luv.slug:make-slug-glyph-cache device)
                   completed-p t))
        (unless completed-p
          (dolist (resource (remove nil (list pipeline layout fragment vertex)))
            (luv:destroy resource)))))))

(defun ensure-gpu-mirror-pipeline (mirror context)
  (let ((format (luv:canvas-format context))
        (device (luv:context-device context)))
    (unless (and (gpu-mirror-pipeline mirror)
                 (eq format (gpu-mirror-format mirror)))
      (release-gpu-mirror-pipeline mirror)
      (let* ((vertex
               (luv:create
                device
                (luv:make-shader-module-descriptor
                 :label "McCLIM solid vertex"
                 :language :mathematical
                 :code (spv:shader-specification-for :mcluv-solid :vertex))))
             (fragment nil)
             (layout nil)
             (uniform-buffer nil)
             (bind-group nil)
             (pipeline nil)
             (completed-p nil))
        (unwind-protect
             (progn
               (setf fragment
                     (luv:create
                      device
                      (luv:make-shader-module-descriptor
                       :label "McCLIM solid fragment"
                       :language :mathematical
                       :code (spv:shader-specification-for
                              :mcluv-solid :fragment)))
                     layout
                     (luv:create
                      device
                      (luv:make-bind-group-layout-descriptor
                       :label "direct McCLIM frame layout"
                       :entries '((:binding 0 :type :uniform-buffer))))
                     uniform-buffer
                     (luv:create
                      device
                      (luv:make-buffer-descriptor
                       :label "direct McCLIM frame placeholder"
                       :size 16 :usage '(:uniform :copy-dst)))
                     bind-group
                     (luv:create
                      device
                      (luv:make-bind-group-descriptor
                       :label "direct McCLIM frame bindings"
                       :layout layout
                       :entries `((:binding 0 :resource ,uniform-buffer))))
                     pipeline
                     (luv:create
                      device
                      (luv:make-render-pipeline-descriptor
                       :label "direct McCLIM solid geometry"
                       :layout layout
                       :vertex
                       `(:module ,vertex
                         :buffers
                         ((:array-stride 24
                           :attributes
                           ((:shader-location 0 :offset 0 :format :float32x3)
                            (:shader-location 1 :offset 12 :format :float32x3)))))
                       :fragment
                       `(:module ,fragment
                         :targets
                         ((:format ,format :blend :premultiplied-alpha)))
                       :primitive '(:topology :triangle-list))))
               (setf (gpu-mirror-vertex-module mirror) vertex
                     (gpu-mirror-fragment-module mirror) fragment
                     (gpu-mirror-layout mirror) layout
                     (gpu-mirror-uniform-buffer mirror) uniform-buffer
                     (gpu-mirror-bind-group mirror) bind-group
                     (gpu-mirror-pipeline mirror) pipeline
                     (gpu-mirror-format mirror) format
                     completed-p t))
          (unless completed-p
            (dolist (resource
                      (remove nil
                              (list pipeline bind-group uniform-buffer
                                    layout fragment vertex)))
              (luv:destroy resource))))))
    ;; Keep live mirrors honest when a new pipeline family is introduced after
    ;; their solid pipeline already exists.
    (ensure-gpu-mirror-analytic-pipeline mirror device format)
    (ensure-gpu-mirror-relief-pipeline mirror device format)
    (ensure-gpu-mirror-gradient-analytic-pipeline mirror device format)
    (ensure-gpu-mirror-image-pipeline mirror device format)
    (ensure-gpu-mirror-text-pipeline mirror device format)))

(defun ensure-embedded-gpu-mirror-preparation-resources (mirror device)
  "Create only what a textureless mirror needs to prepare its commands."
  (unless (gpu-mirror-slug-cache mirror)
    (setf (gpu-mirror-slug-cache mirror)
          (luv.slug:make-slug-glyph-cache device)))
  mirror)

(defun ensure-gpu-mirror-frame-state (mirror context surface)
  (let* ((key (luv:canvas-frame-resource-key context surface))
         (state (gethash key (gpu-mirror-frame-states mirror))))
    (if state
        (unless (eq surface
                    (luv:gpu-texture-view-texture
                     (gpu-frame-state-view state)))
          ;; Metal presents each drawable through a fresh borrowed texture
          ;; wrapper. Keep the stable slot's buffer, but refresh the cheap view
          ;; so it never retains the destroyed wrapper from the prior frame.
          (luv:destroy (gpu-frame-state-view state))
          (setf (gpu-frame-state-view state)
                (luv:create
                 (luv:context-device context)
                 (luv:make-texture-view-descriptor :texture surface))))
        (setf state
              (make-instance
               'gpu-mirror-frame-state
               :view
               (luv:create
                (luv:context-device context)
                (luv:make-texture-view-descriptor :texture surface)))
              (gethash key (gpu-mirror-frame-states mirror)) state))
    state))

(defun ensure-embedded-gpu-mirror-frame-state (mirror)
  (or (gpu-mirror-prepared-frame-state mirror)
      (setf (gpu-mirror-prepared-frame-state mirror)
            (make-instance 'gpu-mirror-frame-state))))

(defun upload-gpu-mirror-frame-data
    (mirror device vertices analytic-vertices relief-vertices
     gradient-vertices image-vertices text-data)
  "Upload one embedded mirror snapshot without creating a raster target."
  (let ((state (ensure-embedded-gpu-mirror-frame-state mirror)))
    (flet ((upload (data ensure-buffer)
             (when (plusp (length data))
               (let ((buffer
                       (funcall ensure-buffer
                                state device (* 4 (length data)))))
                 (luv:write-buffer buffer data)))))
      (upload vertices #'ensure-gpu-frame-vertex-buffer)
      (upload analytic-vertices #'ensure-gpu-frame-analytic-buffer)
      (upload relief-vertices #'ensure-gpu-frame-relief-buffer)
      (upload gradient-vertices #'ensure-gpu-frame-gradient-buffer)
      (upload image-vertices #'ensure-gpu-frame-image-buffer)
      (upload text-data #'ensure-gpu-frame-text-buffer))
    state))

(defun ensure-gpu-frame-vertex-buffer (state device byte-count)
  (when (> byte-count (gpu-frame-state-vertex-capacity state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              device
              (luv:make-buffer-descriptor
               :label "direct McCLIM vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let ((old (gpu-frame-state-vertex-buffer state)))
        (luv:destroy old))
      (setf (gpu-frame-state-vertex-buffer state) replacement
            (gpu-frame-state-vertex-capacity state) capacity)))
  (gpu-frame-state-vertex-buffer state))

(defun ensure-gpu-frame-text-buffer (state device byte-count)
  (when (> byte-count (gpu-frame-state-text-capacity state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              device
              (luv:make-buffer-descriptor
               :label "direct McCLIM Slug vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let ((old (gpu-frame-state-text-buffer state)))
        (luv:destroy old))
      (setf (gpu-frame-state-text-buffer state) replacement
            (gpu-frame-state-text-capacity state) capacity)))
  (gpu-frame-state-text-buffer state))

(defun ensure-gpu-frame-analytic-buffer (state device byte-count)
  (when (> byte-count (gpu-frame-state-analytic-capacity state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              device
              (luv:make-buffer-descriptor
               :label "direct McCLIM analytic vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let ((old (gpu-frame-state-analytic-buffer state)))
        (luv:destroy old))
      (setf (gpu-frame-state-analytic-buffer state) replacement
            (gpu-frame-state-analytic-capacity state) capacity)))
  (gpu-frame-state-analytic-buffer state))

(defun ensure-gpu-frame-relief-buffer (state device byte-count)
  (when (> byte-count (gpu-frame-state-relief-capacity state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              device
              (luv:make-buffer-descriptor
               :label "direct McCLIM relief vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let ((old (gpu-frame-state-relief-buffer state)))
        (luv:destroy old))
      (setf (gpu-frame-state-relief-buffer state) replacement
            (gpu-frame-state-relief-capacity state) capacity)))
  (gpu-frame-state-relief-buffer state))

(defun ensure-gpu-frame-gradient-buffer (state device byte-count)
  (when (> byte-count (gpu-frame-state-gradient-capacity state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              device
              (luv:make-buffer-descriptor
               :label "direct McCLIM gradient vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let ((old (gpu-frame-state-gradient-buffer state)))
        (luv:destroy old))
      (setf (gpu-frame-state-gradient-buffer state) replacement
            (gpu-frame-state-gradient-capacity state) capacity)))
  (gpu-frame-state-gradient-buffer state))

(defun ensure-gpu-frame-image-buffer (state device byte-count)
  (when (> byte-count (gpu-frame-state-image-capacity state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              device
              (luv:make-buffer-descriptor
               :label "direct McCLIM image paint vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let ((old (gpu-frame-state-image-buffer state)))
        (luv:destroy old))
      (setf (gpu-frame-state-image-buffer state) replacement
            (gpu-frame-state-image-capacity state) capacity)))
  (gpu-frame-state-image-buffer state))

(defun ensure-gpu-image-paint (mirror design)
  (let* ((source (gpu-image-paint-source design))
         (cache (gpu-mirror-image-paints mirror)))
    (or (gethash source cache)
        (with-bounding-rectangle* (left top right bottom)
            (bounding-rectangle source)
          (let* ((width (max 1 (ceiling (- right left))))
                 (height (max 1 (ceiling (- bottom top))))
                 (pixels
                   (pattern-array
                    (climi::%collapse-pattern
                     source left top width height)))
                 (device (mirror-device mirror))
                 (texture nil) (view nil) (bind-group nil)
                 (completed-p nil))
            (unwind-protect
                 (progn
                   ;; ARGB32 integers occupy BGRA bytes on the little-endian
                   ;; native targets, matching the portable texture format.
                   (setf texture
                         (luv:create
                          device
                          (luv:make-texture-descriptor
                           :label "cached McCLIM image paint"
                           :size (list width height) :dimensions :2d
                           :format :bgra8-unorm
                           :usage '(:texture-binding :copy-dst)))
                         view
                         (luv:create
                          device
                          (luv:make-texture-view-descriptor
                           :texture texture)))
                   (luv:write-texture
                    (luv:device-queue device)
                    (luv:make-texture-copy :texture texture)
                    pixels
                    (luv:make-texture-data-layout
                     :bytes-per-row (* 4 width)
                     :rows-per-image height)
                    (list width height))
                   (when (gpu-mirror-image-layout mirror)
                     (setf bind-group
                           (luv:create
                            device
                            (luv:make-bind-group-descriptor
                             :label "cached McCLIM image paint bindings"
                             :layout (gpu-mirror-image-layout mirror)
                             :entries
                             `((:binding 0 :resource ,view)
                               (:binding 1
                                :resource
                                ,(gpu-mirror-image-sampler mirror)))))))
                   (let ((paint
                           (make-instance
                            'gpu-cached-image-paint
                            :texture texture :view view :bind-group bind-group
                            :width width :height height)))
                     (setf (gethash source cache) paint
                           completed-p t)
                     paint))
              (unless completed-p
                (dolist (resource
                          (remove nil (list bind-group view texture)))
                  (luv:destroy resource)))))))))

(defun ensure-gpu-text-bind-group (mirror atlas)
  (or (gethash atlas (gpu-mirror-text-bind-groups mirror))
      (setf (gethash atlas (gpu-mirror-text-bind-groups mirror))
            (luv:create
             (mirror-device mirror)
             (luv:make-bind-group-descriptor
              :label "McCLIM Slug atlas bindings"
              :layout (gpu-mirror-text-layout mirror)
              :entries
              `((:binding 0
                 :resource ,(luv.slug:slug-glyph-atlas-band-view atlas))
                (:binding 1
                 :resource ,(luv.slug:slug-glyph-atlas-curve-view atlas))))))))

(defun gpu-text-aligned-baseline
    (command min-x min-y max-x max-y)
  (let ((x (gpu-text-command-x command))
        (y (gpu-text-command-y command))
        (size (gpu-text-command-size command)))
    (values
     (- x
        (* size
           (ecase (gpu-text-command-align-x command)
             ((:baseline :left) 0)
             (:center (/ (+ min-x max-x) 2))
             (:right max-x))))
     (+ y
        (* size
           (ecase (gpu-text-command-align-y command)
             (:baseline 0)
             (:top max-y)
             (:center (/ (+ min-y max-y) 2))
             (:bottom min-y)))))))

(defun append-gpu-text-vertex
    (data width height screen-x screen-y alpha outline-x outline-y
     horizontal-count band-offset curve-offset vertical-count
     min-x min-y max-x max-y color)
  (flet ((push-value (value)
           (vector-push-extend (coerce value 'single-float) data)))
    (dolist (value
              (list (- (* 2 (/ screen-x width)) 1)
                    (- (* 2 (/ screen-y height)) 1)
                    alpha
                    outline-x outline-y horizontal-count
                    band-offset curve-offset vertical-count
                    min-x min-y 0
                    max-x max-y 0
                    (* (first color) alpha)
                    (* (second color) alpha)
                    (* (third color) alpha)))
      (push-value value))))

(defun append-gpu-text-command (mirror command data width height)
  (let* ((cache (gpu-mirror-slug-cache mirror))
         (font-pathname (gpu-text-command-font-pathname command))
         (shaped
           (luv.slug:cached-slug-shaped-text
            cache font-pathname (gpu-text-command-string command)))
         (size (gpu-text-command-size command))
         (color (gpu-text-command-color command)))
    (zpb-ttf:with-font-loader (font-loader font-pathname)
      (let ((glyphs
              (luv.slug:make-slug-glyph-placements
               shaped font-loader cache font-pathname)))
        (when glyphs
          (multiple-value-bind (min-x min-y max-x max-y)
              (luv.slug:slug-text-extents glyphs shaped font-loader)
            (multiple-value-bind (baseline-x baseline-y)
                (gpu-text-aligned-baseline
                 command min-x min-y max-x max-y)
              (let ((atlas (luv.slug:slug-glyph-atlas-for cache glyphs))
                    (first-vertex (/ (length data) 18))
                    ;; The screen quad is dilated here rather than per
                    ;; vertex: its pixel scale is SIZE, and a HiDPI canvas
                    ;; only makes that an underestimate, which is the safe
                    ;; side.  Two logical pixels was the old constant.
                    (padding (max (luv.slug:slug-dilation-em size)
                                  (/ 2.0 size))))
                (dolist (glyph glyphs)
                  (let* ((resource
                           (luv.slug:slug-glyph-placement-resource glyph))
                         (serialized
                           (luv.slug:slug-device-glyph-serialized resource))
                         (location
                           (gethash
                            resource
                            (luv.slug:slug-glyph-atlas-locations atlas)))
                         (outline-left
                           (- (luv.slug:slug-glyph-placement-outline-min-x glyph)
                              padding))
                         (outline-bottom
                           (- (luv.slug:slug-glyph-placement-outline-min-y glyph)
                              padding))
                         (outline-right
                           (+ (luv.slug:slug-glyph-placement-outline-max-x glyph)
                              padding))
                         (outline-top
                           (+ (luv.slug:slug-glyph-placement-outline-max-y glyph)
                              padding))
                         (origin-x
                           (luv.slug:slug-glyph-placement-origin-x glyph))
                         (origin-y
                           (luv.slug:slug-glyph-placement-origin-y glyph))
                         (left (+ baseline-x (* size (+ origin-x outline-left))))
                         (right (+ baseline-x (* size (+ origin-x outline-right))))
                         (top (- baseline-y (* size (+ origin-y outline-top))))
                         (bottom
                           (- baseline-y (* size (+ origin-y outline-bottom))))
                         (horizontal-count
                           (luv.slug:slug-serialized-outline-horizontal-band-count
                            serialized))
                         (vertical-count
                           (luv.slug:slug-serialized-outline-vertical-band-count
                            serialized)))
                    (flet ((vertex (sx sy ox oy)
                             (append-gpu-text-vertex
                              data width height sx sy (fourth color) ox oy
                              horizontal-count (first location) (second location)
                              vertical-count
                              (luv.slug:slug-glyph-placement-outline-min-x glyph)
                              (luv.slug:slug-glyph-placement-outline-min-y glyph)
                              (luv.slug:slug-glyph-placement-outline-max-x glyph)
                              (luv.slug:slug-glyph-placement-outline-max-y glyph)
                              color)))
                      (vertex left bottom outline-left outline-bottom)
                      (vertex right bottom outline-right outline-bottom)
                      (vertex right top outline-right outline-top)
                      (vertex left bottom outline-left outline-bottom)
                      (vertex right top outline-right outline-top)
                      (vertex left top outline-left outline-top))))
                (make-gpu-prepared-text-command
                 :atlas atlas :first-vertex first-vertex
                 :vertex-count (- (/ (length data) 18) first-vertex)
                 :clip (gpu-text-command-clip command))))))))))

(defun prepare-gpu-frame-commands (mirror semantic-commands)
  (luv:with-cpu-trace-zone
      (:mcluv/prepare
       :tracy-value (length semantic-commands))
    (multiple-value-bind (width height)
        (gpu-mirror-logical-size mirror)
      (let ((text-data
              (make-array 1024 :element-type 'single-float
                          :adjustable t :fill-pointer 0))
            commands)
        (loop for command across semantic-commands
              for prepared =
                (etypecase command
                  (gpu-solid-command command)
                  (gpu-analytic-command command)
                  (gpu-relief-analytic-command command)
                  (gpu-gradient-analytic-command command)
                  (gpu-image-command
                   (make-gpu-prepared-image-command
                    :paint
                    (ensure-gpu-image-paint
                     mirror (gpu-image-command-design command))
                    :first-vertex (gpu-image-command-first-vertex command)
                    :vertex-count (gpu-image-command-vertex-count command)
                    :clip (gpu-image-command-clip command)))
                  (gpu-text-command
                   (append-gpu-text-command
                    mirror command text-data width height)))
              when prepared do (push prepared commands))
        (values (nreverse commands) text-data)))))

(defun gpu-frame-command-clip (command)
  (etypecase command
    (gpu-solid-command (gpu-solid-command-clip command))
    (gpu-analytic-command (gpu-analytic-command-clip command))
    (gpu-relief-analytic-command
     (gpu-relief-analytic-command-clip command))
    (gpu-gradient-analytic-command
     (gpu-gradient-analytic-command-clip command))
    (gpu-prepared-image-command
     (gpu-prepared-image-command-clip command))
    (gpu-prepared-text-command
     (gpu-prepared-text-command-clip command))))

(defun set-gpu-frame-scissor (pass mirror surface clip)
  "Encode CLIP in physical drawable pixels and return whether it is nonempty."
  (destructuring-bind (surface-width surface-height &rest ignored)
      (luv:gpu-texture-size surface)
    (declare (ignore ignored))
    (multiple-value-bind (logical-width logical-height)
        (gpu-mirror-logical-size mirror)
      (destructuring-bind (left top right bottom)
          (or clip (list 0 0 logical-width logical-height))
        (let* ((scale-x (/ surface-width logical-width))
               (scale-y (/ surface-height logical-height))
               (x (max 0 (min surface-width (floor (* left scale-x)))))
               (y (max 0 (min surface-height (floor (* top scale-y)))))
               (right
                 (max x (min surface-width (ceiling (* right scale-x)))))
               (bottom
                 (max y (min surface-height (ceiling (* bottom scale-y)))))
               (width (- right x))
               (height (- bottom y)))
          (when (and (plusp width) (plusp height))
            (luv:set-scissor-rect pass x y width height)
            t))))))

(defun gpu-mirror-logical-size (mirror)
  (if (mirror-embedded-p mirror)
      (let ((sheet (mirror-sheet mirror)))
        (values (max 1 (ceiling (bounding-rectangle-width sheet)))
                (max 1 (ceiling (bounding-rectangle-height sheet)))))
      (luv:canvas-logical-size (mirror-target mirror))))

(defun release-gpu-mirror-frame-states (mirror)
  (maphash
   (lambda (key state)
     (declare (ignore key))
     (dolist (buffer
               (list (gpu-frame-state-vertex-buffer state)
                     (gpu-frame-state-analytic-buffer state)
                     (gpu-frame-state-relief-buffer state)
                     (gpu-frame-state-gradient-buffer state)
                     (gpu-frame-state-image-buffer state)
                     (gpu-frame-state-text-buffer state)))
       (when buffer (luv:destroy buffer)))
     (alexandria:when-let ((view (gpu-frame-state-view state)))
       (luv:destroy view)))
   (gpu-mirror-frame-states mirror))
  (clrhash (gpu-mirror-frame-states mirror))
  (alexandria:when-let ((state (gpu-mirror-prepared-frame-state mirror)))
    (dolist (buffer
              (list (gpu-frame-state-vertex-buffer state)
                    (gpu-frame-state-analytic-buffer state)
                    (gpu-frame-state-relief-buffer state)
                    (gpu-frame-state-gradient-buffer state)
                    (gpu-frame-state-image-buffer state)
                    (gpu-frame-state-text-buffer state)))
      (when buffer (luv:destroy buffer)))
    (setf (gpu-mirror-prepared-frame-state mirror) nil))
  mirror)

(defun call-with-gpu-mirror-target (mirror context function)
  (assert (not (mirror-embedded-p mirror)))
  (luv:present-canvas-frame context function))

(defun render-gpu-mirror-frame (mirror &key readback-buffer)
  (let ((medium (sheet-medium (mirror-sheet mirror))))
    (luv:with-cpu-trace-zone
        (:mcluv/frame
         :tracy-value (length (gpu-medium-commands medium)))
      (when (and (mirror-embedded-p mirror)
                 (zerop (length (gpu-medium-commands medium))))
        (setf (gpu-mirror-prepared-commands mirror) nil)
        (return-from render-gpu-mirror-frame mirror))
      ;; Drawing may continue on the McCLIM side while canvas presentation
      ;; crosses onto its native frame thread. Upload one immutable frame
      ;; snapshot so the allocation size and the bytes written cannot drift.
      (let* ((vertices (copy-seq (gpu-medium-vertices medium)))
             (analytic-vertices
               (copy-seq (gpu-medium-analytic-vertices medium)))
             (relief-vertices
               (copy-seq (gpu-medium-relief-vertices medium)))
             (gradient-vertices
               (copy-seq (gpu-medium-gradient-vertices medium)))
             (image-vertices
               (copy-seq (gpu-medium-image-vertices medium)))
             (semantic-commands
               (copy-seq (gpu-medium-commands medium))))
        (when (plusp (length (gpu-medium-commands medium)))
          (let* ((context
                   (ensure-gpu-mirror-context
                    mirror :readback-p (not (null readback-buffer))))
                 (device (luv:context-device context))
                 (byte-count (* 4 (length vertices))))
            (if (mirror-embedded-p mirror)
                (ensure-embedded-gpu-mirror-preparation-resources mirror device)
                (ensure-gpu-mirror-pipeline mirror context))
            (multiple-value-bind (commands text-data)
                (prepare-gpu-frame-commands mirror semantic-commands)
              (when (mirror-embedded-p mirror)
                (upload-gpu-mirror-frame-data
                 mirror device vertices analytic-vertices relief-vertices
                 gradient-vertices image-vertices text-data)
                ;; Publish the semantic stream only after every family buffer
                ;; has received the matching snapshot. The game render thread
                ;; must never observe new command ranges over old bytes.
                (setf (gpu-mirror-prepared-commands mirror) commands)
                (return-from render-gpu-mirror-frame mirror))
              (setf (gpu-mirror-prepared-commands mirror) commands)
              (call-with-gpu-mirror-target
               mirror context
               (lambda (surface encoder)
                 (let* ((state
                          (ensure-gpu-mirror-frame-state
                           mirror context surface))
                        (buffer
                          (and (plusp byte-count)
                               (ensure-gpu-frame-vertex-buffer
                                state device byte-count)))
                        (analytic-byte-count
                          (* 4 (length analytic-vertices)))
                        (analytic-buffer
                          (and (plusp analytic-byte-count)
                               (ensure-gpu-frame-analytic-buffer
                                state device analytic-byte-count)))
                        (relief-byte-count (* 4 (length relief-vertices)))
                        (relief-buffer
                          (and (plusp relief-byte-count)
                               (ensure-gpu-frame-relief-buffer
                                state device relief-byte-count)))
                        (gradient-byte-count
                          (* 4 (length gradient-vertices)))
                        (gradient-buffer
                          (and (plusp gradient-byte-count)
                               (ensure-gpu-frame-gradient-buffer
                                state device gradient-byte-count)))
                        (image-byte-count (* 4 (length image-vertices)))
                        (image-buffer
                          (and (plusp image-byte-count)
                               (ensure-gpu-frame-image-buffer
                                state device image-byte-count)))
                        (text-byte-count (* 4 (length text-data)))
                        (text-buffer
                          (and (plusp text-byte-count)
                               (ensure-gpu-frame-text-buffer
                                state device text-byte-count)))
                        (pass
                          (luv:begin-render-pass
                           encoder
                           (luv:make-render-pass-descriptor
                            :label "direct McCLIM frame"
                            :color-attachments
                            `((:view ,(gpu-frame-state-view state)
                               :load-op :clear :store-op :store
                               :clear-value #(0.94 0.94 0.94 1.0)))))))
                   (when buffer (luv:write-buffer buffer vertices))
                   (when analytic-buffer
                     (luv:write-buffer analytic-buffer analytic-vertices))
                   (when relief-buffer
                     (luv:write-buffer relief-buffer relief-vertices))
                   (when gradient-buffer
                     (luv:write-buffer gradient-buffer gradient-vertices))
                   (when image-buffer
                     (luv:write-buffer image-buffer image-vertices))
                   (when text-buffer
                     (luv:write-buffer text-buffer text-data))
                   (let ((active-clip (list :unset))
                         (active-clip-visible-p t))
                     (dolist (command commands)
                       (let ((clip (gpu-frame-command-clip command)))
                         (unless (equal clip active-clip)
                           (setf active-clip clip)
                           (setf active-clip-visible-p
                                 (set-gpu-frame-scissor
                                  pass mirror surface clip))))
                       (when (and active-clip-visible-p
                                  (gpu-command-rasterized-p
                                   (mirror-compositor mirror) command))
                         (etypecase command
                           (gpu-solid-command
                            (luv:set-pipeline
                             pass (gpu-mirror-pipeline mirror))
                            (luv:set-bind-group
                             pass 0 (gpu-mirror-bind-group mirror))
                            (luv:set-vertex-buffer pass 0 buffer)
                            (luv:draw
                             pass (gpu-solid-command-vertex-count command) 1
                             (gpu-solid-command-first-vertex command)))
                           (gpu-analytic-command
                            (luv:set-pipeline
                             pass (gpu-mirror-analytic-pipeline mirror))
                            (luv:set-bind-group
                             pass 0 (gpu-mirror-bind-group mirror))
                            (luv:set-vertex-buffer pass 0 analytic-buffer)
                            (luv:draw
                             pass (gpu-analytic-command-vertex-count command) 1
                             (gpu-analytic-command-first-vertex command)))
                           (gpu-relief-analytic-command
                            (luv:set-pipeline
                             pass (gpu-mirror-relief-pipeline mirror))
                            (luv:set-bind-group
                             pass 0 (gpu-mirror-bind-group mirror))
                            (luv:set-vertex-buffer pass 0 relief-buffer)
                            (luv:draw
                             pass
                             (gpu-relief-analytic-command-vertex-count command)
                             1
                             (gpu-relief-analytic-command-first-vertex
                              command)))
                           (gpu-gradient-analytic-command
                            (luv:set-pipeline
                             pass
                             (gpu-mirror-gradient-analytic-pipeline mirror))
                            (luv:set-bind-group
                             pass 0 (gpu-mirror-bind-group mirror))
                            (luv:set-vertex-buffer pass 0 gradient-buffer)
                            (luv:draw
                             pass
                             (gpu-gradient-analytic-command-vertex-count
                              command)
                             1
                             (gpu-gradient-analytic-command-first-vertex
                              command)))
                           (gpu-prepared-image-command
                            (luv:set-pipeline
                             pass (gpu-mirror-image-pipeline mirror))
                            (luv:set-bind-group
                             pass 0
                             (gpu-image-paint-bind-group
                              (gpu-prepared-image-command-paint command)))
                            (luv:set-vertex-buffer pass 0 image-buffer)
                            (luv:draw
                             pass
                             (gpu-prepared-image-command-vertex-count command)
                             1
                             (gpu-prepared-image-command-first-vertex command)))
                           (gpu-prepared-text-command
                            (luv:set-pipeline
                             pass (gpu-mirror-text-pipeline mirror))
                            (luv:set-bind-group
                             pass 0
                             (ensure-gpu-text-bind-group
                              mirror
                              (gpu-prepared-text-command-atlas command)))
                            (luv:set-vertex-buffer pass 0 text-buffer)
                            (luv:draw
                             pass
                             (gpu-prepared-text-command-vertex-count command)
                             1
                             (gpu-prepared-text-command-first-vertex
                              command)))))))
                   (luv:end-pass pass)
                   (when readback-buffer
                     (luv:encode
                      encoder
                      (luv:make-gpu-copy-texture-to-buffer-command
                       :source surface :destination readback-buffer))))))))))))
  mirror)

(defmethod present-mirror ((mirror luv-gpu-mirror))
  (let ((target (mirror-target mirror))
        (medium (sheet-medium (mirror-sheet mirror))))
    (when (and (eq :open (luv:canvas-state target))
               (or (mirror-embedded-p mirror)
                   (plusp (length (gpu-medium-commands medium)))))
      (if (mirror-embedded-p mirror)
          ;; No drawable is acquired and no pass is encoded. Publish the
          ;; retained snapshot synchronously with the repaint that authored
          ;; it, so command ranges and all six dense buffers are one revision.
          (render-gpu-mirror-frame mirror)
          (luv:request-canvas-frame
           target
           (lambda (timestamp)
             (declare (ignore timestamp))
             (render-gpu-mirror-frame mirror))))))
  mirror)

(defun capture-gpu-mirror-screenshot (mirror pathname)
  "Render direct-GPU MIRROR into its hidden drawable and save a PNG."
  (check-type mirror luv-gpu-mirror)
  (let ((target (mirror-target mirror)))
    (unless (eq :open (luv:canvas-state target))
      (error "Cannot capture a McCLIM mirror whose canvas is ~S."
             (luv:canvas-state target)))
    (ensure-directories-exist pathname)
    (luv:request-canvas-frame
     target
     (lambda (timestamp)
       (declare (ignore timestamp))
       (let* ((context (ensure-gpu-mirror-context mirror :readback-p t))
              (extent (luv:canvas-extent context))
              (width (first extent))
              (height (second extent))
              (buffer
                (luv:create
                 (luv:context-device context)
                 (luv:make-buffer-descriptor
                  :label "direct McCLIM screenshot readback"
                  :size (* 4 width height) :usage '(:copy-dst)))))
         (unwind-protect
              (progn
                (render-gpu-mirror-frame mirror :readback-buffer buffer)
                (let ((pixels (luv:read-buffer buffer))
                      (format (luv:canvas-format context)))
                  (luv:write-rgba-png
                   pathname pixels width height format)
                  (values pathname pixels width height format)))
           (luv:destroy buffer)))))))

(defmethod release-mirror-presentation ((mirror luv-gpu-mirror))
  (release-raster-mirror-compositor (mirror-compositor mirror))
  (setf (mirror-compositor mirror) nil
        (gpu-mirror-prepared-commands mirror) nil)
  (release-gpu-mirror-pipeline mirror)
  (release-gpu-mirror-frame-states mirror)
  (alexandria:when-let ((texture (mirror-texture mirror)))
    (luv:destroy texture)
    (setf (mirror-texture mirror) nil))
  mirror)
