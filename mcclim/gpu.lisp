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

(defstruct gpu-solid-command first-vertex vertex-count)

(defstruct gpu-text-command
  string x y font-pathname size color align-x align-y)

(defstruct gpu-prepared-text-command
  atlas first-vertex vertex-count)

(defclass gpu-mirror-frame-state ()
  ((view :initarg :view :accessor gpu-frame-state-view)
   (vertex-buffer :initarg :vertex-buffer :initform nil
                  :accessor gpu-frame-state-vertex-buffer)
   (vertex-capacity :initarg :vertex-capacity :initform 0
                    :accessor gpu-frame-state-vertex-capacity)
   (text-buffer :initarg :text-buffer :initform nil
                :accessor gpu-frame-state-text-buffer)
   (text-capacity :initarg :text-capacity :initform 0
                  :accessor gpu-frame-state-text-capacity)))

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

(defmethod medium-draw-polygon*
    ((medium luv-gpu-medium) coordinates closed filled)
  (let ((first-vertex (/ (length (gpu-medium-vertices medium)) 6))
        (points (coordinate-pairs coordinates))
        (color (gpu-medium-color medium)))
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
             medium (car (last points)) (first points) thickness color))))
    (let ((vertex-count
            (- (/ (length (gpu-medium-vertices medium)) 6) first-vertex)))
      (when (plusp vertex-count)
        (vector-push-extend
         (make-gpu-solid-command
          :first-vertex first-vertex :vertex-count vertex-count)
         (gpu-medium-commands medium)))))
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

(defun repaint-gpu-mirror (mirror)
  "Rebuild MIRROR's retained triangle stream as one complete McCLIM frame."
  (let* ((sheet (mirror-sheet mirror))
         (sheets (gpu-sheet-paint-order sheet))
         (target-medium (sheet-medium sheet))
         (media
           (remove-duplicates
            (remove nil (mapcar #'sheet-medium sheets)) :test #'eq)))
    (dolist (medium media)
      (setf (fill-pointer (gpu-medium-vertices medium)) 0)
      (setf (fill-pointer (gpu-medium-commands medium)) 0)
      (incf (gpu-medium-buffering-depth medium)))
    (unwind-protect
         (repaint-sheet sheet +everywhere+)
      (dolist (medium media)
        (decf (gpu-medium-buffering-depth medium))))
    ;; Each pane owns a semantic medium, but one mirror owns the ordered GPU
    ;; frame. The top-level stream is its compact presentation buffer.
    (dolist (medium (rest media))
      (let ((vertex-offset
              (/ (length (gpu-medium-vertices target-medium)) 6)))
        (loop for command across (gpu-medium-commands medium)
              do (vector-push-extend
                  (etypecase command
                    (gpu-solid-command
                     (make-gpu-solid-command
                      :first-vertex
                      (+ vertex-offset
                         (gpu-solid-command-first-vertex command))
                      :vertex-count
                      (gpu-solid-command-vertex-count command)))
                    (gpu-text-command command))
                  (gpu-medium-commands target-medium))))
      (loop for value across (gpu-medium-vertices medium)
            do (vector-push-extend
                value (gpu-medium-vertices target-medium))))
    (present-mirror mirror)))

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
    (unless (mirror-embedded-p mirror)
      (luv:show-canvas (mirror-target mirror)))))

(defun gpu-text-font-pathname (text-style)
  (multiple-value-bind (family face size)
      (text-style-components (climb:parse-text-style* text-style))
    (declare (ignore size))
    (cl-dejavu:font-pathname
     (cond
       ((eq family :fix) "DejaVuSansMono.ttf")
       ((eq family :serif)
        (if (member :bold (if (listp face) face (list face)))
            "DejaVuSerif-Bold.ttf"
            "DejaVuSerif.ttf"))
       ((member :bold (if (listp face) face (list face)))
        "DejaVuSans-Bold.ttf")
       (t "DejaVuSans.ttf")))))

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
          :align-x align-x :align-y align-y)
         (gpu-medium-commands medium)))))
  nil)

(defun ensure-gpu-mirror-context (mirror)
  (when (mirror-embedded-p mirror)
    (return-from ensure-gpu-mirror-context (mirror-context mirror)))
  (let* ((target (mirror-target mirror))
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
                       :device device :usage '(:render-attachment)))))))
    (multiple-value-bind (width height) (luv:canvas-size target)
      (unless (equal (list width height) (luv:canvas-extent context))
        (luv:configure-canvas-context
         context
         (luv:make-canvas-configuration
          :device device :format (luv:canvas-format context)
          :usage '(:render-attachment)))))
    context))

(defun release-gpu-mirror-pipeline (mirror)
  (maphash (lambda (atlas group)
             (declare (ignore atlas))
             (luv:destroy group))
           (gpu-mirror-text-bind-groups mirror))
  (clrhash (gpu-mirror-text-bind-groups mirror))
  (alexandria:when-let ((cache (gpu-mirror-slug-cache mirror)))
    (luv.slug:release-slug-glyph-cache cache))
  (dolist (resource
            (list (gpu-mirror-pipeline mirror)
                  (gpu-mirror-text-pipeline mirror)
                  (gpu-mirror-bind-group mirror)
                  (gpu-mirror-uniform-buffer mirror)
                  (gpu-mirror-text-fragment-module mirror)
                  (gpu-mirror-text-vertex-module mirror)
                  (gpu-mirror-text-layout mirror)
                  (gpu-mirror-fragment-module mirror)
                  (gpu-mirror-vertex-module mirror)
                  (gpu-mirror-layout mirror)))
    (when resource (luv:destroy resource)))
  (setf (gpu-mirror-pipeline mirror) nil
        (gpu-mirror-fragment-module mirror) nil
        (gpu-mirror-vertex-module mirror) nil
        (gpu-mirror-layout mirror) nil
        (gpu-mirror-uniform-buffer mirror) nil
        (gpu-mirror-bind-group mirror) nil
        (gpu-mirror-format mirror) nil
        (gpu-mirror-slug-cache mirror) nil
        (gpu-mirror-text-pipeline mirror) nil
        (gpu-mirror-text-fragment-module mirror) nil
        (gpu-mirror-text-vertex-module mirror) nil
        (gpu-mirror-text-layout mirror) nil))

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
  (let ((format (luv:canvas-format context)))
    (unless (and (gpu-mirror-pipeline mirror)
                 (eq format (gpu-mirror-format mirror)))
      (release-gpu-mirror-pipeline mirror)
      (let* ((device (luv:context-device context))
             (vertex
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
              (luv:destroy resource))))
        (ensure-gpu-mirror-text-pipeline mirror device format)))))

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
                    (padding (max 0.035 (/ 2.0 size))))
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
                 :vertex-count (- (/ (length data) 18) first-vertex))))))))))

(defun prepare-gpu-frame-commands (mirror medium)
  (multiple-value-bind (width height)
      (luv:canvas-logical-size (mirror-target mirror))
    (let ((text-data
            (make-array 1024 :element-type 'single-float
                        :adjustable t :fill-pointer 0))
          commands)
      (loop for command across (gpu-medium-commands medium)
            for prepared =
              (etypecase command
                (gpu-solid-command command)
                (gpu-text-command
                 (append-gpu-text-command
                  mirror command text-data width height)))
            when prepared do (push prepared commands))
      (values (nreverse commands) text-data))))

(defmethod present-mirror ((mirror luv-gpu-mirror))
  (let* ((target (mirror-target mirror))
         (medium (sheet-medium (mirror-sheet mirror)))
         (vertices (gpu-medium-vertices medium)))
    (when (and (eq :open (luv:canvas-state target))
               (plusp (length (gpu-medium-commands medium))))
      (luv:request-canvas-frame
       target
       (lambda (timestamp)
         (declare (ignore timestamp))
         (let* ((context (ensure-gpu-mirror-context mirror))
                (device (luv:context-device context))
                (byte-count (* 4 (length vertices))))
           (ensure-gpu-mirror-pipeline mirror context)
           (multiple-value-bind (commands text-data)
               (prepare-gpu-frame-commands mirror medium)
             (luv:present-canvas-frame
              context
              (lambda (surface encoder)
                (let* ((state
                         (ensure-gpu-mirror-frame-state
                          mirror context surface))
                       (buffer
                         (and (plusp byte-count)
                              (ensure-gpu-frame-vertex-buffer
                               state device byte-count)))
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
                  (when text-buffer (luv:write-buffer text-buffer text-data))
                  (dolist (command commands)
                    (etypecase command
                      (gpu-solid-command
                       (luv:set-pipeline pass (gpu-mirror-pipeline mirror))
                       (luv:set-bind-group
                        pass 0 (gpu-mirror-bind-group mirror))
                       (luv:set-vertex-buffer pass 0 buffer)
                       (luv:draw
                        pass (gpu-solid-command-vertex-count command) 1
                        (gpu-solid-command-first-vertex command)))
                      (gpu-prepared-text-command
                       (luv:set-pipeline
                        pass (gpu-mirror-text-pipeline mirror))
                       (luv:set-bind-group
                        pass 0
                        (ensure-gpu-text-bind-group
                         mirror (gpu-prepared-text-command-atlas command)))
                       (luv:set-vertex-buffer pass 0 text-buffer)
                       (luv:draw
                        pass
                        (gpu-prepared-text-command-vertex-count command) 1
                        (gpu-prepared-text-command-first-vertex command)))))
                  (luv:end-pass pass))))))))))
  mirror)

(defmethod release-mirror-presentation ((mirror luv-gpu-mirror))
  (release-gpu-mirror-pipeline mirror)
  (maphash
   (lambda (key state)
     (declare (ignore key))
     (alexandria:when-let ((buffer (gpu-frame-state-vertex-buffer state)))
       (luv:destroy buffer))
     (alexandria:when-let ((buffer (gpu-frame-state-text-buffer state)))
       (luv:destroy buffer))
     (luv:destroy (gpu-frame-state-view state)))
   (gpu-mirror-frame-states mirror))
  (clrhash (gpu-mirror-frame-states mirror))
  mirror)
