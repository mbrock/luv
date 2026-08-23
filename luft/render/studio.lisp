(in-package #:luft.render)

(defvar *viewer* nil)

(defparameter *inspection-ink-p* t
  "Whether a ray hit gets a blueprint reticle and local triangle-edge lens.")

(defparameter *inspection-reach* 200.0
  "How far, in cells, the atelier's pointer ray may inspect.")

(defparameter *projection* :isometric
  "Either :PERSPECTIVE or :ISOMETRIC.

An isometric picture has no vanishing point, so two chamfers the same width
are the same width on screen wherever they sit.  That is what makes it the
projection to judge a shape rule in.")

(defparameter *isometric-height* 18.0
  "How many world units of height an isometric frame spans.")

(defclass fly-camera ()
  ((position :initarg :position :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform 0.0 :accessor camera-pitch)
   (field-of-view :initarg :field-of-view :initform (* 70.0 (/ pi 180))
                  :accessor camera-field-of-view)))

(defclass site-inspection ()
  ((source :initarg :source :reader site-inspection-source)
   (site :initarg :site :reader site-inspection-site)
   (cell :initarg :cell :reader site-inspection-cell)
   (point :initarg :point :reader site-inspection-point)
   (distance :initarg :distance :reader site-inspection-distance)
   (star-mask :initarg :star-mask :reader site-inspection-star-mask)
   (stock :initarg :stock :reader site-inspection-stock))
  (:documentation
   "One retained semantic ray hit, suitable for sparse inspection.

LUFT sites remain packed integers in dense products.  This object exists only
at the atelier boundary where a person has selected one site."))

(defmethod print-object ((inspection site-inspection) stream)
  (print-unreadable-object (inspection stream :type t)
    (let ((site (site-inspection-site inspection)))
      (format stream "(~D ~D ~D) extent ~3,'0B ~:[-~;+~] at ~,2F"
              (luft:site-x site) (luft:site-y site) (luft:site-z site)
              (luft:site-extent site) (luft:site-positive-p site)
              (site-inspection-distance inspection)))))

(defun make-fly-camera
    (&key (position (vec3:make-vec3 28.0 -2.0 13.0))
          (yaw 2.3562) (pitch -0.393)
          (field-of-view 0.9599311))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun reset-viewer-camera (&optional (viewer *viewer*))
  "Return VIEWER to the connected miter-study view."
  (when viewer
    (let ((camera (viewer-camera viewer)))
      (setf (camera-position camera) (vec3:make-vec3 28.0 -2.0 13.0)
            (camera-yaw camera) 2.3562
            (camera-pitch camera) -0.393
            (camera-field-of-view camera) 0.9599311
            *projection* :isometric
            *isometric-height* 18.0)
      (when (viewer-renderer viewer)
        (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))))
  viewer)

(defun camera-basis (camera)
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defun add-scaled-directions (origin &rest direction-scales)
  (let ((x (vec3:vec3-x origin))
        (y (vec3:vec3-y origin))
        (z (vec3:vec3-z origin)))
    (loop for (direction scale) on direction-scales by #'cddr
          do (incf x (* (vec3:vec3-x direction) scale))
             (incf y (* (vec3:vec3-y direction) scale))
             (incf z (* (vec3:vec3-z direction) scale)))
    (vec3:make-vec3 x y z)))

(defgeneric inspection-source-solid (source)
  (:documentation "Return SOURCE's packed solid chain for pointer queries."))

(defmethod inspection-source-solid ((source t)) source)

(defmethod inspection-source-solid ((source scene)) (scene-solid source))

(defgeneric inspection-face-stock (source face)
  (:documentation "Return the atelier stock at FACE in SOURCE."))

(defmethod inspection-face-stock ((source t) face)
  (default-face-stock face))

(defmethod inspection-face-stock ((source scene) face)
  (scene-face-stock source face))

(defun ray-axis-crossings (position direction)
  "Return grid STEP, first crossing distance, and crossing interval."
  (cond ((plusp direction)
         (values 1 (/ (- (1+ (floor position)) position) direction)
                 (/ direction)))
        ((minusp direction)
         (values -1 (/ (- position (floor position)) (- direction))
                 (/ (- direction))))
        (t (values 0 most-positive-single-float most-positive-single-float))))

(defun ray-entry-face (solid cell-x cell-y cell-z axis step)
  "Return the outward face through which a ray entered CELL along AXIS."
  (let* ((domain (luft:chain-domain solid))
         (anchor-x (+ cell-x (if (and (eq axis :x) (minusp step)) 1 0)))
         (anchor-y (+ cell-y (if (and (eq axis :y) (minusp step)) 1 0)))
         (anchor-z (+ cell-z (if (and (eq axis :z) (minusp step)) 1 0)))
         (extent (ecase axis
                   (:x luft:+yz-face-extent+)
                   (:y luft:+xz-face-extent+)
                   (:z luft:+xy-face-extent+)))
         (geometry (luft:make-site domain anchor-x anchor-y anchor-z extent 1))
         (occupancy (lambda (x y z)
                      (luft:chain-cell-occupancy-bit solid x y z))))
    (luft:orient-face-outward domain geometry occupancy)))

(defun make-site-inspection (source face cell-x cell-y cell-z point distance)
  (let* ((solid (inspection-source-solid source))
         (domain (luft:chain-domain solid))
         (occupancy (lambda (x y z)
                      (luft:chain-cell-occupancy-bit solid x y z)))
         (cell (luft:make-site domain cell-x cell-y cell-z
                               luft:+cell-extent+ 1)))
    (make-instance
     'site-inspection :source source :site face :cell cell :point point
     :distance distance
     :star-mask
     (luft:site-star-occupancy-mask
      domain
      (luft:make-site domain cell-x cell-y cell-z luft:+vertex-extent+ 1)
      occupancy)
     :stock (inspection-face-stock source face))))

(defun raycast-site (source origin direction
                     &key (max-distance *inspection-reach*))
  "Return the first outward LUFT face met by a continuous lattice ray.

Tied edge and corner crossings advance together, so a ray never reports a
cell it merely touches.  The returned SITE-INSPECTION is the one sparse object
boundary over the packed chain and dense face records."
  (let* ((solid (inspection-source-solid source))
         (direction (vec3:vec3-normalize direction))
         (x (floor (vec3:vec3-x origin)))
         (y (floor (vec3:vec3-y origin)))
         (z (floor (vec3:vec3-z origin)))
         step-x step-y step-z
         next-x next-y next-z
         delta-x delta-y delta-z
         (distance 0.0)
         (entry-axis nil)
         (entry-step 0))
    (multiple-value-setq (step-x next-x delta-x)
      (ray-axis-crossings (vec3:vec3-x origin) (vec3:vec3-x direction)))
    (multiple-value-setq (step-y next-y delta-y)
      (ray-axis-crossings (vec3:vec3-y origin) (vec3:vec3-y direction)))
    (multiple-value-setq (step-z next-z delta-z)
      (ray-axis-crossings (vec3:vec3-z origin) (vec3:vec3-z direction)))
    (loop
      (when (= 1 (luft:chain-cell-occupancy-bit solid x y z))
        (when entry-axis
          (let* ((face (ray-entry-face solid x y z entry-axis entry-step))
                 (point (add-scaled-directions origin direction distance)))
            (when face
              (return
                (make-site-inspection source face x y z point distance))))))
      (let ((next (min next-x next-y next-z)))
        (when (> next max-distance) (return nil))
        (setf distance next
              entry-axis nil)
        ;; Remember one carrier face for tied crossings, choosing the axis
        ;; most aligned with the ray.  All tied cells still advance together.
        (flet ((remember (axis step component)
                 (when (or (null entry-axis)
                           (> (abs component)
                              (abs (vec3:vec3-component direction entry-axis))))
                   (setf entry-axis axis entry-step step))))
          (when (<= next-x (+ next 1.0e-6))
            (incf x step-x)
            (incf next-x delta-x)
            (remember :x step-x (vec3:vec3-x direction)))
          (when (<= next-y (+ next 1.0e-6))
            (incf y step-y)
            (incf next-y delta-y)
            (remember :y step-y (vec3:vec3-y direction)))
          (when (<= next-z (+ next 1.0e-6))
            (incf z step-z)
            (incf next-z delta-z)
            (remember :z step-z (vec3:vec3-z direction))))))))

(defun projection-lane (width height field-of-view near far)
  "The four projection coefficients and the homogeneous-divisor selector.

Both projections use the same three rows: clip X and Y are the view
coordinates scaled, and clip Z is an affine function of view depth.  The
perspective divisor is the view depth and the isometric divisor is one, so
the selector is the whole of the difference."
  (let ((aspect (/ (coerce width 'single-float) height)))
    (ecase *projection*
      (:perspective
       (let ((focal (/ (tan (/ field-of-view 2.0)))))
         (values (/ focal aspect) focal
                 (/ far (- far near))
                 (/ (- (* far near)) (- far near))
                 1.0)))
      (:isometric
       (let ((half (/ *isometric-height* 2.0)))
         (values (/ 1.0 (* half aspect)) (/ 1.0 half)
                 (/ 1.0 (- far near))
                 (/ (- near) (- far near))
                 0.0))))))

(defstruct (frame-view (:constructor %make-frame-view))
  "One immutable camera sample shared by geometry and temporal motion."
  position right up forward projection divisor jitter)

(defun halton (index base)
  (loop with fraction = (/ 1.0 base)
        with value = 0.0
        while (plusp index)
        do (incf value (* fraction (mod index base)))
           (setf index (floor index base)
                 fraction (/ fraction base))
        finally (return (coerce value 'single-float))))

(defun temporal-jitter (frame-index width height)
  "Sample the eight-position Halton(2,3) sequence in clip coordinates."
  (let ((sample (1+ (mod frame-index 8))))
    (vector (coerce (/ (* 2.0 (- (halton sample 2) 0.5))
                       (max width 1))
                    'single-float)
            (coerce (/ (* 2.0 (- (halton sample 3) 0.5))
                       (max height 1))
                    'single-float))))

(defun capture-frame-view (camera width height jitter)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let ((near 0.1)
          (far 200.0))
      (multiple-value-bind (px py pz pw divisor)
          (projection-lane width height (camera-field-of-view camera)
                           near far)
        (%make-frame-view
         :position (let ((position (camera-position camera)))
                     (vec3:make-vec3 (vec3:vec3-x position)
                                     (vec3:vec3-y position)
                                     (vec3:vec3-z position)))
         :right right :up up :forward forward
         :projection (vector px py pz pw)
         :divisor divisor :jitter jitter)))))

(defun camera-uniform-data (view previous inspection-parameters ink-strength)
  (flet ((lane (vector fourth)
           (list (vec3:vec3-x vector) (vec3:vec3-y vector)
                 (vec3:vec3-z vector) fourth)))
    (make-array
     52 :element-type 'single-float
     :initial-contents
     (mapcar
      (lambda (value) (coerce value 'single-float))
      (append (lane (frame-view-position view) 0.0)
              (lane (frame-view-right view) 0.0)
              (lane (frame-view-up view) 0.0)
              (lane (frame-view-forward view) 0.0)
              (coerce (frame-view-projection view) 'list)
              (list (/ luft:+mesh-bevel-width+ luft:+mesh-cell-size+)
                    *wireframe*
                    (frame-view-divisor view) ink-strength)
              (lane (frame-view-position previous) 0.0)
              (lane (frame-view-right previous) 0.0)
              (lane (frame-view-up previous) 0.0)
              (lane (frame-view-forward previous) 0.0)
              (coerce (frame-view-projection previous) 'list)
              (list (aref (frame-view-jitter view) 0)
                    (aref (frame-view-jitter view) 1)
                    (frame-view-divisor previous) 0.0)
              (coerce inspection-parameters 'list))))))

(defun viewer-logical-extent (viewer)
  (let ((canvas (viewer-canvas viewer)))
    (list (canvas-width canvas) (canvas-height canvas))))

(defun viewer-pointer-position (viewer)
  (let ((extent (viewer-logical-extent viewer)))
    (if (viewer-pointer-captured-p viewer)
        (values (/ (first extent) 2.0) (/ (second extent) 2.0))
        (values (or (viewer-pointer-x viewer) (/ (first extent) 2.0))
                (or (viewer-pointer-y viewer) (/ (second extent) 2.0))))))

(defun viewer-pointer-ray (viewer)
  "Return the world-space origin and direction through VIEWER's pointer."
  (let* ((extent (viewer-logical-extent viewer))
         (camera (viewer-camera viewer))
         (width (first extent))
         (height (second extent))
         (view (capture-frame-view camera width height #(0.0 0.0)))
         (projection (frame-view-projection view)))
    (multiple-value-bind (pointer-x pointer-y)
        (viewer-pointer-position viewer)
      (let* ((ndc-x (- (* 2.0 (/ pointer-x width)) 1.0))
             (ndc-y (- 1.0 (* 2.0 (/ pointer-y height))))
             (right-scale (/ ndc-x (aref projection 0)))
             (up-scale (/ (- ndc-y) (aref projection 1))))
        (if (eq *projection* :perspective)
            (values
             (camera-position camera)
             (vec3:vec3-normalize
              (add-scaled-directions
               (frame-view-forward view)
               (frame-view-right view) right-scale
               (frame-view-up view) up-scale)))
            (values
             (add-scaled-directions
              (camera-position camera)
              (frame-view-right view) right-scale
              (frame-view-up view) up-scale)
             (frame-view-forward view)))))))

(defun same-inspected-site-p (left right)
  (or (eq left right)
      (and left right
           (= (site-inspection-site left) (site-inspection-site right)))))

(defun update-viewer-inspection (viewer)
  (multiple-value-bind (origin direction) (viewer-pointer-ray viewer)
    (let* ((inspection (raycast-site (viewer-source viewer) origin direction))
           (changed-p
             (not (same-inspected-site-p
                   inspection (viewer-inspection viewer)))))
      ;; Retain the exact current distance and point even while the semantic
      ;; site remains the same; repaint McCLIM only at semantic boundaries.
      (setf (viewer-inspection viewer) inspection)
      (when changed-p (refresh-viewer-inspector viewer))
      inspection)))

(defun viewer-inspection-parameters (viewer extent)
  (let ((logical-extent (viewer-logical-extent viewer)))
    (multiple-value-bind (pointer-x pointer-y)
        (viewer-pointer-position viewer)
      (vector (coerce (/ pointer-x (first logical-extent)) 'single-float)
              (coerce (/ pointer-y (second logical-extent)) 'single-float)
              (coerce (/ (first extent)) 'single-float)
              (coerce (/ (second extent)) 'single-float)))))

(declaim (ftype function viewer-surface-view))

(defun encode-viewer-frame
    (viewer encoder surface-texture extent &key (inspector-p t))
  (let* ((renderer (viewer-renderer viewer))
         (surface-view (viewer-surface-view viewer surface-texture))
         (width (first extent))
         (height (second extent))
         (jitter (if (renderer-temporal-p renderer)
                     (temporal-jitter (renderer-frame-index renderer)
                                      width height)
                     #(0.0 0.0)))
         (view (capture-frame-view (viewer-camera viewer)
                                   width height jitter))
         (previous (or (renderer-previous-view renderer) view))
         (inspection (and inspector-p (update-viewer-inspection viewer))))
    (encode-renderer-frame
     renderer encoder surface-view extent
     (camera-uniform-data
      view previous (viewer-inspection-parameters viewer extent)
     (if (and inspection *inspection-ink-p*) 1.0 0.0))
     :jitter jitter :view view
     :construction-p (plusp *wireframe*)
     :inspector-texture (and inspector-p (viewer-inspector-texture viewer))
     :inspector-rect (and inspector-p
                          (viewer-inspector-rect viewer extent)))))

(clim:define-command-table luft-window)
(clim:define-command-table luft-window-release)
(clim:define-command-table luft-atelier)
(clim:define-command-table luft-atelier-release)

(defconstant +site-inspector-width+ 372)
(defconstant +site-inspector-height+ 306)

(defclass site-inspector-pane (clim:application-pane) ())

(clim:define-presentation-type luft-site ())

(defun site-extent-label (site)
  (with-output-to-string (stream)
    (dolist (axis '(:x :y :z))
      (when (luft:site-extends-p site axis)
        (write-char (char (symbol-name axis) 0) stream)))))

(defun inspector-row (stream y label value &key presentation)
  (clim:draw-text* stream label 18 y :align-y :center :text-size 12
                   :ink (clim:make-rgb-color 0.48 0.52 0.55))
  (flet ((draw ()
           (clim:draw-text* stream value 148 y :align-y :center :text-size 12
                            :ink (clim:make-rgb-color 0.86 0.88 0.84))))
    (if presentation
        (clim:with-output-as-presentation (stream presentation 'luft-site)
          (draw))
        (draw))))

(defun display-site-inspector (viewer stream)
  "Draw VIEWER's current sparse ray hit as McCLIM presentations."
  (clim:draw-rectangle* stream 0 0 +site-inspector-width+
                        +site-inspector-height+
                        :ink (clim:make-rgb-color 0.025 0.070 0.090))
  (clim:draw-text* stream
                   (if (plusp *wireframe*)
                       "LUFT SITE  ·  C CONSTRUCTION ON"
                       "LUFT SITE  ·  C CONSTRUCTION OFF")
                   18 25 :align-y :center :text-size 14
                   :text-face :bold
                   :ink (clim:make-rgb-color 0.42 0.91 0.94))
  (let ((inspection (viewer-inspection viewer)))
    (if (null inspection)
        (progn
          (clim:draw-text* stream "point at the boundary" 18 60
                           :align-y :center :text-size 13
                           :ink (clim:make-rgb-color 0.58 0.62 0.62))
          (clim:draw-text* stream "escape releases the pointer" 18 84
                           :align-y :center :text-size 11
                           :ink (clim:make-rgb-color 0.40 0.44 0.45)))
        (let* ((site (site-inspection-site inspection))
               (cell (site-inspection-cell inspection))
               (point (site-inspection-point inspection))
               (star-mask (site-inspection-star-mask inspection)))
          (inspector-row
           stream 57 "site"
           (format nil "~D, ~D, ~D" (luft:site-x site)
                   (luft:site-y site) (luft:site-z site))
           :presentation site)
          (inspector-row stream 80 "extent"
                         (format nil "~A · dimension ~D"
                                 (site-extent-label site)
                                 (luft:site-dimension site)))
          (inspector-row stream 103 "orientation"
                         (if (luft:site-positive-p site) "+" "−"))
          (inspector-row
           stream 126 "solid cell"
           (format nil "~D, ~D, ~D" (luft:site-x cell)
                   (luft:site-y cell) (luft:site-z cell))
           :presentation cell)
          (inspector-row stream 149 "distance"
                         (format nil "~,3F cells"
                                 (site-inspection-distance inspection)))
          (inspector-row stream 172 "world hit"
                         (format nil "~,2F  ~,2F  ~,2F"
                                 (vec3:vec3-x point) (vec3:vec3-y point)
                                 (vec3:vec3-z point)))
          (inspector-row stream 195 "stock"
                         (format nil "~D" (site-inspection-stock inspection)))
          (inspector-row stream 218 "cell-corner star"
                         (format nil "#x~2,'0X" star-mask))
          (inspector-row stream 241 "star topology"
                         (if (luft:star-singular-p star-mask)
                             "singular · split sheets"
                             "regular"))
          (inspector-row
           stream 264 "spike junction"
           (handler-case
               (format nil "~D regular sheet~:P"
                       (length (luft:decompose-star-mask star-mask)))
             (error () "covered junction · next spike")))))))

(defun refresh-viewer-inspector (viewer)
  (let ((pane (ignore-errors (clim:find-pane-named viewer 'inspector))))
    (when pane
      (clim:redisplay-frame-pane viewer pane :force-p t)
      ;; Application panes may follow their output cursor even without visible
      ;; scroll bars; this inspector is a fixed HUD, so pin its origin.
      (clim:scroll-extent pane 0 0)
      (let ((mirror (clim:sheet-direct-mirror
                     (clim:frame-top-level-sheet viewer))))
        (setf (viewer-inspector-mirror viewer) mirror)
        (mcluv:present-mirror mirror))))
  viewer)

(defun viewer-inspector-texture (viewer)
  (let ((mirror (viewer-inspector-mirror viewer)))
    (and mirror (mcluv:mirror-texture mirror))))

(defun viewer-inspector-rect (viewer extent)
  (declare (ignore extent))
  (when (viewer-inspector-texture viewer)
    (let* ((logical-extent (viewer-logical-extent viewer))
           (width (first logical-extent))
           (height (second logical-extent))
           (margin 14.0)
           (left (- 1.0 (* 2.0 (/ (+ margin +site-inspector-width+) width))))
           (right (- 1.0 (* 2.0 (/ margin width))))
           (top (- 1.0 (* 2.0 (/ margin height))))
           (bottom (- 1.0
                      (* 2.0 (/ (+ margin +site-inspector-height+) height)))))
      (make-array 4 :element-type 'single-float
                    :initial-contents (mapcar (lambda (value)
                                                (coerce value 'single-float))
                                              (list left top right bottom))))))

(clim:define-application-frame viewer
    (clim:standard-application-frame canvas-event-handler)
  ((canvas :initarg :canvas :initform nil :reader viewer-canvas)
   (context :initarg :context :initform nil :reader viewer-context)
   (device :initarg :device :initform nil :reader viewer-device)
   (source :initarg :source :initform (make-miter-study-scene)
           :accessor viewer-source)
   (renderer :initarg :renderer :initform nil :accessor viewer-renderer)
   (camera :initarg :camera :initform (make-fly-camera) :reader viewer-camera)
   (surface-views :initform (make-hash-table :test #'eql)
                  :reader viewer-surface-views)
   (controls :initform (make-hash-table :test #'eq)
             :reader viewer-controls)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (pointer-x :initform nil :accessor viewer-pointer-x)
   (pointer-y :initform nil :accessor viewer-pointer-y)
   (inspection :initform nil :accessor viewer-inspection)
   (inspector-mirror :initform nil :accessor viewer-inspector-mirror)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 4.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)
   (running-p :initform t :accessor viewer-running-p)
   (quit-requested-p :initform nil :accessor viewer-quit-requested-p)
   (stop-state :initform :running :accessor viewer-stop-state)
   (stop-lock :initform (sb-thread:make-mutex :name "LUFT viewer stop")
              :reader viewer-stop-lock)
   (stop-ready :initform (sb-thread:make-waitqueue
                          :name "LUFT viewer stopped")
               :reader viewer-stop-ready))
  ;; The frame is the application and the inspector is its first pane.  Its
  ;; command table inherits every input phase so McCLIM considers each command
  ;; executable; event dispatch still chooses one phase explicitly.
  (:command-table (luft-viewer
                   :inherit-from (luft-window luft-window-release
                                  luft-atelier luft-atelier-release)
                   :inherit-menu t))
  (:panes
   (inspector :application
              :display-function 'display-site-inspector
              :scroll-bars nil
              :default-text-style (clim:make-text-style :sans-serif nil
                                                        :normal)))
  (:layouts
   (default
    (clim:horizontally (:width +site-inspector-width+
                        :height +site-inspector-height+)
      inspector)))
  (:menu-bar nil))

(defun viewer-surface-view (viewer surface)
  "Return VIEWER's texture view for the current presentation slot."
  (let* ((context (viewer-context viewer))
         (key (canvas-frame-resource-key context surface))
         (views (viewer-surface-views viewer))
         (view (gethash key views)))
    (when (and view (not (eq surface (gpu-texture-view-texture view))))
      ;; Metal returns a fresh borrowed wrapper when it revisits a drawable.
      ;; Keep the stable slot but never let its view retain the old wrapper.
      (destroy view)
      (setf view nil))
    (or view
        (setf (gethash key views)
              (create (viewer-device viewer)
                      (make-texture-view-descriptor :texture surface))))))

(defun release-viewer-surface-views (viewer)
  (maphash (lambda (key view)
             (declare (ignore key))
             (destroy view))
           (viewer-surface-views viewer))
  (clrhash (viewer-surface-views viewer))
  (values))

(defun viewer-control-active-p (viewer direction)
  (gethash direction (viewer-controls viewer)))

(defun set-viewer-control (viewer direction active-p)
  (if active-p
      (setf (gethash direction (viewer-controls viewer)) t)
      (remhash direction (viewer-controls viewer)))
  viewer)

(defun clear-viewer-controls (viewer)
  (clrhash (viewer-controls viewer))
  viewer)

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (viewer-camera viewer))
         (step (* dt (viewer-speed viewer))))
    (setf (viewer-last-timestamp viewer) timestamp)
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore up))
      (flet ((move (direction amount)
               (let ((position (camera-position camera)))
                 (setf (camera-position camera)
                       (vec3:make-vec3
                        (+ (vec3:vec3-x position)
                           (* amount (vec3:vec3-x direction)))
                        (+ (vec3:vec3-y position)
                           (* amount (vec3:vec3-y direction)))
                        (+ (vec3:vec3-z position)
                           (* amount (vec3:vec3-z direction))))))))
        ;; A dolly along the viewing ray is invisible in an orthographic
        ;; projection, so W/S change its scale instead.  Perspective retains
        ;; the ordinary fly-camera dolly.
        (if (eq *projection* :isometric)
            (let ((zoom-rate (* dt (viewer-speed viewer) 0.12)))
              (when (viewer-control-active-p viewer :forward)
                (setf *isometric-height*
                      (* *isometric-height* (exp (- zoom-rate)))))
              (when (viewer-control-active-p viewer :backward)
                (setf *isometric-height*
                      (* *isometric-height* (exp zoom-rate)))))
            (progn
              (when (viewer-control-active-p viewer :forward)
                (move forward step))
              (when (viewer-control-active-p viewer :backward)
                (move forward (- step)))))
        (when (viewer-control-active-p viewer :right) (move right step))
        (when (viewer-control-active-p viewer :left) (move right (- step)))
        (when (viewer-control-active-p viewer :up)
          (move (vec3:make-vec3 0 0 1) step))
        (when (viewer-control-active-p viewer :down)
          (move (vec3:make-vec3 0 0 1) (- step)))))))

(defun render-viewer-frame (viewer timestamp)
  (declare (ignore timestamp))
  (when (viewer-running-p viewer)
    (present-canvas-frame
     (viewer-context viewer)
     (lambda (surface-texture encoder presentation-time)
       (advance-viewer-camera viewer presentation-time)
       (let ((extent (canvas-extent (viewer-context viewer))))
         (encode-viewer-frame viewer encoder surface-texture extent))))))

(defun viewer-command-viewer ()
  "Return the LUFT application receiving the current McCLIM command."
  clim:*application-frame*)

(defun request-viewer-quit (viewer)
  "Begin an orderly stop once and return true when this call began it."
  (let ((begin-p nil))
    (sb-thread:with-mutex ((viewer-stop-lock viewer))
      (unless (viewer-quit-requested-p viewer)
        (setf (viewer-quit-requested-p viewer) t
              (viewer-running-p viewer) nil
              begin-p t)))
    (when begin-p
      ;; A native close and a command both run on the canvas thread. Teardown
      ;; establishes a canvas-thread barrier, so it must run beside that thread
      ;; and let CLOSE-CANVAS end the loop after application resources are gone.
      (sb-thread:make-thread
       (lambda () (stop-viewer viewer))
       :name "LUFT viewer quit"))
    begin-p))

(clim:define-command (com-start-moving :command-table luft-atelier
                                       :name "Start Moving")
    ((direction 'keyword :prompt "direction"))
  (set-viewer-control (viewer-command-viewer) direction t))

(clim:define-command (com-stop-moving :command-table luft-atelier-release
                                      :name "Stop Moving")
    ((direction 'keyword :prompt "direction"))
  (set-viewer-control (viewer-command-viewer) direction nil))

(clim:define-command (com-reset-view :command-table luft-atelier
                                     :name "Reset View"
                                     :keystroke (:r))
    ()
  (reset-viewer-camera (viewer-command-viewer)))

(clim:define-command (com-toggle-construction-lines
                      :command-table luft-atelier
                      :name "Toggle Construction Lines"
                      :keystroke (:c))
    ()
  (let ((viewer (viewer-command-viewer)))
    (setf *wireframe* (if (plusp *wireframe*) 0.0 1.0))
    (when (viewer-renderer viewer)
      (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))
    (refresh-viewer-inspector viewer)))

(clim:define-command (com-release-pointer :command-table luft-window
                                          :name "Release Pointer"
                                          :keystroke (:escape))
    ()
  (let* ((viewer (viewer-command-viewer))
         (canvas (viewer-canvas viewer)))
    (when (viewer-pointer-captured-p viewer)
      (set-canvas-relative-pointer-mode canvas nil)
      (setf (viewer-pointer-captured-p viewer) nil))))

(clim:define-command (com-toggle-fullscreen :command-table luft-window
                                            :name "Toggle Fullscreen"
                                            :keystroke (:f11))
    ()
  (let ((canvas (viewer-canvas (viewer-command-viewer))))
    (set-canvas-fullscreen canvas (not (canvas-fullscreen-p canvas)))))

(clim:define-command (com-quit :command-table luft-window
                               :name "Quit"
                               :keystroke (#\q :control))
    ()
  (request-viewer-quit (viewer-command-viewer)))

(defparameter +viewer-movement-keys+
  '((:w :forward) (:up :forward)
    (:s :backward) (:down :backward)
    (:a :left) (:left :left)
    (:d :right) (:right :right)
    (:space :up)
    (:shift-left :down) (:shift-right :down))
  "Physical keys whose press and release urge the inspection camera.")

(defun install-viewer-movement-commands ()
  "Install the atelier's held controls into their press and release tables."
  (dolist (binding +viewer-movement-keys+)
    (destructuring-bind (key direction) binding
      (let ((gesture (list key :any)))
        ;; Definition reloads replace the live vocabulary instead of stacking
        ;; another item at the same gesture coordinate.
        (dolist (table '(luft-atelier luft-atelier-release))
          (clim:remove-keystroke-from-command-table
           table gesture :errorp nil))
        (flet ((command-item (command)
                 (let ((direction direction))
                   (lambda (gesture numeric-argument)
                     (declare (ignore gesture numeric-argument))
                     (list command direction)))))
          (clim:add-keystroke-to-command-table
           'luft-atelier gesture :function
           (command-item 'com-start-moving) :errorp nil)
          (clim:add-keystroke-to-command-table
           'luft-atelier-release gesture :function
           (command-item 'com-stop-moving) :errorp nil)))))
  (values))

(install-viewer-movement-commands)

(defgeneric viewer-key-event-tables (event)
  (:documentation "Return the window and atelier tables for key EVENT."))

(defmethod viewer-key-event-tables ((event canvas-key-press-event))
  (values 'luft-window 'luft-atelier))

(defmethod viewer-key-event-tables ((event canvas-key-release-event))
  (values 'luft-window-release 'luft-atelier-release))

(defun viewer-key-command (viewer event)
  "Return the named McCLIM command VIEWER binds to key EVENT, or NIL."
  (multiple-value-bind (window atelier) (viewer-key-event-tables event)
    (or (mcluv:canvas-key-event-command
         viewer event :command-table window)
        (mcluv:canvas-key-event-command
         viewer event :command-table atelier))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (request-viewer-quit viewer)
  :defer-canvas-close)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (declare (ignore canvas))
  (unless (canvas-key-event-repeat-p event)
    (let ((command (viewer-key-command viewer event)))
      (when command
        (clim:execute-frame-command viewer command))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (let ((command (viewer-key-command viewer event)))
    (when command
      (clim:execute-frame-command viewer command)))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-button-press-event))
  (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
        (viewer-pointer-y viewer) (canvas-pointer-event-y event))
  (when (and (eq :left (canvas-pointer-event-button event))
             (not (viewer-pointer-captured-p viewer)))
    (set-canvas-relative-pointer-mode canvas t)
    (setf (viewer-pointer-captured-p viewer) t))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-wheel-event))
  (declare (ignore canvas))
  (let ((factor (expt 1.10 (- (canvas-pointer-event-scroll-y event)))))
    (if (eq *projection* :isometric)
        (setf *isometric-height* (* *isometric-height* factor))
        (let ((camera (viewer-camera viewer)))
          (setf (camera-field-of-view camera)
                (max 0.43633232
                     (min 1.7453293
                          (* (camera-field-of-view camera) factor)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
        (viewer-pointer-y viewer) (canvas-pointer-event-y event))
  (when (viewer-pointer-captured-p viewer)
    (let ((camera (viewer-camera viewer))
          (sensitivity (viewer-sensitivity viewer)))
      (decf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event) sensitivity))
      (setf (camera-pitch camera)
            (max -1.5 (min 1.5
                           (- (camera-pitch camera)
                              (* (canvas-pointer-event-delta-y event)
                                 sensitivity)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-focus-lost-event))
  (declare (ignore event))
  (clear-viewer-controls viewer)
  (when (viewer-pointer-captured-p viewer)
    (set-canvas-relative-pointer-mode canvas nil)
    (setf (viewer-pointer-captured-p viewer) nil))
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun start-viewer (&key
                       (solid (make-miter-study-scene))
                       (camera (make-fly-camera))
                       (title "LUFT miter-study spike")
                       (width 1100) (height 800)
                       fullscreen-p
                       (frames-per-second 60)
                       (provider *gpu-provider*))
  "Open the indexed-instanced LUFT renderer as a McCLIM atelier."
  (let ((canvas
          (make-sdl-canvas
           :title title :width width :height height :visible-p nil
           :fullscreen-p fullscreen-p
           :high-pixel-density-p t
           :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (let* ((device*
                  (setf device
                        (request-gpu-device
                         provider (make-device-descriptor :label title))))
                (context
                  (make-canvas-context
                   canvas provider
                   ;; :copy-src drops the Metal framebuffer-only contract so
                   ;; CAPTURE-VIEWER-FRAME can read the drawable back.
                   (make-canvas-configuration
                    :device device*
                    :usage '(:render-attachment :copy-src))))
                (renderer*
                  (setf renderer
                        (make-renderer
                         device* (make-render-mesh solid)
                         (canvas-format context) (canvas-extent context))))
                (port (clim:find-port :server-path '(:luv-raster)))
                (manager
                  (or (first (clim-internals::frame-managers port))
                      (make-instance 'mcluv:luv-frame-manager :port port)))
                (viewer
                  (let ((mcluv:*embedded-mirror-target* canvas)
                        (mcluv:*embedded-mirror-context* context)
                        (mcluv:*embedded-mirror-device* device*))
                    (clim:make-application-frame
                     'viewer :frame-manager manager :enable t
                             :canvas canvas :context context
                             :device device* :renderer renderer*
                             :camera camera :source solid))))
           (setf (canvas-event-handler canvas) viewer)
           (refresh-viewer-inspector viewer)
           (request-canvas-frame
            canvas (lambda (timestamp) (render-viewer-frame viewer timestamp)))
           (show-canvas canvas)
           (setf (canvas-clock canvas)
                 (make-cadence-clock
                  (lambda (native-canvas timestamp)
                    (declare (ignore native-canvas))
                    (render-viewer-frame viewer timestamp))
                  :frames-per-second frames-per-second))
           (setf *viewer* viewer
                 completed-p t)
           viewer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (when (eq :open (canvas-state canvas)) (close-canvas canvas))
        (when device (ignore-errors (destroy device)))))))

(defun capture-viewer-frame
    (pathname &optional (viewer *viewer*) &key (inspector-p t))
  "Render one VIEWER frame on its canvas thread and write it to PATHNAME.

INSPECTOR-P is true by default.  A source-defined evidence capture may make it
false when the subject is the geometry rather than the atelier UI."
  (let* ((context (viewer-context viewer))
         (extent (canvas-extent context))
         (pathname (merge-pathnames pathname))
         (buffer
           (create (viewer-device viewer)
                   (make-buffer-descriptor
                    :label "luft capture readback"
                    :size (* 4 (first extent) (second extent))
                    :usage '(:copy-dst)))))
    (unwind-protect
         (progn
           (luv::call-on-sdl-canvas-thread
            (viewer-canvas viewer)
            (lambda ()
              (present-canvas-frame
               context
               (lambda (surface-texture encoder presentation-time)
                 (declare (ignore presentation-time))
                 (encode-viewer-frame
                  viewer encoder surface-texture extent
                  :inspector-p inspector-p)
                 (encode encoder
                         (make-gpu-copy-texture-to-buffer-command
                          :source surface-texture :destination buffer))))))
           (ensure-directories-exist pathname)
           (write-rgba-png pathname (read-buffer buffer)
                           (first extent) (second extent)
                           (canvas-format context)))
      (destroy buffer))))

(defun refresh-viewer-renderer (&optional (viewer *viewer*)
                                &key (solid (make-miter-study-scene)))
  "Rebuild VIEWER's renderer so edited shaders and geometry take effect."
  (when viewer
    (luv::call-on-sdl-canvas-thread
     (viewer-canvas viewer)
     (lambda ()
       (let* ((context (viewer-context viewer))
              (old (viewer-renderer viewer))
              (mesh (make-render-mesh solid))
              (was-running-p (viewer-running-p viewer)))
         (setf (viewer-running-p viewer) nil)
         (unwind-protect
              (setf (viewer-renderer viewer)
                    (make-renderer (viewer-device viewer) mesh
                                   (canvas-format context)
                                   (canvas-extent context))
                    (viewer-source viewer) solid
                    (viewer-inspection viewer) nil)
           (setf (viewer-running-p viewer) was-running-p))
         (when old (destroy-renderer old))))))
  (values))

(defun stop-viewer (&optional (viewer *viewer*))
  "Quiesce VIEWER, release its renderer, then close its canvas and device.

The first caller owns teardown. Concurrent callers wait for that teardown to
finish, which makes native close, Control-Q, the standalone unwind cleanup,
and an interactive STOP-VIEWER the same idempotent application operation."
  (when viewer
    (let ((owner-p nil))
      (sb-thread:with-mutex ((viewer-stop-lock viewer))
        (case (viewer-stop-state viewer)
          (:running
           (setf (viewer-stop-state viewer) :stopping
                 (viewer-running-p viewer) nil
                 owner-p t))
          (:stopping
           (loop while (eq :stopping (viewer-stop-state viewer))
                 do (sb-thread:condition-wait
                     (viewer-stop-ready viewer) (viewer-stop-lock viewer))))))
      (when owner-p
        (let ((errors nil)
              (canvas (viewer-canvas viewer)))
          (labels ((release (part function)
                     (handler-case (funcall function)
                       (error (condition)
                         (push (cons part condition) errors)))))
            (unwind-protect
                 (progn
                   (clear-viewer-controls viewer)
                   (when (member (canvas-state canvas) '(:opening :open))
                     (release :clock
                              (lambda ()
                                (setf (canvas-clock canvas)
                                      (make-demand-clock))))
                     (when (viewer-pointer-captured-p viewer)
                       (release :pointer-capture
                                (lambda ()
                                  (set-canvas-relative-pointer-mode
                                   canvas nil)))
                       (setf (viewer-pointer-captured-p viewer) nil))
                     ;; The synchronous no-op is the owner-thread barrier: no
                     ;; already-running frame can still hold renderer state.
                     (release :canvas-quiescence
                              (lambda ()
                                (request-canvas-frame
                                 canvas
                                 (lambda (timestamp)
                                   (declare (ignore timestamp)))))))
                   (setf (canvas-event-handler canvas) nil)
                   (release :surface-views
                            (lambda ()
                              (release-viewer-surface-views viewer)))
                   (when (viewer-renderer viewer)
                     (release :renderer
                              (lambda ()
                                (destroy-renderer
                                 (viewer-renderer viewer))))
                     (setf (viewer-renderer viewer) nil))
                   (unless (eq :disowned (clim:frame-state viewer))
                     (release :inspector-frame
                              (lambda () (clim:destroy-frame viewer))))
                   (when (member (canvas-state canvas) '(:opening :open))
                     (release :canvas (lambda () (close-canvas canvas))))
                   (when (viewer-device viewer)
                     (release :device
                              (lambda () (destroy (viewer-device viewer))))))
              (when (eq viewer *viewer*)
                (setf *viewer* nil))
              (sb-thread:with-mutex ((viewer-stop-lock viewer))
                (setf (viewer-stop-state viewer) :stopped)
                (sb-thread:condition-broadcast (viewer-stop-ready viewer))))
            (when errors
              (warn "LUFT viewer release failed in ~{~A~^, ~}: ~A"
                    (mapcar #'car (reverse errors))
                    (cdar errors))))))))
  (values))

(defun run-standalone-viewer ()
  (let ((viewer (start-viewer)))
    (unwind-protect
         (loop while (viewer-running-p viewer) do (sleep 0.05))
      (stop-viewer viewer))))
