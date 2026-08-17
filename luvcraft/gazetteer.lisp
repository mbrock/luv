;;; Named screenshot views for semantic luvcraft visual checks.

(in-package #:luvcraft)

(defclass gazetteer-open-sky-source () ())

(defmethod absent-chunk-light-semantics
    ((source gazetteer-open-sky-source) world chunk-key direction)
  (declare (ignore source world chunk-key))
  (cond ((plusp (voxel-direction-dy direction)) :open-sky)
        ((minusp (voxel-direction-dy direction)) :closed)
        (t :unknown)))

(defclass luvcraft-gazetteer-view ()
  ((name :initarg :name :reader luvcraft-gazetteer-view-name)
   (description :initarg :description
                :reader luvcraft-gazetteer-view-description)
   (title :initarg :title :reader luvcraft-gazetteer-view-title)
   (world-factory :initarg :world-factory
                  :reader luvcraft-gazetteer-view-world-factory)
   (camera-factory :initarg :camera-factory
                   :reader luvcraft-gazetteer-view-camera-factory)
   (sky-clock-factory :initarg :sky-clock-factory
                      :reader luvcraft-gazetteer-view-sky-clock-factory)
   (sky-profile-factory :initarg :sky-profile-factory
                        :reader luvcraft-gazetteer-view-sky-profile-factory)
   (critter-factory :initarg :critter-factory
                    :reader luvcraft-gazetteer-view-critter-factory)
   (width :initarg :width :initform nil
          :reader luvcraft-gazetteer-view-width)
   (height :initarg :height :initform nil
           :reader luvcraft-gazetteer-view-height)))

(defun make-gazetteer-floor-world (&key (chunk-count 1)
                                        (crystal-x 8)
                                        (crystal-y 1)
                                        (crystal-z 8))
  (let ((world (make-block-world)))
    (dotimes (chunk-x chunk-count)
      (ensure-world-chunk world chunk-x 0 0)
      (loop for x from (* chunk-x 16) below (* (1+ chunk-x) 16) do
        (loop for z below 16 do
          (setf (world-block-at world x 0 z) *stone-block*))))
    (setf (world-block-at world crystal-x crystal-y crystal-z) *crystal-block*)
    (relight-block-world world)
    world))

(defun make-gazetteer-shadow-yard-world ()
  "A deliberately artificial yard for checking legible cast shadows."
  (let ((world (make-block-world
                :source (make-instance 'gazetteer-open-sky-source))))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 16 do
      (loop for z below 16 do
        (setf (world-block-at world x 0 z) *snow-block*)))
    ;; A tall simple pillar should cast an unmistakable pillar-shaped shadow
    ;; across the bright receiver under the pinned low sun.
    (loop for y from 1 to 8 do
      (setf (world-block-at world 9 y 10) *stone-block*
            (world-block-at world 10 y 10) *stone-block*))
    (relight-block-world world)
    world))

(defun make-gazetteer-meadow-world ()
  "A small open grass meadow with a sandy patch, for looking at animals."
  (let ((world (make-block-world
                :source (make-instance 'gazetteer-open-sky-source))))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 16 do
      (loop for z below 16 do
        (setf (world-block-at world x 0 z) *dirt-block*
              (world-block-at world x 1 z)
              (if (and (<= 9 x 14) (<= 2 z 7))
                  *sand-block*
                  *grass-block*))))
    ;; A couple of blocks for scale, and something for a turtle to walk around.
    (setf (world-block-at world 4 2 11) *stone-block*
          (world-block-at world 5 2 11) *stone-block*
          (world-block-at world 4 3 11) *stone-block*)
    (relight-block-world world)
    world))

(defun make-gazetteer-turtle (x y z yaw gait-phase &key resting-p (seed 7))
  "Pose one turtle for a still photograph rather than letting it wander."
  (let ((turtle (make-instance 'turtle
                               :seed seed
                               :position (make-vec3 x y z)
                               :yaw yaw :heading yaw)))
    (setf (turtle-gait-phase turtle) gait-phase
          (turtle-resting-p turtle) resting-p
          (critter-grounded-p turtle) t)
    turtle))

(defun make-gazetteer-turtle-population ()
  "Three turtles posed across the meadow: mid-stride, turning, and resting."
  (let ((population (make-instance 'critter-population)))
    (dolist (turtle
             (list (make-gazetteer-turtle 7.8d0 2d0 6.7d0 2.15d0 1.1d0
                                          :seed 11)
                   (make-gazetteer-turtle 10.4d0 2d0 8.4d0 4.90d0 3.9d0
                                          :seed 12)
                   (make-gazetteer-turtle 5.0d0 2d0 8.0d0 0.90d0 0d0
                                          :resting-p t :seed 13)))
      (add-critter population turtle))
    population))

(defun make-pinned-sky-clock (day-fraction)
  (make-instance 'sky-clock :pinned-day-fraction day-fraction))

(defun make-shadow-yard-sky-profile ()
  "A single-frame high-contrast profile for visual shadow inspection."
  (make-sky-profile
   (list
    (make-sky-keyframe
     :day-fraction 0.36
     :zenith-color #(0.28 0.52 0.88)
     :horizon-color #(0.72 0.84 0.96)
     :sun-color #(2.20 2.05 1.70)
     :ambient-color #(0.06 0.08 0.12)
     :fog-color #(0.50 0.72 0.94)
     :sun-angular-width 0.006))))

(defun make-luvcraft-gazetteer-view
    (name description title world-factory camera-factory day-fraction
     &key width height sky-profile-factory critter-factory)
  (make-instance
   'luvcraft-gazetteer-view
   :name name
   :description description
   :title title
   :world-factory world-factory
   :camera-factory camera-factory
   :sky-clock-factory (lambda () (make-pinned-sky-clock day-fraction))
   :sky-profile-factory (or sky-profile-factory #'make-default-sky-profile)
   :critter-factory
   (or critter-factory
       (lambda () (make-instance 'critter-population)))
   :width width
   :height height))

(defparameter *luvcraft-gazetteer-views*
  (list
   (make-luvcraft-gazetteer-view
    :little-world-noon
    "Generated terrain, ordinary play camera, pinned noon."
    "luvcraft gazetteer - little world noon"
    (lambda () (make-empty-little-block-world :seed 121))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 8d0 16d0 -18d0)
                              :yaw 0.0d0 :pitch -0.42d0))
    0.50)
   (make-luvcraft-gazetteer-view
    :little-world-dusk
    "Generated terrain under a warm low sky."
    "luvcraft gazetteer - little world dusk"
    (lambda () (make-empty-little-block-world :seed 31))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 -7d0 12d0 -8d0)
                              :yaw 0.42d0 :pitch -0.30d0))
    0.74)
   (make-luvcraft-gazetteer-view
    :shadow-forest
    "Generated terrain for representative cast-shadow inspection in motion."
    "luvcraft gazetteer - shadow forest"
    (lambda () (make-empty-little-block-world :seed 121))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 8d0 16d0 -18d0)
                              :yaw 0.0d0 :pitch -0.42d0))
    0.42
    :width 1512 :height 982)
   (make-luvcraft-gazetteer-view
    :glow-floor
    "A placed emitter proving material emission and blocklight under a dark sky."
    "luvcraft gazetteer - glow floor"
    (lambda () (make-gazetteer-floor-world))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 8d0 4d0 1.5d0)
                              :yaw 0.0d0 :pitch -0.28d0))
    0.90)
   (make-luvcraft-gazetteer-view
    :crystal-seam
    "A crystal on a chunk seam, lighting both sides of the boundary."
    "luvcraft gazetteer - crystal seam"
    (lambda () (make-gazetteer-floor-world
                :chunk-count 2 :crystal-x 16 :crystal-y 1 :crystal-z 8))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 15.5d0 4.2d0 1.2d0)
                              :yaw 0.05d0 :pitch -0.28d0))
    0.90)
   (make-luvcraft-gazetteer-view
    :turtle-meadow
    "Three posed turtles on a grass meadow: model, materials, and cast shadow."
    "luvcraft gazetteer - turtle meadow"
    (lambda () (make-gazetteer-meadow-world))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 7.4d0 2.85d0 3.4d0)
                              :yaw 0.10d0 :pitch -0.20d0))
    0.42
    :width 960 :height 640
    :critter-factory #'make-gazetteer-turtle-population)
   (make-luvcraft-gazetteer-view
    :shadow-yard
    "A low sun over a flat receiver with elevated block casters."
    "luvcraft gazetteer - shadow yard"
    (lambda () (make-gazetteer-shadow-yard-world))
    (lambda () (make-instance 'fly-camera
                              :position (make-vec3 7.5d0 9.5d0 -7d0)
                              :yaw 0.10d0 :pitch -0.50d0))
    0.36
    :width 960 :height 640
    :sky-profile-factory #'make-shadow-yard-sky-profile)))

(defun luvcraft-gazetteer-views ()
  "Return the semantic screenshot views captured by the luvcraft gazetteer."
  (copy-list *luvcraft-gazetteer-views*))

(defun normalize-luvcraft-gazetteer-view-name (name)
  (etypecase name
    (keyword name)
    (symbol (intern (string-upcase (symbol-name name)) :keyword))
    (string (intern (string-upcase name) :keyword))))

(defun find-luvcraft-gazetteer-view (name &key (errorp t))
  (let* ((normalized (normalize-luvcraft-gazetteer-view-name name))
         (view (find normalized *luvcraft-gazetteer-views*
                     :key #'luvcraft-gazetteer-view-name)))
    (cond (view view)
          (errorp
           (error "No luvcraft gazetteer view named ~S." name))
          (t nil))))

(defun luvcraft-gazetteer-view-pathname (directory view)
  (merge-pathnames
   (format nil "~(~A~).png" (luvcraft-gazetteer-view-name view))
   (uiop:ensure-directory-pathname directory)))

(defun capture-luvcraft-gazetteer-view
    (view directory &key width height (provider *gpu-provider*))
  "Capture one gazetteer VIEW into DIRECTORY and return the PNG pathname."
  (let* ((view (if (typep view 'luvcraft-gazetteer-view)
                   view
                   (find-luvcraft-gazetteer-view view)))
         (pathname (luvcraft-gazetteer-view-pathname directory view))
         (world (funcall (luvcraft-gazetteer-view-world-factory view)))
         (camera (funcall (luvcraft-gazetteer-view-camera-factory view)))
         (sky-clock
           (funcall (luvcraft-gazetteer-view-sky-clock-factory view)))
         (sky-profile
           (funcall (luvcraft-gazetteer-view-sky-profile-factory view)))
         (critters
           (funcall (luvcraft-gazetteer-view-critter-factory view))))
    (capture-hidden-luvcraft-screenshot
     pathname
     :title (luvcraft-gazetteer-view-title view)
     :width (or width (luvcraft-gazetteer-view-width view) 960)
     :height (or height (luvcraft-gazetteer-view-height view) 640)
     :provider provider
     :world world
     :camera camera
     :critters critters
     :sky-clock sky-clock
     :sky-profile sky-profile)
    pathname))

(defun capture-luvcraft-gazetteer
    (directory &key views width height (provider *gpu-provider*))
  "Capture VIEWS, or every known gazetteer view, into DIRECTORY."
  (let ((views (if views
                   (mapcar #'find-luvcraft-gazetteer-view views)
                   (luvcraft-gazetteer-views))))
    (loop for view in views
          collect (capture-luvcraft-gazetteer-view
                   view directory :width width :height height
                   :provider provider))))

(defun capture-luvcraft-gazetteer-sequence
    (view directory &key (count 4) (forward-step 0.2) (yaw-step 0.0)
                         day-start (day-step 0.0) difference-scale
                         shadow-diagnostic-p width height
                         (provider *gpu-provider*))
  "Capture one gazetteer VIEW as a consecutive spatial or temporal sequence."
  (let* ((view (if (typep view 'luvcraft-gazetteer-view)
                   view
                   (find-luvcraft-gazetteer-view view)))
         (world (funcall (luvcraft-gazetteer-view-world-factory view)))
         (camera (funcall (luvcraft-gazetteer-view-camera-factory view)))
         (sky-clock
           (funcall (luvcraft-gazetteer-view-sky-clock-factory view)))
         (sky-profile
           (funcall (luvcraft-gazetteer-view-sky-profile-factory view)))
         (critters
           (funcall (luvcraft-gazetteer-view-critter-factory view))))
    (capture-hidden-luvcraft-frames
     directory
     :critters critters
     :count count :forward-step forward-step :yaw-step yaw-step
     :day-start day-start :day-step day-step
     :difference-scale difference-scale
     :shadow-diagnostic-p shadow-diagnostic-p
     :provider provider
     :pathname-prefix
     (string-downcase (symbol-name (luvcraft-gazetteer-view-name view)))
     :title (luvcraft-gazetteer-view-title view)
     :width (or width (luvcraft-gazetteer-view-width view) 960)
     :height (or height (luvcraft-gazetteer-view-height view) 640)
     :world world :camera camera :sky-clock sky-clock
     :sky-profile sky-profile)))
