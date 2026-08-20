(in-package #:luvcraft.agent)

;;; An embodied glance has two inseparable products: a perspective image and
;;; a small exact reading of the resident world around the body.  The image is
;;; what the figure can see; the axis-aligned census is explicitly a sampled
;;; neighbourhood rather than a fictitious bounding box for the view frustum.

(defparameter *surroundings-horizontal-radius* 12)
(defparameter *surroundings-below* 6)
(defparameter *surroundings-above* 10)
(defparameter *surroundings-camera-distance* 4.5d0)
(defparameter *surroundings-camera-lift* 1.25d0)
(defparameter *surroundings-look-ahead* 3.0d0)

(defclass neighborhood-census ()
  ((minimum :initarg :minimum :reader census-minimum)
   (maximum :initarg :maximum :reader census-maximum)
   (total :initarg :total :reader census-total)
   (resident :initarg :resident :reader census-resident)
   (unavailable :initarg :unavailable :reader census-unavailable)
   (air :initarg :air :reader census-air)
   (kind-counts :initarg :kind-counts :reader census-kind-counts))
  (:documentation
   "An exact material census over one inclusive integer world box."))

(defclass surroundings-observation ()
  ((png :initarg :png :reader surroundings-png)
   (width :initarg :width :reader surroundings-width)
   (height :initarg :height :reader surroundings-height)
   (subject :initarg :subject :reader surroundings-subject)
   (subject-position :initarg :subject-position
                     :reader surroundings-subject-position)
   (facing-yaw :initarg :facing-yaw :reader surroundings-facing-yaw)
   (camera-position :initarg :camera-position
                    :reader surroundings-camera-position)
   (camera-yaw :initarg :camera-yaw :reader surroundings-camera-yaw)
   (camera-pitch :initarg :camera-pitch :reader surroundings-camera-pitch)
   (player-position :initarg :player-position
                    :reader surroundings-player-position)
   (player-offset :initarg :player-offset :reader surroundings-player-offset)
   (player-distance :initarg :player-distance
                    :reader surroundings-player-distance)
   (world-revision :initarg :world-revision
                   :reader surroundings-world-revision)
   (nearby-edit-count :initarg :nearby-edit-count
                      :reader surroundings-nearby-edit-count)
   (census :initarg :census :reader surroundings-census))
  (:documentation
   "One clean third-person PNG and the compact world reading captured with it."))

(defun inclusive-box-volume (minimum maximum)
  (destructuring-bind (minimum-x minimum-y minimum-z) minimum
    (destructuring-bind (maximum-x maximum-y maximum-z) maximum
      (* (1+ (- maximum-x minimum-x))
         (1+ (- maximum-y minimum-y))
         (1+ (- maximum-z minimum-z))))))

(defun embodied-agent-neighborhood-bounds (agent)
  (multiple-value-bind (x y z) (luvcraft:body-cell agent)
    (values (list (- x *surroundings-horizontal-radius*)
                  (- y *surroundings-below*)
                  (- z *surroundings-horizontal-radius*))
            (list (+ x *surroundings-horizontal-radius*)
                  (+ y *surroundings-above*)
                  (+ z *surroundings-horizontal-radius*)))))

(defun census-block-neighborhood (world minimum maximum)
  "Count each resident dense lane of WORLD inside inclusive MINIMUM..MAXIMUM."
  (destructuring-bind (minimum-x minimum-y minimum-z) minimum
    (destructuring-bind (maximum-x maximum-y maximum-z) maximum
      (let ((resident 0)
            (air 0)
            (counts (make-hash-table :test #'eq)))
        (dolist (chunk (world:resident-world-chunks world))
          (let* ((domain (world:block-chunk-domain chunk))
                 (shape (world:voxel-space-chunk-shape
                         (world:chunk-domain-space domain)))
                 (width (world:chunk-shape-width shape))
                 (height (world:chunk-shape-height shape))
                 (depth (world:chunk-shape-depth shape)))
            (multiple-value-bind (origin-x origin-y origin-z)
                (world:chunk-domain-world-components domain 0 0 0)
              (let ((from-x (max minimum-x origin-x))
                    (from-y (max minimum-y origin-y))
                    (from-z (max minimum-z origin-z))
                    (to-x (min maximum-x (+ origin-x width -1)))
                    (to-y (min maximum-y (+ origin-y height -1)))
                    (to-z (min maximum-z (+ origin-z depth -1))))
                (when (and (<= from-x to-x) (<= from-y to-y) (<= from-z to-z))
                  (world:with-block-content-storage
                      (dense-domain palette indices) chunk
                    (declare (ignore dense-domain))
                    (loop for world-z from from-z to to-z do
                      (loop for world-y from from-y to to-y do
                        (loop for world-x from from-x to to-x
                              for offset =
                                (world:chunk-domain-offset-components
                                 domain (- world-x origin-x)
                                 (- world-y origin-y)
                                 (- world-z origin-z))
                              for block = (aref palette (aref indices offset))
                              do (incf resident)
                                 (if block
                                     (incf (gethash (luvcraft:block-kind-name block)
                                                    counts 0))
                                     (incf air)))))))))))
        (let ((total (inclusive-box-volume minimum maximum)))
          (make-instance
           'neighborhood-census
           :minimum minimum :maximum maximum :total total :resident resident
           :unavailable (- total resident) :air air
           :kind-counts
           (sort (loop for name being the hash-keys of counts
                         using (hash-value count)
                       collect (cons name count))
                 (lambda (left right)
                   (if (= (cdr left) (cdr right))
                       (string< (symbol-name (car left))
                                (symbol-name (car right)))
                       (> (cdr left) (cdr right)))))))))))

(defun authored-edits-in-box (world minimum maximum)
  (let ((source (world:block-world-source world)))
    (if (typep source 'luvcraft:little-world-source)
        (destructuring-bind (minimum-x minimum-y minimum-z) minimum
          (destructuring-bind (maximum-x maximum-y maximum-z) maximum
            (loop for coordinate being the hash-keys
              of (world:block-edit-overlay-entries
                        (luvcraft:little-world-source-edits source))
                  count (destructuring-bind (x y z) coordinate
                          (and (<= minimum-x x maximum-x)
                               (<= minimum-y y maximum-y)
                               (<= minimum-z z maximum-z))))))
        0)))

(defun surroundings-camera-pose-for (agent)
  "Return an above-and-behind pose looking forward over AGENT."
  (let* ((yaw (embodied-agent-facing-yaw agent))
         (forward-x (sin yaw))
         (forward-z (cos yaw))
         (body-x (luvcraft::body-x agent))
         (body-y (luvcraft::body-y agent))
         (body-z (luvcraft::body-z agent))
         (camera-y (+ body-y (embodied-agent-body-height agent)
                      *surroundings-camera-lift*))
         (camera-x (- body-x (* *surroundings-camera-distance* forward-x)))
         (camera-z (- body-z (* *surroundings-camera-distance* forward-z)))
         (target-y (+ body-y (* 0.65d0 (embodied-agent-body-height agent))))
         (target-x (+ body-x (* *surroundings-look-ahead* forward-x)))
         (target-z (+ body-z (* *surroundings-look-ahead* forward-z)))
         (horizontal-distance
           (sqrt (+ (expt (- target-x camera-x) 2)
                    (expt (- target-z camera-z) 2))))
         (pitch (atan (- target-y camera-y) horizontal-distance)))
    (luvcraft::make-camera-pose
     (luvcraft::make-vec3 camera-x camera-y camera-z)
     yaw pitch luvcraft::+luvcraft-camera-focused-vertical-field-of-view+)))

(defun read-binary-file (pathname)
  (with-open-file (stream pathname :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun capture-surroundings-observation (agent)
  "Capture one clean agent glance, including its bounded dense census."
  (check-type agent embodied-agent)
  (let* ((session (gnome-session agent))
         (world (luvcraft:luvcraft-session-world session))
         (player (luvcraft:luvcraft-session-player session))
         (pose nil)
         (facts nil)
         (pathname
           (merge-pathnames
            (make-pathname :name (format nil "luv-surroundings-~36R"
                                         (random (expt 36 10)))
                           :type "png")
            (uiop:temporary-directory))))
    (unwind-protect
         (multiple-value-bind (written pixels width height format metadata)
             (luvcraft:capture-luvcraft-screenshot
              session pathname
              :camera-pose
              (lambda (ignored-session)
                (declare (ignore ignored-session))
                (setf pose (surroundings-camera-pose-for agent)))
              :metadata-function
              (lambda (ignored-session)
                (declare (ignore ignored-session))
                (multiple-value-bind (minimum maximum)
                    (embodied-agent-neighborhood-bounds agent)
                  (let* ((census (census-block-neighborhood world minimum maximum))
                         (agent-position (list (luvcraft::body-x agent)
                                               (luvcraft::body-y agent)
                                               (luvcraft::body-z agent)))
                         (player-position (list (luvcraft:player-x player)
                                                (luvcraft:player-y player)
                                                (luvcraft:player-z player)))
                         (offset (mapcar #'- player-position agent-position))
                         (distance (sqrt (reduce #'+ offset
                                                 :key (lambda (value)
                                                        (* value value))))))
                    (setf facts
                          (list :subject (embodied-agent-name agent)
                                :subject-position agent-position
                                :facing-yaw (embodied-agent-facing-yaw agent)
                                :player-position player-position
                                :player-offset offset :player-distance distance
                                :world-revision
                                (world:block-world-revision world)
                                :nearby-edit-count
                                (authored-edits-in-box world minimum maximum)
                                :census census)))))
              :include-hud-p nil :include-viewmodel-p nil)
           (declare (ignore pixels format metadata))
           (let ((camera-position
                   (luvcraft::camera-pose-position pose)))
             (make-instance
              'surroundings-observation
              :png (read-binary-file written) :width width :height height
              :subject (getf facts :subject)
              :subject-position (getf facts :subject-position)
              :facing-yaw (getf facts :facing-yaw)
              :camera-position
              (list (luvcraft::vec3-x camera-position)
                    (luvcraft::vec3-y camera-position)
                    (luvcraft::vec3-z camera-position))
              :camera-yaw (luvcraft::camera-pose-yaw pose)
              :camera-pitch (luvcraft::camera-pose-pitch pose)
              :player-position (getf facts :player-position)
              :player-offset (getf facts :player-offset)
              :player-distance (getf facts :player-distance)
              :world-revision (getf facts :world-revision)
              :nearby-edit-count (getf facts :nearby-edit-count)
              :census (getf facts :census))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun present-position (position stream)
  (destructuring-bind (x y z) position
    (format stream "(~,2F, ~,2F, ~,2F)" x y z)))

(define-presentation-method present
    (observation (type surroundings-observation) stream
                 (view textual-view) &key)
  (let ((census (surroundings-census observation)))
    (format stream "~:(~A~) sees a ~Dx~D third-person view, facing ~A.~%"
            (surroundings-subject observation)
            (surroundings-width observation) (surroundings-height observation)
            (compass-word (surroundings-facing-yaw observation)))
    (write-string "Agent " stream)
    (present-position (surroundings-subject-position observation) stream)
    (write-string "; camera " stream)
    (present-position (surroundings-camera-position observation) stream)
    (format stream " yaw=~,3F pitch=~,3F.~%"
            (surroundings-camera-yaw observation)
            (surroundings-camera-pitch observation))
    (write-string "Player " stream)
    (present-position (surroundings-player-position observation) stream)
    (write-string "; offset from agent " stream)
    (present-position (surroundings-player-offset observation) stream)
    (format stream "; distance ~,2F.~%" (surroundings-player-distance observation))
    (format stream "Sampled neighbourhood ~S..~S: ~:D cells; ~:D resident (~:D air), ~:D unavailable.~%"
            (census-minimum census) (census-maximum census)
            (census-total census) (census-resident census) (census-air census)
            (census-unavailable census))
    (if (census-kind-counts census)
        (format stream "Blocks: ~{~(~A~)=~:D~^, ~}.~%"
                (loop for (name . count) in (census-kind-counts census)
                      append (list name count)))
        (write-string "Blocks: none.~%" stream))
    (format stream "World revision ~D; ~:D authored edit~:P in the sampled box."
            (surroundings-world-revision observation)
            (surroundings-nearby-edit-count observation))))

(define-command (com-view-surroundings :command-table luvcraft-agent
                                        :name "View Surroundings")
    ()
  "Look forward from just above and behind your body. Return a clean third-person image together with agent, camera, and player coordinates and an exact material census of a nearby sampled voxel box. Unavailable cells are distinct from air."
  (let* ((session (luvcraft.clim:luvcraft-command-session))
         (subject (command-subject session)))
    (unless (typep subject 'embodied-agent)
      (error "View Surroundings needs an embodied agent presence."))
    (capture-surroundings-observation subject)))

(defmethod command-tool-runs-on-canvas-p
    ((command (eql 'com-view-surroundings)))
  ;; CAPTURE-LUVCRAFT-SCREENSHOT owns its own canvas request and waits for the
  ;; readback; nesting it inside EXECUTE-COMMAND-FOR-AGENT would deadlock.
  nil)

(defmethod command-provider-output
    ((command (eql 'com-view-surroundings)) values text)
  (declare (ignore command))
  (let ((observation (first values)))
    (openai:make-tool-output
     :text text
     :images
     (list (openai:make-tool-output-image
            (surroundings-png observation)
            :media-type "image/png" :detail "high")))))
