(in-package #:luft.render)

;;; Cameras and their projections are the renderer's: a game owns where its
;;; camera stands and how it moves, and the renderer turns that pose into the
;;; frame's view, its temporal jitter, the scene uniform lanes, and the ray
;;; through a point on the screen.  The atelier's follow, orbit, and
;;; first-person behaviours and the isometric wizard's are clients of this
;;; file.

(defparameter *projection* :isometric
  "Either :PERSPECTIVE or :ISOMETRIC.

An isometric picture has no vanishing point, so two chamfers the same width
are the same width on screen wherever they sit.  That is what makes it the
projection to judge a shape rule in.")

(defparameter *isometric-height* 28.0
  "How many world units of height an isometric frame spans.")

(defconstant +orthographic-near+ -4096.0)
(defconstant +orthographic-far+ 4096.0)
(defconstant +camera-yaw-response-rate+ 8.0)

(defclass fly-camera ()
  ((position :initarg :position :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (target-yaw :initarg :target-yaw :initform nil :accessor camera-target-yaw)
   (pitch :initarg :pitch :initform 0.0 :accessor camera-pitch)
   (field-of-view :initarg :field-of-view :initform (* 70.0 (/ pi 180))
                  :accessor camera-field-of-view)))

(defmethod initialize-instance :after ((camera fly-camera) &key)
  (unless (camera-target-yaw camera)
    (setf (camera-target-yaw camera) (camera-yaw camera))))

(defmethod update-instance-for-redefined-class :after
    ((camera fly-camera) added-slots discarded-slots property-list
     &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (unless (camera-target-yaw camera)
    (setf (camera-target-yaw camera) (camera-yaw camera))))

(defmethod (setf camera-yaw) :after (yaw (camera fly-camera))
  "Keep an explicit camera pose assignment an immediate, settled cut."
  (setf (camera-target-yaw camera) yaw))

(defun shortest-angle-difference (target current)
  (- (mod (+ (- target current) pi) (* 2 pi)) pi))

(defun target-camera-yaw (camera yaw)
  "Ask CAMERA to turn toward YAW without discontinuously changing its pose."
  (setf (camera-target-yaw camera) (mod yaw (* 2 pi)))
  camera)

(defun advance-camera-response (camera seconds)
  "Advance CAMERA's rendered pose toward its requested pose.

The exact exponential response is independent of frame rate.  Pose setters
remain cuts; camera behaviors use targets when motion should be visible."
  (let* ((current (camera-yaw camera))
         (difference
           (shortest-angle-difference (camera-target-yaw camera) current)))
    (setf (slot-value camera 'yaw)
          (if (< (abs difference) 1.0e-5)
              (camera-target-yaw camera)
              (+ current
                 (* difference
                    (- 1.0
                       (exp (- (* +camera-yaw-response-rate+ seconds)))))))))
  camera)

(defun camera-basis (camera)
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (make-vec3 (* (cos yaw) (cos pitch))
                             (* (sin yaw) (cos pitch))
                             (sin pitch)))
         (right (make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3-cross right forward)))
    (values right up forward)))

(defun add-scaled-directions (origin &rest direction-scales)
  (let ((x (vec3-x origin))
        (y (vec3-y origin))
        (z (vec3-z origin)))
    (loop for (direction scale) on direction-scales by #'cddr
          do (incf x (* (vec3-x direction) scale))
             (incf y (* (vec3-y direction) scale))
             (incf z (* (vec3-z direction) scale)))
    (make-vec3 x y z)))

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
  "Sample the sixteen-position Halton(2,3) sequence in clip coordinates."
  (let ((sample (1+ (mod frame-index 16))))
    (vector (coerce (/ (* 2.0 (- (halton sample 2) 0.5))
                       (max width 1))
                    'single-float)
            (coerce (/ (* 2.0 (- (halton sample 3) 0.5))
                       (max height 1))
                    'single-float))))

(defun capture-frame-view (camera width height jitter)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let ((near (if (eq *projection* :isometric)
                    +orthographic-near+ 0.1))
          (far (if (eq *projection* :isometric)
                   +orthographic-far+ 600.0)))
      (multiple-value-bind (px py pz pw divisor)
          (projection-lane width height (camera-field-of-view camera)
                           near far)
        (%make-frame-view
         :position (let ((position (camera-position camera)))
                     (make-vec3 (vec3-x position)
                                (vec3-y position)
                                (vec3-z position)))
         :right right :up up :forward forward
         :projection (vector px py pz pw)
         :divisor divisor :jitter jitter)))))

(defun camera-pointer-ray (camera width height x y)
  "Return the world origin and direction of the ray through CAMERA's screen.

X and Y are a point on a WIDTH by HEIGHT screen, in the same logical units,
measured from its top left.  A perspective ray leaves the eye; an isometric
ray leaves the screen plane parallel to the view, as *PROJECTION* draws."
  (let* ((view (capture-frame-view camera width height #(0.0 0.0)))
         (projection (frame-view-projection view))
         (ndc-x (- (* 2.0 (/ x width)) 1.0))
         (ndc-y (- 1.0 (* 2.0 (/ y height))))
         (right-scale (/ ndc-x (aref projection 0)))
         (up-scale (/ ndc-y (aref projection 1))))
    (if (eq *projection* :perspective)
        (values
         (camera-position camera)
         (vec3-normalize
          (add-scaled-directions
           (frame-view-forward view)
           (frame-view-right view) right-scale
           (frame-view-up view) up-scale)))
        (values
         (add-scaled-directions
          (camera-position camera)
          (frame-view-right view) right-scale
          (frame-view-up view) up-scale)
         (frame-view-forward view)))))

(defun frame-views-continuous-p (previous current)
  "Whether PREVIOUS and CURRENT describe motion rather than a camera cut."
  (and previous
       (let* ((new-position (frame-view-position current))
              (old-position (frame-view-position previous))
              (delta (make-vec3
                      (- (vec3-x new-position)
                         (vec3-x old-position))
                      (- (vec3-y new-position)
                         (vec3-y old-position))
                      (- (vec3-z new-position)
                         (vec3-z old-position))))
              (distance-squared (vec3-dot delta delta))
              (facing (vec3-dot (frame-view-forward previous)
                                (frame-view-forward current)))
              (old-projection (frame-view-projection previous))
              (new-projection (frame-view-projection current)))
         (and (< distance-squared 64.0)
              (> facing 0.5)
              (< (abs (- (aref old-projection 1)
                         (aref new-projection 1)))
                 0.25)))))

(defun figure-lanes (feet previous-feet gait previous-gait
                     heading-x heading-y previous-heading-x previous-heading-y)
  "The figure stage's three vec4 lanes: where it stands now and a frame ago,
each with its gait phase, and its present and previous heading.

FEET and PREVIOUS-FEET are where the figure's feet touch the ground; the
robed figure's own origin sits above them.  A game passes the result to
CAMERA-UNIFORM-DATA or ENCODE-RENDERER-VIEW as its FIGURE."
  (flet ((position-lane (position gait)
           (list (vec3-x position) (vec3-y position)
                 (+ (vec3-z position) 1.48) gait)))
    (list (position-lane feet gait)
          (position-lane previous-feet previous-gait)
          (list heading-x heading-y previous-heading-x previous-heading-y))))

(defun camera-uniform-data
    (view previous inspection-parameters ink-strength figure
     &optional (bevel-width luft:+mesh-bevel-width+) (exposure 1.0f0) first-person-p)
  "Pack VIEW, PREVIOUS, and FIGURE (FIGURE-LANES, or NIL) as scene uniforms."
  (flet ((lane (vector fourth)
           (list (vec3-x vector) (vec3-y vector)
                 (vec3-z vector) fourth)))
    (destructuring-bind (character previous-character character-direction)
        (or figure
            '((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 1.0 0.0 1.0)))
      (make-array
       (shaders::scene-uniform-scalar-count) :element-type 'single-float
       :initial-contents
       (mapcar
        (lambda (value) (coerce value 'single-float))
        (append (lane (frame-view-position view) (if first-person-p 1.0 0.0))
                (lane (frame-view-right view) 0.0)
                (lane (frame-view-up view) 0.0)
                (lane (frame-view-forward view) 0.0)
                (coerce (frame-view-projection view) 'list)
                (list (/ bevel-width luft:+mesh-cell-size+)
                      *wireframe*
                      (frame-view-divisor view) ink-strength)
                (lane (frame-view-position previous) 0.0)
                (lane (frame-view-right previous) 0.0)
                (lane (frame-view-up previous) 0.0)
                (lane (frame-view-forward previous) 0.0)
                (coerce (frame-view-projection previous) 'list)
                ;; MetalFX receives the current sample offset separately and
                ;; its default motion-vector contract is unjittered.  Retain
                ;; the previous lane shape for ABI stability, but only the
                ;; current two components participate in rasterization.
                (list (aref (frame-view-jitter view) 0)
                      (aref (frame-view-jitter view) 1)
                      (aref (frame-view-jitter previous) 0)
                      (aref (frame-view-jitter previous) 1))
                (coerce inspection-parameters 'list)
                character
                (light-uniform-data (current-light) (frame-view-position view) exposure)
                previous-character character-direction
                (atmosphere-uniform-data *light* (or *flame-time* *sky-elapsed* 0.0))
                ;; The renderer writes the attachment lights at upload.
                (make-list +point-light-scalar-count+ :initial-element 0.0)))))))

