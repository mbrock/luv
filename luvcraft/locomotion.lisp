;;; Destinational movement for any body in the block world.
;;;
;;; A Move To request is intentionally discrete: an integer place, a short
;;; path of standable cells, and a terminal result.  Its realization is not.
;;; The body keeps an ordinary continuous position and velocity and reaches
;;; each waypoint through the same acceleration, gravity, step-up, and voxel
;;; collision motor as held player input.  This is the two-authority model in
;;; #YPGXKI: a compact intention above an honest physical body.

(in-package #:luvcraft)

(defparameter *body-path-horizontal-radius* 10
  "Farthest horizontal cell distance accepted by one Move To action.")
(defparameter *body-path-vertical-radius* 4
  "Farthest vertical cell distance accepted by one Move To action.")
(defparameter *body-path-visit-limit* 1200
  "Maximum cells considered by one nearby path search.")
(defparameter *body-movement-stuck-seconds* 1.75d0
  "Time without useful progress before a Move To action replans.")
(defparameter *body-movement-arrival-radius* 0.09d0
  "Horizontal distance from a cell centre which counts as arriving.")

(defclass body-move-action ()
  ((body :initarg :body :reader body-move-action-body)
   (start :initarg :start :reader body-move-action-start)
   (destination :initarg :destination :reader body-move-action-destination)
   (path :initarg :path :accessor body-move-action-path)
   (status :initform :running :accessor body-move-action-status)
   (detail :initform nil :accessor body-move-action-detail)
   (elapsed :initform 0d0 :accessor body-move-action-elapsed)
   (deadline :initarg :deadline :reader body-move-action-deadline)
   (best-distance :initform most-positive-double-float
                  :accessor body-move-action-best-distance)
   (stuck-seconds :initform 0d0 :accessor body-move-action-stuck-seconds)
   (replans :initform 0 :accessor body-move-action-replans)
   (lock :initform (sb-thread:make-mutex :name "body Move To action")
         :reader body-move-action-lock)
   (completion
    :initform (sb-concurrency:make-mailbox :name "body Move To completion")
    :reader body-move-action-completion))
  (:documentation
   "One bounded, waitable request for a continuous body to reach a cell."))

(defun body-cell (body)
  "Return the integer X, Y, Z cell whose floor contains BODY's base centre."
  (values (floor (body-x body))
          (floor (+ (body-y body) +player-collision-epsilon+))
          (floor (body-z body))))

(defun body-cell-list (body)
  (multiple-value-list (body-cell body)))

(defun body-standable-at-p (body world x y z)
  "Whether BODY can stand centred in cell X,Y,Z on resident solid support."
  (multiple-value-bind (support residency) (world-block-at world x (1- y) z)
    (and (eq residency :resident)
         (block-solid-p support)
         (body-position-clear-p body world (+ x 0.5d0) y (+ z 0.5d0)))))

(defun nearby-body-cell-p (cell start)
  (destructuring-bind (x y z) cell
    (destructuring-bind (start-x start-y start-z) start
      (and (<= (+ (abs (- x start-x)) (abs (- z start-z)))
               *body-path-horizontal-radius*)
           (<= (abs (- y start-y)) *body-path-vertical-radius*)))))

(defun body-cell-neighbors (body world cell start)
  "Standable horizontal neighbors of CELL, allowing one-cell steps up/down."
  (destructuring-bind (x y z) cell
    (loop for (dx dz) in '((0 1) (1 0) (0 -1) (-1 0)) append
      (loop for dy in '(0 1 -1)
            for next = (list (+ x dx) (+ y dy) (+ z dz))
            when (and (nearby-body-cell-p next start)
                      (body-standable-at-p
                       body world (first next) (second next) (third next)))
              collect next))))

(defun reconstruct-body-path (parents start destination)
  (let ((path nil)
        (cell destination))
    (loop until (equal cell start)
          do (push cell path)
             (setf cell (gethash cell parents)))
    path))

(defun find-body-path (body world destination &key (start (body-cell-list body)))
  "Find a bounded four-connected path of standable cells to DESTINATION.

The returned path excludes START and includes DESTINATION.  NIL also denotes
the already-there path; the second value distinguishes that from failure."
  (unless (and (= 3 (length destination)) (every #'integerp destination))
    (error "A body destination must be three integer cells, got ~S." destination))
  (unless (nearby-body-cell-p destination start)
    (return-from find-body-path
      (values nil nil
              (format nil "destination ~{~D~^ ~} is not nearby (start ~{~D~^ ~})"
                      destination start))))
  (unless (or (equal destination start)
              (apply #'body-standable-at-p body world destination))
    (return-from find-body-path
      (values nil nil
              (format nil "destination ~{~D~^ ~} is not a clear supported cell"
                      destination))))
  (when (equal destination start)
    (return-from find-body-path (values nil t nil)))
  (let ((parents (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (queue (make-array 32 :adjustable t :fill-pointer 0))
        (head 0)
        (visits 0))
    (setf (gethash start seen) t)
    (vector-push-extend start queue)
    (loop while (and (< head (length queue))
                     (< visits *body-path-visit-limit*))
          for cell = (aref queue head)
          do (incf head)
             (incf visits)
             (dolist (next (body-cell-neighbors body world cell start))
               (unless (gethash next seen)
                 (setf (gethash next seen) t
                       (gethash next parents) cell)
                 (when (equal next destination)
                   (return-from find-body-path
                     (values (reconstruct-body-path parents start destination)
                             t nil)))
                 (vector-push-extend next queue))))
    (values nil nil
            (format nil "no nearby walkable path reaches ~{~D~^ ~}" destination))))

(defun body-move-action-terminal-p (action)
  (member (body-move-action-status action)
          '(:arrived :failed :cancelled)))

(defun finish-body-move-action (action status &optional detail)
  "Publish ACTION's terminal STATUS exactly once and wake its waiting caller."
  (let ((finished-p nil))
    (sb-thread:with-mutex ((body-move-action-lock action))
      (unless (body-move-action-terminal-p action)
        (setf (body-move-action-status action) status
              (body-move-action-detail action) detail
              finished-p t)))
    (when finished-p
      (let ((body (body-move-action-body action)))
        (when (eq action (body-movement-action body))
          (setf (body-movement-action body) nil))
        (setf (vec3-x (body-velocity body)) 0d0
              (vec3-z (body-velocity body)) 0d0))
      (sb-concurrency:send-message
       (body-move-action-completion action) action))
    action))

(defun cancel-body-movement (body &optional (detail "superseded"))
  "Cancel BODY's current Move To action, if any."
  (alexandria:when-let ((action (body-movement-action body)))
    (finish-body-move-action action :cancelled detail)))

(defun start-body-move-to (body world x y z)
  "Start BODY moving to integer cell X,Y,Z and return its waitable action."
  (cancel-body-movement body)
  (let* ((start (body-cell-list body))
         (destination (list x y z)))
    (multiple-value-bind (path found-p failure)
        (find-body-path body world destination :start start)
      (let ((action
              (make-instance
               'body-move-action
               :body body :start start :destination destination :path path
               :deadline (max 8d0 (* 3d0 (max 1 (length path)))))))
        (setf (body-movement-action body) action)
        (cond ((not found-p)
               (finish-body-move-action action :failed failure))
              ((null path)
               (finish-body-move-action action :arrived "already there")))
        action))))

(defun await-body-move-action (action)
  "Block the calling worker until ACTION reaches a terminal state.

The canvas thread must never call this; it advances ACTION a little each
frame.  A provider/tool thread may wait here without stalling rendering."
  (unless (body-move-action-terminal-p action)
    (sb-concurrency:receive-message (body-move-action-completion action)))
  action)

(defun body-waypoint-distance (body waypoint)
  (destructuring-bind (x y z) waypoint
    (sqrt (+ (expt (- (+ x 0.5d0) (body-x body)) 2)
             (expt (- y (body-y body)) 2)
             (expt (- (+ z 0.5d0) (body-z body)) 2)))))

(defun body-reached-waypoint-p (body waypoint)
  (destructuring-bind (x y z) waypoint
    (and (< (sqrt
             (+ (expt (- (+ x 0.5d0) (body-x body)) 2)
                (expt (- (+ z 0.5d0) (body-z body)) 2)))
            *body-movement-arrival-radius*)
         (< (abs (- y (body-y body))) 0.16d0))))

(defun replan-body-move-action (action world)
  (multiple-value-bind (path found-p failure)
      (find-body-path (body-move-action-body action) world
                      (body-move-action-destination action))
    (cond (found-p
           (setf (body-move-action-path action) path
                 (body-move-action-best-distance action)
                 most-positive-double-float
                 (body-move-action-stuck-seconds action) 0d0)
           t)
          (t
           (finish-body-move-action action :failed failure)
           nil))))

(defun advance-body-movement (body world seconds)
  "Advance BODY's current discrete destination through continuous physics."
  (let ((action (body-movement-action body)))
    (unless (and action (eq :running (body-move-action-status action)))
      (return-from advance-body-movement action))
    (incf (body-move-action-elapsed action) seconds)
    (when (> (body-move-action-elapsed action)
             (body-move-action-deadline action))
      (return-from advance-body-movement
        (finish-body-move-action action :failed "movement timed out")))
    (loop while (and (body-move-action-path action)
                     (body-reached-waypoint-p
                      body (first (body-move-action-path action))))
          do (pop (body-move-action-path action))
             (setf (body-move-action-best-distance action)
                   most-positive-double-float
                   (body-move-action-stuck-seconds action) 0d0))
    (let ((waypoint (first (body-move-action-path action))))
      (if (null waypoint)
          (progn
            (step-walking-body body world 0d0 0d0 seconds)
            (let ((velocity (body-velocity body)))
              (when (and (body-grounded-p body)
                         (< (sqrt (+ (expt (vec3-x velocity) 2)
                                     (expt (vec3-z velocity) 2)))
                            0.12d0))
                (finish-body-move-action action :arrived "destination reached"))))
          (destructuring-bind (x y z) waypoint
            (declare (ignore y))
            (let* ((dx (- (+ x 0.5d0) (body-x body)))
                   (dz (- (+ z 0.5d0) (body-z body)))
                   (distance (max 1d-9 (sqrt (+ (* dx dx) (* dz dz)))))
                   (speed (min (body-walk-speed body) (* 5d0 distance))))
              (step-walking-body body world
                                 (* speed (/ dx distance))
                                 (* speed (/ dz distance))
                                 seconds))
            (let ((distance (body-waypoint-distance body waypoint)))
              (if (< distance (- (body-move-action-best-distance action) 0.015d0))
                  (setf (body-move-action-best-distance action) distance
                        (body-move-action-stuck-seconds action) 0d0)
                  (incf (body-move-action-stuck-seconds action) seconds))
              (when (> (body-move-action-stuck-seconds action)
                       *body-movement-stuck-seconds*)
                (if (< (incf (body-move-action-replans action)) 3)
                    (replan-body-move-action action world)
                    (finish-body-move-action
                     action :failed "body remained blocked after replanning")))))))
    action))
