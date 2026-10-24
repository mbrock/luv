;;; Holding a frame rate by drawing the scene smaller.
;;;
;;; Two pieces any renderer can share: a stopwatch that reads how long the
;;; GPU spent on one submitted frame without blocking the canvas thread, and
;;; the controller Moppe settled on, which turns those times into the share
;;; of its ordinary width and height a scene should be drawn at.

(in-package #:luv)

(defun start-gpu-work-stopwatch (queue sink)
  "Start a stopwatch on the frame about to be recorded for QUEUE.

Returns a function to call once the frame is submitted; SINK is then called,
on a helper thread, with the frame's GPU seconds.  Returns NIL when QUEUE
cannot report its progress cheaply.

Where QUEUE has GPU timestamps (QUEUE-GPU-TIME-READER), the time is the sum
of the GPU's busy time over the frame's submissions.  Anything measured from
the CPU instead also counts waiting: for drawables, and, with several frames
in flight, for the frames ahead of this one.

Without timestamps the time runs from when the GPU could start the frame --
its first submission, or the previous frame's completion if that came later
-- to its work completing.  A frame may be submitted in parts: the Metal
canvas hands over the frame's work before it waits for a drawable, and only
the final copy into that drawable is submitted after.  That copy waits on the
GPU for the drawable too, which is presentation, not work, so a split frame's
clock stops at its next-to-last submission.

A helper thread polls the queue's counters without its lock, so the canvas
thread keeps its beat; waiting there instead made every sampled frame miss
the next one."
  (let ((progress (queue-progress-reader queue))
        (timer (queue-gpu-time-reader queue))
        (target nil))
    (when progress
      (multiple-value-bind (before completed) (funcall progress)
        (let ((start (get-internal-real-time)))
          (labels ((now () (get-internal-real-time))
                   (busy (last)
                     ;; NIL until every submission's feedback has arrived.
                     (loop for value from (1+ before) to last
                           for seconds = (funcall timer value)
                           unless seconds do (return nil)
                           sum seconds)))
            (sb-thread:make-thread
             (lambda ()
               (let ((submitted-at nil)
                     (previous-done-at (and (>= completed before) start)))
                 (loop repeat 2000
                       do (multiple-value-bind (submitted done)
                              (funcall progress)
                            (when (and (null submitted-at) (> submitted before))
                              (setf submitted-at (now)))
                            (when (and (null previous-done-at) (>= done before))
                              (setf previous-done-at (now)))
                            (let ((last target))
                              (when (and last submitted-at (>= done last))
                                (if timer
                                    (let ((seconds (busy last)))
                                      (when seconds
                                        (funcall sink seconds)
                                        (return)))
                                    (progn
                                      (funcall sink
                                               (/ (- (now)
                                                     (max submitted-at
                                                          (or previous-done-at start)))
                                                  (float internal-time-units-per-second
                                                         1d0)))
                                      (return))))))
                          (sleep 0.0005))))
             :name "GPU work stopwatch")
            (lambda ()
              (let ((submitted (funcall progress)))
                (setf target (if (and (null timer) (>= (- submitted before) 2))
                                 (1- submitted)
                                 submitted))))))))))

;;; A frame's cost follows the scene's pixels only in part: passes at the
;;; output's size, shadows, and vertex work stay.  So nothing is modelled.
;;; Every few samples the mean GPU time is compared with the budget, a little
;;; under the frame's period; over it, or under a quiet band below it, the
;;; pixel count is scaled by the ratio of where the time should rest to where
;;; it is -- sharply down, gently up -- until the time comes to rest in the
;;; band.  The band is wider than one step of share changes, so a steady view
;;; settles on a step rather than hunting between two.  A changed share takes
;;; a few frames to show in the timings, which are passed over.

(defclass dynamic-resolution ()
  ((steps :initarg :steps :initform 32 :reader dynamic-resolution-steps
          :documentation "The share moves in steps of one in this many.")
   (fewest :initarg :fewest :initform 16 :reader dynamic-resolution-fewest
           :documentation "The fewest steps ever drawn: half by default.")
   (window :initarg :window :initform 8 :reader dynamic-resolution-window
           :documentation "Samples averaged before the share may change.")
   (in-flight :initarg :in-flight :initform 4
              :reader dynamic-resolution-in-flight
              :documentation "Samples passed over after a change.")
   (headroom :initarg :headroom :initform 0.90
             :reader dynamic-resolution-headroom
             :documentation "The GPU's part of a frame's period.")
   (quiet :initarg :quiet :initform 0.85 :reader dynamic-resolution-quiet
          :documentation "The bottom of the quiet band, as part of the budget.")
   (rest-fraction :initarg :rest :initform 0.925 :reader dynamic-resolution-rest
         :documentation "Where the time is steered to, as part of the budget.")
   (fall :initarg :fall :initform 0.70 :reader dynamic-resolution-fall
         :documentation "The most the pixel count shrinks at once.")
   (rise :initarg :rise :initform 1.10 :reader dynamic-resolution-rise
         :documentation "The most the pixel count grows at once.")
   (growth-windows :initarg :growth-windows :initform 1
                   :reader dynamic-resolution-growth-windows
                   :documentation "Consecutive quiet windows required before growth.")
   (quiet-windows :initform 0 :accessor %dynamic-resolution-quiet-windows)
   (current-step :initform nil :accessor %dynamic-resolution-step)
   (pixels :initform 1.0 :accessor %dynamic-resolution-pixels)
   (sample-sum :initform 0d0 :accessor %dynamic-resolution-sum)
   (sample-count :initform 0 :accessor %dynamic-resolution-count)
   (passing :initform 0 :accessor %dynamic-resolution-passing))
  (:documentation
   "Moppe's controller holding a frame rate by shedding scene resolution."))

(defun make-dynamic-resolution (&rest initargs)
  (apply #'make-instance 'dynamic-resolution initargs))

(defun dynamic-resolution-share (controller)
  "The share of its ordinary width and height the scene should be drawn at."
  (/ (or (%dynamic-resolution-step controller)
         (dynamic-resolution-steps controller))
     (float (dynamic-resolution-steps controller))))

(defun dynamic-resolution-floor-p (controller)
  "Whether CONTROLLER already draws the scene as small as it ever will."
  (<= (or (%dynamic-resolution-step controller)
          (dynamic-resolution-steps controller))
      (dynamic-resolution-fewest controller)))

(defun reset-dynamic-resolution (controller)
  "Return CONTROLLER to the whole scene with no evidence gathered."
  (setf (%dynamic-resolution-quiet-windows controller) 0
        (%dynamic-resolution-step controller) nil
        (%dynamic-resolution-pixels controller) 1.0
        (%dynamic-resolution-sum controller) 0d0
        (%dynamic-resolution-count controller) 0
        (%dynamic-resolution-passing controller) 0)
  controller)

(defun observe-dynamic-resolution (controller gpu-seconds period)
  "Take one frame's GPU-SECONDS where frames are due every PERIOD seconds.

Returns true when the share changed."
  (when (and (realp gpu-seconds) (plusp gpu-seconds)
             (realp period) (plusp period))
    (when (plusp (%dynamic-resolution-passing controller))
      (decf (%dynamic-resolution-passing controller))
      (return-from observe-dynamic-resolution nil))
    (incf (%dynamic-resolution-sum controller) gpu-seconds)
    (when (< (incf (%dynamic-resolution-count controller))
             (dynamic-resolution-window controller))
      (return-from observe-dynamic-resolution nil))
    (let* ((mean (/ (%dynamic-resolution-sum controller)
                    (%dynamic-resolution-count controller)))
           (budget (* period (dynamic-resolution-headroom controller)))
           (toward (/ (* budget (dynamic-resolution-rest controller)) mean))
           (steps (dynamic-resolution-steps controller))
           (fewest (dynamic-resolution-fewest controller)))
      (setf (%dynamic-resolution-sum controller) 0d0
            (%dynamic-resolution-count controller) 0)
      (cond ((> mean budget)
             (setf (%dynamic-resolution-quiet-windows controller) 0)
             (setf (%dynamic-resolution-pixels controller)
                   (* (%dynamic-resolution-pixels controller)
                      (max toward (dynamic-resolution-fall controller)))))
            ((< mean (* budget (dynamic-resolution-quiet controller)))
             ;; Growth needs sustained spare time, not one quiet sample window.
             (when (< (incf (%dynamic-resolution-quiet-windows controller))
                      (dynamic-resolution-growth-windows controller))
               (return-from observe-dynamic-resolution nil))
             (setf (%dynamic-resolution-quiet-windows controller) 0)
             (setf (%dynamic-resolution-pixels controller)
                   (* (%dynamic-resolution-pixels controller)
                      (min toward (dynamic-resolution-rise controller)))))
            (t
             (setf (%dynamic-resolution-quiet-windows controller) 0)
             (return-from observe-dynamic-resolution nil)))
      (setf (%dynamic-resolution-pixels controller)
            (max (/ (* fewest fewest) (float (* steps steps)))
                 (min 1.0 (%dynamic-resolution-pixels controller))))
      (let ((step (max fewest
                       (min steps
                            (round (* (sqrt (%dynamic-resolution-pixels
                                             controller))
                                      steps))))))
        (unless (eql step (or (%dynamic-resolution-step controller) steps))
          (setf (%dynamic-resolution-step controller) step
                (%dynamic-resolution-passing controller)
                (dynamic-resolution-in-flight controller))
          t)))))
