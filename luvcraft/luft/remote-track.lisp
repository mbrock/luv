;;;; A remote player is drawn a little behind time (#VM5TN6).
;;;;
;;;; Moppe's CompanionTrack, for a Luvcraft body: the player's recent pose
;;;; snapshots, played back a little behind their arrival so that there is
;;;; nearly always one on either side of the moment drawn.  Snapshots arrive
;;;; unevenly; the moment drawn moves on evenly with the frames and is only
;;;; steered by the arrivals.  Between snapshots the body travels on a curve
;;;; that passes each without a corner; past the newest it is carried on a
;;;; little way and slows to a stand; what late word corrects is eased; and a
;;;; leap too long to have been travelled is a teleport.  Nothing here knows
;;;; a socket or a clock: callers pass this machine's time.
(in-package #:luvcraft.luft.game)

(defconstant +simulation-ticks-per-second+ 120)

(defstruct (pose-snapshot (:copier nil))
  "How a player looked at SENT seconds of its sender's simulation clock: its
feet, velocity, look, walking phase, flight, and the block in hand."
  (sent 0d0 :type double-float)
  (position (vector 0d0 0d0 0d0) :type simple-vector)
  (velocity (vector 0d0 0d0 0d0) :type simple-vector)
  (yaw 0d0 :type double-float)
  (pitch 0d0 :type double-float)
  (gait 0d0 :type double-float)
  (flying nil)
  (held nil))

(defparameter *track-length* 32)
(defparameter *track-patience* 3d0
  "Seconds after a player's last word that it is taken to have gone.")
(defparameter *track-leap-cells* 16d0
  "Farther than this between two snapshots was not travelled: a teleport.")
(defparameter *track-carry-seconds* 0.2d0)
(defparameter *track-chase-gain* 2d0)
(defparameter *track-rate-gain* 0.5d0)
(defparameter *track-resync-seconds* 0.5d0)
(defparameter *track-offset-ease* 0.1d0)
(defparameter *track-gap-ease* 0.01d0)
(defparameter *track-usual-gap* 0.05d0)
(defparameter *track-longest-gap* 0.3d0)
(defparameter *track-slip-seconds* 0.15d0)
(defparameter *track-slip-cells* 3d0)

(defun mix (a b s) (+ a (* (- b a) s)))

(defun mix-vector (a b s)
  (vector (mix (svref a 0) (svref b 0) s) (mix (svref a 1) (svref b 1) s)
          (mix (svref a 2) (svref b 2) s)))

(defun mix-angle (a b s)
  "Between two angles, the short way round."
  (+ a (* s (let ((step (rem (- b a) (* 2 pi))))
              (cond ((> step pi) (- step (* 2 pi)))
                    ((< step (- pi)) (+ step (* 2 pi)))
                    (t step))))))

(defun distance-squared (a b)
  (loop for i below 3 sum (expt (- (svref a i) (svref b i)) 2)))

(defun leapt-p (a b)
  (> (distance-squared a b) (expt *track-leap-cells* 2)))

(defun hermite (p1 p2 m1 m2 s)
  "The curve from P1 (at zero) to P2 (at one) leaving with M1 and arriving with M2."
  (let* ((s2 (* s s)) (s3 (* s2 s))
         (h00 (+ (* 2 s3) (* -3 s2) 1)) (h10 (+ s3 (* -2 s2) s))
         (h01 (+ (* 3 s2) (* -2 s3))) (h11 (- s3 s2)))
    (map 'simple-vector (lambda (p1 p2 m1 m2) (+ (* h00 p1) (* h10 m1) (* h01 p2) (* h11 m2)))
         p1 p2 m1 m2)))

(defun travel (before a b after s)
  "Where the body is partway from A to B: on the curve that passes each the
way its neighbours lie, and beyond them, straight on."
  (let ((p1 (pose-snapshot-position a)) (p2 (pose-snapshot-position b))
        (span (- (pose-snapshot-sent b) (pose-snapshot-sent a))))
    (if (or (<= s 0) (>= s 1) (not (plusp span)))
        (mix-vector p1 p2 s)
        (let ((m1 (map 'simple-vector #'- p2 p1))
              (m2 (map 'simple-vector #'- p2 p1)))
          (when (and before (< (pose-snapshot-sent before) (pose-snapshot-sent a))
                     (not (leapt-p (pose-snapshot-position before) p1)))
            (let ((k (/ span (- (pose-snapshot-sent b) (pose-snapshot-sent before)))))
              (setf m1 (map 'simple-vector (lambda (p2 p0) (* k (- p2 p0)))
                            p2 (pose-snapshot-position before)))))
          (when (and after (> (pose-snapshot-sent after) (pose-snapshot-sent b))
                     (not (leapt-p p2 (pose-snapshot-position after))))
            (let ((k (/ span (- (pose-snapshot-sent after) (pose-snapshot-sent a)))))
              (setf m2 (map 'simple-vector (lambda (p3 p1) (* k (- p3 p1)))
                            (pose-snapshot-position after) p1))))
          (hermite p1 p2 m1 m2 s)))))

(defun blend-poses (before a b after s)
  "The look partway from A (at zero) to B (at one); S beyond one carries the
motion on.  What does not blend is B's, and so is all of it across a leap."
  (if (leapt-p (pose-snapshot-position a) (pose-snapshot-position b))
      b
      (let ((within (max 0d0 (min 1d0 s))))
        (make-pose-snapshot
         :sent (mix (pose-snapshot-sent a) (pose-snapshot-sent b) s)
         :position (travel before a b after s)
         :velocity (mix-vector (pose-snapshot-velocity a) (pose-snapshot-velocity b) within)
         :yaw (mix-angle (pose-snapshot-yaw a) (pose-snapshot-yaw b) within)
         :pitch (mix (pose-snapshot-pitch a) (pose-snapshot-pitch b) within)
         :gait (mix-angle (pose-snapshot-gait a) (pose-snapshot-gait b) s)
         :flying (pose-snapshot-flying b)
         :held (pose-snapshot-held b)))))

(defclass remote-track ()
  ((snapshots :initform (make-array 0 :adjustable t :fill-pointer 0) :reader track-snapshots)
   (offset :initform 0d0 :documentation "The sender's clock less this machine's, eased over arrivals.")
   (heard :initform -1d9 :reader track-heard)
   (gap :initform *track-usual-gap* :documentation "The longest wait between arrivals of late.")
   (fresh :initform 0 :documentation "How many snapshots have come since the last advance.")
   (playing :initform nil)
   (play :initform 0d0 :reader track-play
         :documentation "The moment drawn, on the sender's clock.")
   (rate :initform 1d0 :documentation "How fast the sender's clock runs against this one.")
   (slip :initform (vector 0d0 0d0 0d0)
         :documentation "How far the body is still drawn from where the snapshots put it.")
   (shown :initform nil :accessor track-shown :documentation "The pose last drawn.")
   (previous :initform nil :accessor track-previous :documentation "The pose drawn before it."))
  (:documentation "A remote player's recent snapshots and the moment they are drawn at."))

(defun make-remote-track () (make-instance 'remote-track))

(defun clear-remote-track (track)
  (with-slots (snapshots heard gap fresh playing rate slip) track
    (setf (fill-pointer snapshots) 0 heard -1d9 gap *track-usual-gap* fresh 0
          playing nil rate 1d0 slip (vector 0d0 0d0 0d0))))

(defun track-present-p (track now)
  "Whether the player has been heard from lately."
  (and (plusp (length (track-snapshots track)))
       (< (- now (track-heard track)) *track-patience*)))

(defun track-delay (track)
  "How far in the past the player is drawn: enough to cover the longest wait
between arrivals of late, between 0.05 and 0.3 seconds."
  (max 0.05d0 (min 0.3d0 (+ (* 1.5d0 (slot-value track 'gap)) 0.01d0))))

(defun push-pose-snapshot (track snapshot now)
  "Take SNAPSHOT, which arrived at NOW on this machine's clock; one no newer
than the newest held is dropped."
  (with-slots (snapshots offset heard gap fresh) track
    (unless (track-present-p track now) (clear-remote-track track))
    (let ((arrival (- (pose-snapshot-sent snapshot) now)))
      (cond
        ((zerop (length snapshots)) (setf offset arrival))
        ((<= (pose-snapshot-sent snapshot)
             (pose-snapshot-sent (aref snapshots (1- (length snapshots)))))
         (return-from push-pose-snapshot nil))
        (t
         ;; No one arrival is believed much, unless the clocks have parted.
         (if (> (abs (- arrival offset)) *track-resync-seconds*)
             (setf offset arrival)
             (incf offset (* (- arrival offset) *track-offset-ease*)))
         (let ((wait (min (- now heard) *track-longest-gap*)))
           (cond ((> wait gap) (setf gap wait))
                 ((> wait 1d-4) (incf gap (* (- wait gap) *track-gap-ease*))))))))
    (setf heard now)
    (vector-push-extend snapshot snapshots)
    (incf fresh)
    (when (> (length snapshots) *track-length*)
      (replace snapshots snapshots :start2 1)
      (decf (fill-pointer snapshots)))
    snapshot))

(defun track-look (track count at)
  "How the player looked at AT on its clock, by the first COUNT snapshots held."
  (let ((snapshots (track-snapshots track)))
    (if (or (= count 1) (<= at (pose-snapshot-sent (aref snapshots 0))))
        (aref snapshots 0)
        (or (loop for i from 1 below count
                  for a = (aref snapshots (1- i)) for b = (aref snapshots i)
                  unless (> at (pose-snapshot-sent b))
                    return (blend-poses (and (>= i 2) (aref snapshots (- i 2))) a b
                                        (and (< (1+ i) count) (aref snapshots (1+ i)))
                                        (/ (- at (pose-snapshot-sent a))
                                           (- (pose-snapshot-sent b) (pose-snapshot-sent a)))))
            ;; Past the newest: on the way the last two were going, slowing to a stand.
            (let* ((a (aref snapshots (- count 2))) (b (aref snapshots (1- count)))
                   (reach (* *track-carry-seconds*
                             (- 1 (exp (- (/ (- at (pose-snapshot-sent b)) *track-carry-seconds*)))))))
              (blend-poses nil a b nil
                           (+ 1 (/ reach (- (pose-snapshot-sent b) (pose-snapshot-sent a))))))))))

(defun advance-remote-track (track now elapsed)
  "Move the moment drawn on by ELAPSED seconds and return the pose then, or NIL
once the player has gone.  The previous pose drawn stays in TRACK-PREVIOUS."
  (with-slots (snapshots offset fresh playing play rate slip) track
    (unless (track-present-p track now)
      (setf playing nil (track-shown track) nil (track-previous track) nil)
      (return-from advance-remote-track nil))
    (let* ((elapsed (max 0d0 (min 0.25d0 (coerce elapsed 'double-float))))
           (target (- (+ now offset) (track-delay track))))
      (incf play (* elapsed rate))
      (let ((ahead (- target play)))
        (if (or (not playing) (> (abs ahead) *track-resync-seconds*))
            (setf playing t play target rate 1d0 slip (vector 0d0 0d0 0d0))
            (setf play (+ play (max (* -0.5d0 elapsed)
                                    (min (* 0.5d0 elapsed)
                                         (* ahead (- 1 (exp (- (* *track-chase-gain* elapsed))))))))
                  rate (max 0.5d0 (min 2d0 (+ rate (* ahead *track-rate-gain* elapsed)))))))
      (let ((seen (track-look track (length snapshots) play)))
        ;; Word that came late shows where the player really was while this
        ;; machine was guessing: keep the difference as a dwindling slip.
        (when (< 0 fresh (length snapshots))
          (let ((had (- (length snapshots) fresh)))
            (when (> play (pose-snapshot-sent (aref snapshots (1- had))))
              (let ((guessed (track-look track had play)))
                (unless (leapt-p (pose-snapshot-position guessed) (pose-snapshot-position seen))
                  (setf slip (map 'simple-vector #'+ slip
                                  (map 'simple-vector #'- (pose-snapshot-position guessed)
                                       (pose-snapshot-position seen)))))))))
        (setf fresh 0)
        (let ((keep (exp (- (/ elapsed *track-slip-seconds*)))))
          (setf slip (if (> (loop for x across slip sum (* x x)) (expt *track-slip-cells* 2))
                         (vector 0d0 0d0 0d0)
                         (map 'simple-vector (lambda (x) (* x keep)) slip))))
        (let ((drawn (make-pose-snapshot
                      :sent (pose-snapshot-sent seen)
                      :position (map 'simple-vector #'+ (pose-snapshot-position seen) slip)
                      :velocity (pose-snapshot-velocity seen) :yaw (pose-snapshot-yaw seen)
                      :pitch (pose-snapshot-pitch seen) :gait (pose-snapshot-gait seen)
                      :flying (pose-snapshot-flying seen) :held (pose-snapshot-held seen))))
          (setf (track-previous track) (or (track-shown track) drawn)
                (track-shown track) drawn))))))
