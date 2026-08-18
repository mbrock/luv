;;; A film's sound: decoded on its own thread, heard from where the player
;;; stands, and the clock the picture keeps time by.
;;;
;;; The pump is one thread per playing film.  It keeps a quarter second of
;;; sound queued in an SDL audio sink and otherwise sleeps; every push places
;;; the mono signal between the two channels according to the pan the render
;;; thread last computed, and the sink's own gain carries the distance.  Two
;;; threads share three numbers -- gain, left, right -- each written whole
;;; and read whole, which is all the synchronisation a float needs.
;;;
;;; The clock: frames the sink has actually played, minus the frame count at
;;; the last rewind, over the rate.  That is the film's time as the ear
;;; hears it, and ADVANCE-VIDEO-SCREEN shows the picture that is due at it.

(in-package #:luvcraft)

(defparameter *film-sound-lead* 0.25
  "How many seconds of sound the pump keeps queued ahead of the speaker.

Enough that a busy frame or a garbage collection does not run the queue
dry, little enough that a pan or a gain change is heard within a blink.")

(defparameter *film-sound-reference-distance* 4.0
  "Cells from the screen at which the film is heard at full volume;
beyond it the sound falls off with the square of the distance.")

(defclass film-sound ()
  ((track :initarg :track :reader film-sound-track)
   (sink :initarg :sink :reader film-sound-sink)
   (thread :initform nil :accessor film-sound-thread)
   (running-p :initform t :accessor film-sound-running-p)
   (loop-p :initarg :loop-p :initform t :reader film-sound-loop-p)
   (finished-p :initform nil :accessor film-sound-finished-p
               :documentation "True once a film that does not loop has all
been handed to the sink.")
   ;; The sink's frame count when the current pass through the film began.
   (loop-start :initform 0 :accessor film-sound-loop-start)
   (passes :initform 0 :accessor film-sound-passes
           :documentation "How many times the film has started over.")
   ;; Written by the render thread, read by the pump.
   (gain :initform 1.0 :accessor film-sound-gain)
   (left :initform 1.0 :accessor film-sound-left)
   (right :initform 1.0 :accessor film-sound-right)
   (buffer :initform nil :accessor film-sound-buffer))
  (:documentation "One film's soundtrack, playing."))

(defun open-film-sound (pathname &key (loop-p t))
  "Open PATHNAME's soundtrack into a sink and start pumping it, or return
NIL for a silent film."
  (let ((track (libav:open-audio pathname)))
    (when track
      (let ((sink nil) (sound nil))
        (handler-case
            (progn
              (setf sink (luv:open-audio-sink
                          :rate (libav:audio-sample-rate track) :channels 2)
                    sound (make-instance 'film-sound :track track :sink sink
                                                     :loop-p loop-p))
              (setf (film-sound-thread sound)
                    (sb-thread:make-thread
                     (lambda () (run-film-sound sound))
                     :name (format nil "film sound ~A"
                                   (file-namestring pathname))))
              sound)
          (error (condition)
            (when sink (luv:close-audio-sink sink))
            (libav:close-audio track)
            (warn "The film's sound could not start; playing silent: ~A"
                  condition)
            nil))))))

(defun run-film-sound (sound)
  "The pump: keep the sink fed until told to stop."
  (let ((track (film-sound-track sound))
        (sink (film-sound-sink sound)))
    (handler-case
        (loop while (film-sound-running-p sound)
              do (cond ((film-sound-finished-p sound)
                        (sleep 0.05))
                       ((> (luv:audio-sink-queued-seconds sink)
                           *film-sound-lead*)
                        (sleep 0.01))
                       ((libav:decode-next-audio-frame track)
                        (multiple-value-bind (samples count)
                            (libav:audio-frame-mono-samples
                             track (film-sound-buffer sound))
                          (setf (film-sound-buffer sound) samples)
                          (luv:push-audio-sink-mono
                           sink samples count
                           :left (film-sound-left sound)
                           :right (film-sound-right sound))))
                       ((film-sound-loop-p sound)
                        (libav:rewind-audio track)
                        (setf (film-sound-loop-start sound)
                              (luv:audio-sink-frames-pushed sink))
                        (incf (film-sound-passes sound)))
                       (t
                        (setf (film-sound-finished-p sound) t))))
      (error (condition)
        (luv:log-event :luvcraft "film sound stopped: ~A" condition)))))

(defun close-film-sound (sound)
  "Stop the pump, then release the sink and the decoder."
  (setf (film-sound-running-p sound) nil)
  (alexandria:when-let ((thread (film-sound-thread sound)))
    (when (and (sb-thread:thread-alive-p thread)
               (not (eq thread sb-thread:*current-thread*)))
      (sb-thread:join-thread thread :default nil :timeout 2))
    (setf (film-sound-thread sound) nil))
  (luv:close-audio-sink (film-sound-sink sound))
  (libav:close-audio (film-sound-track sound))
  sound)

(defun film-sound-time (sound)
  "Seconds into the current pass of the film, as heard.

Negative for the moment after a rewind while the previous pass's tail is
still coming out of the speaker: the picture should hold its last frame
until this comes round to zero."
  (/ (- (luv:audio-sink-played-frames (film-sound-sink sound))
        (film-sound-loop-start sound))
     (float (luv:audio-sink-rate (film-sound-sink sound)) 1.0)))

(defun place-film-sound-listener (sound source listener right)
  "Hear SOUND, which comes from world point SOURCE, at LISTENER facing with
RIGHT as the ear-to-ear axis: set the gain by distance and the pan by
bearing.

Distance follows an inverse square past the reference distance -- a screen
in the next field is a murmur, at the wall it is the film.  Pan is constant
power over the bearing's sine, so walking past the screen swings it from
one ear to the other without a dip in the middle."
  (let* ((dx (- (vec3-x source) (vec3-x listener)))
         (dy (- (vec3-y source) (vec3-y listener)))
         (dz (- (vec3-z source) (vec3-z listener)))
         (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
         (reference *film-sound-reference-distance*)
         (gain (if (<= distance reference)
                   1.0
                   (/ (* reference reference) (* distance distance))))
         ;; The bearing's sine: how far to the right the source lies, from
         ;; -1 (hard left) to 1 (hard right).  A source at the listener has
         ;; no bearing and sits in the middle.
         (side (if (< distance 1e-3)
                   0.0
                   (/ (+ (* dx (vec3-x right)) (* dy (vec3-y right))
                         (* dz (vec3-z right)))
                      distance)))
         (angle (* (/ pi 4) (+ 1.0 side))))
    (setf (film-sound-gain sound) (coerce gain 'single-float)
          (film-sound-left sound) (coerce (cos angle) 'single-float)
          (film-sound-right sound) (coerce (sin angle) 'single-float)
          (luv:audio-sink-gain (film-sound-sink sound)) gain)
    sound))
