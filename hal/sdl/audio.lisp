;;;; Sound out, through SDL3.
;;;;
;;;; An AUDIO-SINK is one SDL audio stream bound to the default playback
;;;; device: the caller pushes interleaved single-float frames at the rate it
;;;; has them and SDL resamples, mixes, and paces them out to the hardware on
;;;; its own thread.  Everything here is thread-safe in SDL3, so a decoder
;;;; may feed the sink from wherever it runs while the render thread reads the
;;;; clock and turns the gain.
;;;;
;;;; The clock is the point.  How many frames have been handed over minus how
;;;; many SDL still holds is how many have gone out the speaker, and that --
;;;; not a wall clock -- is what a picture should be shown against, because
;;;; the ear notices a lip out of step long before the eye notices a picture
;;;; held a frame too long.

(in-package #:luv)

(cffi:defcstruct sdl-audio-spec
  (format :int)
  (channels :int)
  (frequency :int))

(cffi:defcfun ("SDL_InitSubSystem" %sdl-init-subsystem) :bool
  (flags :uint32))
(cffi:defcfun ("SDL_OpenAudioDeviceStream" %sdl-open-audio-device-stream)
    :pointer
  (device :uint32) (spec :pointer) (callback :pointer) (userdata :pointer))
(cffi:defcfun ("SDL_ResumeAudioStreamDevice" %sdl-resume-audio-stream-device)
    :bool
  (stream :pointer))
(cffi:defcfun ("SDL_PutAudioStreamData" %sdl-put-audio-stream-data) :bool
  (stream :pointer) (buffer :pointer) (length :int))
(cffi:defcfun ("SDL_GetAudioStreamQueued" %sdl-get-audio-stream-queued) :int
  (stream :pointer))
(cffi:defcfun ("SDL_ClearAudioStream" %sdl-clear-audio-stream) :bool
  (stream :pointer))
(cffi:defcfun ("SDL_SetAudioStreamGain" %sdl-set-audio-stream-gain) :bool
  (stream :pointer) (gain :float))
(cffi:defcfun ("SDL_DestroyAudioStream" %sdl-destroy-audio-stream) :void
  (stream :pointer))
(cffi:defcfun ("SDL_GetError" %sdl-audio-get-error) :string)

(defconstant +sdl-init-audio+ #x10)
(defconstant +sdl-audio-device-default-playback+ #xFFFFFFFF)
(defconstant +sdl-audio-f32+ #x8120
  "32-bit float, native (little) endian.")

(defclass audio-sink ()
  ((stream :initarg :stream :accessor audio-sink-stream)
   (rate :initarg :rate :reader audio-sink-rate)
   (channels :initarg :channels :reader audio-sink-channels)
   (frames-pushed :initform 0 :accessor audio-sink-frames-pushed
                  :documentation "Every frame ever handed to SDL.")
   ;; The interleaved staging buffer, grown to the largest push so far.
   (staging :initform nil :accessor audio-sink-staging)
   (staging-frames :initform 0 :accessor audio-sink-staging-frames))
  (:documentation "One playback stream on the default audio device."))

(defun open-audio-sink (&key (rate 48000) (channels 2))
  "Open a playback stream taking CHANNELS-channel float frames at RATE.

The device is opened at whatever it prefers; SDL converts.  Playback starts
at once, so a sink with nothing queued is one playing silence."
  (with-sdl-native-environment
    (unless (%sdl-init-subsystem +sdl-init-audio+)
      (error "SDL audio initialization failed: ~A" (%sdl-audio-get-error)))
    (cffi:with-foreign-object (spec '(:struct sdl-audio-spec))
      (setf (cffi:foreign-slot-value spec '(:struct sdl-audio-spec) 'format)
            +sdl-audio-f32+
            (cffi:foreign-slot-value spec '(:struct sdl-audio-spec) 'channels)
            channels
            (cffi:foreign-slot-value spec '(:struct sdl-audio-spec) 'frequency)
            rate)
      (let ((stream (%sdl-open-audio-device-stream
                     +sdl-audio-device-default-playback+ spec
                     (cffi:null-pointer) (cffi:null-pointer))))
        (when (cffi:null-pointer-p stream)
          (error "SDL could not open an audio stream: ~A"
                 (%sdl-audio-get-error)))
        (%sdl-resume-audio-stream-device stream)
        (make-instance 'audio-sink :stream stream
                                   :rate rate :channels channels)))))

(defun audio-sink-open-p (sink)
  (not (cffi:null-pointer-p (audio-sink-stream sink))))

(defun close-audio-sink (sink)
  "Destroy SINK's stream, dropping whatever was still queued.  Idempotent."
  (when (audio-sink-open-p sink)
    (%sdl-destroy-audio-stream (audio-sink-stream sink))
    (setf (audio-sink-stream sink) (cffi:null-pointer)))
  (when (audio-sink-staging sink)
    (cffi:foreign-free (audio-sink-staging sink))
    (setf (audio-sink-staging sink) nil
          (audio-sink-staging-frames sink) 0))
  sink)

(defun ensure-audio-sink-staging (sink frames)
  (when (< (audio-sink-staging-frames sink) frames)
    (when (audio-sink-staging sink)
      (cffi:foreign-free (audio-sink-staging sink)))
    (setf (audio-sink-staging sink)
          (cffi:foreign-alloc :float
                              :count (* frames (audio-sink-channels sink)))
          (audio-sink-staging-frames sink) frames))
  (audio-sink-staging sink))

(defun push-audio-sink-mono (sink samples count &key (left 1.0) (right 1.0))
  "Queue COUNT mono SAMPLES for SINK, placed between the channels by LEFT and
RIGHT.  A two-channel sink gets the sample times LEFT on the left and times
RIGHT on the right; any other channel count gets it times LEFT everywhere.

This is where a point source becomes a stereo image: the film's sound is
one signal, and its place in the room is decided per push, so a listener who
walks past the screen hears it cross over."
  (declare (type (simple-array single-float (*)) samples)
           (type fixnum count))
  (let* ((channels (audio-sink-channels sink))
         (staging (ensure-audio-sink-staging sink count))
         (left (coerce left 'single-float))
         (right (coerce right 'single-float)))
    (declare (type fixnum channels) (type single-float left right))
    (if (= channels 2)
        (dotimes (index count)
          (let ((sample (aref samples index)))
            (setf (cffi:mem-aref staging :float (* 2 index)) (* sample left)
                  (cffi:mem-aref staging :float (1+ (* 2 index)))
                  (* sample right))))
        (dotimes (index count)
          (let ((sample (* left (aref samples index))))
            (dotimes (channel channels)
              (setf (cffi:mem-aref staging :float (+ (* channels index) channel))
                    sample)))))
    (with-sdl-native-environment
      (unless (%sdl-put-audio-stream-data
               (audio-sink-stream sink) staging (* count channels 4))
        (error "SDL refused audio data: ~A" (%sdl-audio-get-error))))
    (incf (audio-sink-frames-pushed sink) count)
    sink))

(defun audio-sink-queued-frames (sink)
  "How many frames SINK is still holding, not yet out the speaker."
  (if (audio-sink-open-p sink)
      (floor (%sdl-get-audio-stream-queued (audio-sink-stream sink))
             (* 4 (audio-sink-channels sink)))
      0))

(defun audio-sink-played-frames (sink)
  "How many frames have gone out: pushed less still queued.  This is the
sink's clock, in frames at its own rate."
  (max 0 (- (audio-sink-frames-pushed sink) (audio-sink-queued-frames sink))))

(defun audio-sink-queued-seconds (sink)
  (/ (audio-sink-queued-frames sink) (float (audio-sink-rate sink) 1.0)))

(defun clear-audio-sink (sink)
  "Drop everything queued, so the next push is heard at once."
  (when (audio-sink-open-p sink)
    ;; What is dropped was never played; the clock must not count it.
    (let ((played (audio-sink-played-frames sink)))
      (%sdl-clear-audio-stream (audio-sink-stream sink))
      (setf (audio-sink-frames-pushed sink) played)))
  sink)

(defun (setf audio-sink-gain) (gain sink)
  "Set SINK's overall loudness, 0 silent, 1 as decoded."
  (when (audio-sink-open-p sink)
    (%sdl-set-audio-stream-gain (audio-sink-stream sink)
                                (coerce (max 0.0 gain) 'single-float)))
  gain)
