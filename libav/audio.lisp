;;;; Opening a file's sound and pulling samples out of it.
;;;;
;;;; An AUDIO-TRACK is the same pump as a VIDEO -- one demuxer, one decoder,
;;;; DECODE-NEXT-FRAME -- on the file's best audio stream, opened on its own
;;;; AVFormatContext so a player can run picture and sound on different
;;;; threads at different rates without either waiting for the other.
;;;;
;;;; What comes out is deliberately plain: mono single-floats.  A film on a
;;;; wall is a point in the world; where it sits between the listener's ears
;;;; is the player's business, not the file's, so channels are folded here
;;;; and the sample rate is left as the file has it for the output device to
;;;; resample.  There is no swresample in this: interleaving and averaging a
;;;; few thousand floats a frame is not worth a fifth library.

(in-package #:luv.libav)

(defclass audio-track (stream-decoder)
  ((sample-rate :initarg :sample-rate :reader audio-sample-rate)
   (channel-count :initarg :channel-count :reader audio-channel-count)
   (duration :initarg :duration :initform nil :reader audio-duration
             :documentation "The container's length in seconds, or NIL."))
  (:documentation "An open file's sound and its decoder."))

(defun open-audio (pathname)
  "Open PATHNAME's best audio stream and start a decoder for it.

Returns an AUDIO-TRACK, or NIL when the file has no sound.  The caller owns
it and must CLOSE-AUDIO it."
  (load-libav)
  (let ((truename (uiop:native-namestring (truename pathname)))
        (format-context nil)
        (codec-context nil)
        (track nil))
    (unwind-protect
         (with-libav-native-environment
           (cffi:with-foreign-objects ((context-cell :pointer)
                                       (decoder-cell :pointer))
             (setf (cffi:mem-ref context-cell :pointer) (cffi:null-pointer))
             (check-code (%avformat-open-input
                          context-cell truename
                          (cffi:null-pointer) (cffi:null-pointer))
                         'open-audio)
             (setf format-context (cffi:mem-ref context-cell :pointer))
             (check-code (%avformat-find-stream-info
                          format-context (cffi:null-pointer))
                         'open-audio)
             (setf (cffi:mem-ref decoder-cell :pointer) (cffi:null-pointer))
             (let ((index (%av-find-best-stream
                           format-context
                           (cffi:foreign-enum-value 'media-type :audio)
                           -1 -1 decoder-cell 0)))
               (when (minusp index)
                 ;; A silent film is not an error.
                 (return-from open-audio nil))
               (let* ((decoder (cffi:mem-ref decoder-cell :pointer))
                      (stream (stream-pointer format-context index))
                      (parameters
                        (cffi:foreign-slot-value stream '(:struct av-stream)
                                                 'codec-parameters)))
                 (when (cffi:null-pointer-p decoder)
                   (error "No decoder is available for the audio stream in ~A."
                          pathname))
                 (setf codec-context (%avcodec-alloc-context3 decoder))
                 (when (cffi:null-pointer-p codec-context)
                   (error "Could not allocate a decoder context for ~A."
                          pathname))
                 (check-code (%avcodec-parameters-to-context
                              codec-context parameters)
                             'open-audio)
                 (check-code (%avcodec-open2 codec-context decoder
                                             (cffi:null-pointer))
                             'open-audio)
                 (let ((duration
                         (cffi:foreign-slot-value
                          format-context '(:struct av-format-context)
                          'duration)))
                   (setf track
                         (make-instance
                          'audio-track
                          :pathname pathname
                          :format-context format-context
                          :codec-context codec-context
                          :stream-index index
                          :sample-rate (cffi:foreign-slot-value
                                        codec-context
                                        '(:struct av-codec-context)
                                        'sample-rate)
                          :channel-count
                          (cffi:foreign-slot-value
                           (cffi:foreign-slot-pointer
                            codec-context '(:struct av-codec-context)
                            'channel-layout)
                           '(:struct av-channel-layout) 'channel-count)
                          ;; AV_TIME_BASE is a million.
                          :duration (and (plusp duration)
                                         (/ duration 1000000))
                          :frame (make-frame)
                          :packet (%av-packet-alloc))))
                 (setf format-context nil codec-context nil)
                 track))))
      (progn
        (when codec-context
          (cffi:with-foreign-object (cell :pointer)
            (setf (cffi:mem-ref cell :pointer) codec-context)
            (%avcodec-free-context cell)))
        (when format-context
          (cffi:with-foreign-object (cell :pointer)
            (setf (cffi:mem-ref cell :pointer) format-context)
            (%avformat-close-input cell)))))))

(defun close-audio (track)
  "Release TRACK's decoder, demuxer, and frame.  Idempotent."
  (close-video track))

(defun rewind-audio (track)
  "Seek TRACK back to its start and reset the decoder."
  (rewind-video track))

(defun decode-next-audio-frame (track)
  "Decode until TRACK's frame holds the next run of samples, or NIL at the
end of the file."
  (decode-next-frame track))

(defun audio-frame-sample-count (track)
  "How many samples per channel TRACK's current frame holds."
  (cffi:foreign-slot-value (frame-pointer (video-frame track))
                           '(:struct av-frame) 'sample-count))

(defun audio-frame-mono-samples (track &optional buffer)
  "Return TRACK's current frame folded to mono single-floats, and how many.

BUFFER is reused when it is large enough; otherwise a fresh one is made.  The
count is the number of samples written, which is what matters, since the
buffer may be longer.  Every sample format a decoder hands out is read here
directly -- planar or interleaved, 8/16/32-bit integer, float, or double --
because that is a small table and a resampler is a large dependency."
  (let* ((frame (frame-pointer (video-frame track)))
         (count (cffi:foreign-slot-value frame '(:struct av-frame)
                                         'sample-count))
         (channels
           (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer frame '(:struct av-frame)
                                       'channel-layout)
            '(:struct av-channel-layout) 'channel-count))
         (format (cffi:foreign-slot-value frame '(:struct av-frame) 'format))
         (planes (cffi:foreign-slot-value frame '(:struct av-frame)
                                          'extended-data))
         (buffer (if (and buffer (>= (length buffer) count))
                     buffer
                     (make-array (max count 1024) :element-type 'single-float)))
         (scale (/ 1.0 (max 1 channels))))
    (declare (type (simple-array single-float (*)) buffer)
             (type fixnum count channels))
    (flet ((planar-p (keyword)
             (member keyword '(:u8p :s16p :s32p :fltp :dblp))))
      (let* ((keyword (cffi:foreign-enum-keyword 'sample-format format
                                                 :errorp nil))
             (planar (planar-p keyword))
             (bytes (ecase keyword
                      ((:u8 :u8p) 1)
                      ((:s16 :s16p) 2)
                      ((:s32 :s32p :flt :fltp) 4)
                      ((:dbl :dblp) 8))))
        (macrolet ((fold (reader convert)
                     ;; Sum the channels of each sample; planar data is one
                     ;; pointer per channel, interleaved is one pointer with
                     ;; the channels side by side.
                     `(dotimes (sample count)
                        (let ((sum 0.0))
                          (declare (type single-float sum))
                          (dotimes (channel channels)
                            (let ((pointer
                                    (if planar
                                        (cffi:inc-pointer
                                         (cffi:mem-aref planes :pointer channel)
                                         (* sample bytes))
                                        (cffi:inc-pointer
                                         (cffi:mem-aref planes :pointer 0)
                                         (* (+ (* sample channels) channel)
                                            bytes)))))
                              (incf sum (,convert (cffi:mem-ref pointer ,reader)))))
                          (setf (aref buffer sample) (* sum scale))))))
          (ecase keyword
            ((:u8 :u8p)
             (fold :uint8 (lambda (v) (/ (- v 128) 128.0))))
            ((:s16 :s16p)
             (fold :int16 (lambda (v) (/ v 32768.0))))
            ((:s32 :s32p)
             (fold :int32 (lambda (v) (/ v 2147483648.0))))
            ((:flt :fltp)
             (fold :float (lambda (v) v)))
            ((:dbl :dblp)
             (fold :double (lambda (v) (coerce v 'single-float))))))))
    (values buffer count)))
