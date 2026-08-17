;;;; Opening a video file and pulling RGBA pictures out of it.
;;;;
;;;; The shape of FFmpeg's decode loop is not obvious from its names, so it is
;;;; worth stating once.  Demuxing and decoding are separate rates: one packet
;;;; may produce no frames, or several.  So the loop is a pump with two ends --
;;;; avcodec_send_packet feeds it, avcodec_receive_frame drains it -- and each
;;;; end may answer EAGAIN meaning "I want the other end first".  At the file's
;;;; end the codec is flushed with a null packet, after which it keeps handing
;;;; back the frames it had buffered until it finally says EOF.
;;;;
;;;; This is the software path: the decoder writes planes into ordinary memory
;;;; and swscale converts them to RGBA.  It exists to get pictures onto a
;;;; surface at all.  The hardware path -- a VideoToolbox, VAAPI, or Vulkan
;;;; frame whose data never touches the CPU -- reuses everything here except
;;;; the last step, and is the reason the AVFrame binding knows what
;;;; FRAME-HARDWARE-P means.

(in-package #:luv.libav)

(defclass video ()
  ((pathname :initarg :pathname :reader video-pathname)
   (format-context :initarg :format-context :accessor video-format-context)
   (codec-context :initarg :codec-context :accessor video-codec-context)
   (stream-index :initarg :stream-index :reader video-stream-index)
   (width :initarg :width :reader video-width)
   (height :initarg :height :reader video-height)
   (frame-rate :initarg :frame-rate :reader video-frame-rate)
   ;; The decoder's own output, reused across every picture in the file.
   (frame :initarg :frame :reader video-frame)
   (packet :initarg :packet :accessor video-packet)
   ;; Lazily built, because its size depends on what the caller asks for.
   (scaler :initform nil :accessor video-scaler)
   (scaler-key :initform nil :accessor video-scaler-key)
   ;; The RGBA staging buffer swscale writes into, kept across frames so a
   ;; player is not allocating megabytes per picture.
   (staging :initform nil :accessor video-staging)
   (staging-size :initform 0 :accessor video-staging-size)
   (drained-p :initform nil :accessor video-drained-p))
  (:documentation "An open video file and its decoder."))

;;; Demuxing.

(cffi:defcfun ("avformat_open_input" %avformat-open-input) :int
  (context :pointer) (url :string) (format :pointer) (options :pointer))
(cffi:defcfun ("avformat_close_input" %avformat-close-input) :void
  (context :pointer))
(cffi:defcfun ("avformat_find_stream_info" %avformat-find-stream-info) :int
  (context :pointer) (options :pointer))
(cffi:defcfun ("av_find_best_stream" %av-find-best-stream) :int
  (context :pointer) (type :int) (wanted :int) (related :int)
  (decoder :pointer) (flags :int))
(cffi:defcfun ("av_read_frame" %av-read-frame) :int
  (context :pointer) (packet :pointer))
(cffi:defcfun ("av_seek_frame" %av-seek-frame) :int
  (context :pointer) (stream :int) (timestamp :int64) (flags :int))

;;; Decoding.

(cffi:defcfun ("avcodec_alloc_context3" %avcodec-alloc-context3) :pointer
  (codec :pointer))
(cffi:defcfun ("avcodec_free_context" %avcodec-free-context) :void
  (context :pointer))
(cffi:defcfun ("avcodec_parameters_to_context"
               %avcodec-parameters-to-context)
    :int
  (context :pointer) (parameters :pointer))
(cffi:defcfun ("avcodec_open2" %avcodec-open2) :int
  (context :pointer) (codec :pointer) (options :pointer))
(cffi:defcfun ("avcodec_flush_buffers" %avcodec-flush-buffers) :void
  (context :pointer))
(cffi:defcfun ("avcodec_send_packet" %avcodec-send-packet) :int
  (context :pointer) (packet :pointer))
(cffi:defcfun ("avcodec_receive_frame" %avcodec-receive-frame) :int
  (context :pointer) (frame :pointer))
(cffi:defcfun ("av_hwdevice_ctx_create" %av-hwdevice-context-create) :int
  (reference :pointer) (type :int) (device :pointer) (options :pointer)
  (flags :int))
(cffi:defcfun ("av_buffer_ref" %av-buffer-reference) :pointer
  (reference :pointer))
(cffi:defcfun ("av_buffer_unref" %av-buffer-unreference) :void
  (reference :pointer))

(cffi:defcfun ("av_packet_alloc" %av-packet-alloc) :pointer)
(cffi:defcfun ("av_packet_free" %av-packet-free) :void (packet :pointer))
(cffi:defcfun ("av_packet_unref" %av-packet-unref) :void (packet :pointer))

;;; Converting.

(cffi:defcfun ("sws_getContext" %sws-get-context) :pointer
  (source-width :int) (source-height :int) (source-format :int)
  (target-width :int) (target-height :int) (target-format :int)
  (flags :int) (source-filter :pointer) (target-filter :pointer)
  (parameters :pointer))
(cffi:defcfun ("sws_freeContext" %sws-free-context) :void (context :pointer))
(cffi:defcfun ("sws_scale" %sws-scale) :int
  (context :pointer)
  (source-planes :pointer) (source-pitches :pointer)
  (source-y :int) (source-height :int)
  (target-planes :pointer) (target-pitches :pointer))

(defun stream-pointer (format-context index)
  "Return the INDEXth AVStream of FORMAT-CONTEXT."
  (cffi:mem-aref
   (cffi:foreign-slot-value format-context '(:struct av-format-context)
                            'streams)
   :pointer index))

(defun rational-value (pointer type slot)
  "Return the AVRational in SLOT as a Lisp rational, or NIL when undefined."
  (let* ((rational (cffi:foreign-slot-pointer pointer type slot))
         (numerator (cffi:foreign-slot-value rational '(:struct av-rational)
                                             'numerator))
         (denominator (cffi:foreign-slot-value rational '(:struct av-rational)
                                               'denominator)))
    (unless (zerop denominator)
      (/ numerator denominator))))

(cffi:defcallback choose-videotoolbox-format :int
    ((context :pointer) (formats :pointer))
  (declare (ignore context))
  (let ((wanted (cffi:foreign-enum-value 'pixel-format :videotoolbox)))
    (loop for index from 0
          for format = (cffi:mem-aref formats :int index)
          until (= format (cffi:foreign-enum-value 'pixel-format :none))
          when (= format wanted) return format
          finally (return (cffi:mem-aref formats :int 0)))))

(defun enable-videotoolbox-decoding (codec-context)
  "Ask FFmpeg to decode into reference-counted CVPixelBuffers."
  #+darwin
  (cffi:with-foreign-object (reference-cell :pointer)
    (setf (cffi:mem-ref reference-cell :pointer) (cffi:null-pointer))
    (check-code
     (%av-hwdevice-context-create
      reference-cell
      (cffi:foreign-enum-value 'hardware-device-type :videotoolbox)
      (cffi:null-pointer) (cffi:null-pointer) 0)
     'enable-videotoolbox-decoding)
    (unwind-protect
         (let ((reference (%av-buffer-reference
                           (cffi:mem-ref reference-cell :pointer))))
           (when (cffi:null-pointer-p reference)
             (error "Could not retain FFmpeg's VideoToolbox device context."))
           (setf (cffi:foreign-slot-value
                  codec-context '(:struct av-codec-context)
                  'hardware-device-context)
                 reference
                 (cffi:foreign-slot-value
                  codec-context '(:struct av-codec-context) 'get-format)
                 (cffi:callback choose-videotoolbox-format)))
      (%av-buffer-unreference reference-cell)))
  #-darwin
  (declare (ignore codec-context))
  codec-context)

(defun open-video (pathname &key (hardware :auto))
  "Open PATHNAME, find its best video stream, and start a decoder for it.

Returns a VIDEO.  The caller owns it and must CLOSE-VIDEO it."
  (load-libav)
  (let ((truename (uiop:native-namestring (truename pathname)))
        (format-context nil)
        (codec-context nil)
        (video nil))
    (unwind-protect
         (with-libav-native-environment
           (cffi:with-foreign-objects ((context-cell :pointer)
                                       (decoder-cell :pointer))
             (setf (cffi:mem-ref context-cell :pointer) (cffi:null-pointer))
             (check-code (%avformat-open-input
                          context-cell truename
                          (cffi:null-pointer) (cffi:null-pointer))
                         'open-video)
             (setf format-context (cffi:mem-ref context-cell :pointer))
             (check-code (%avformat-find-stream-info
                          format-context (cffi:null-pointer))
                         'open-video)
             (setf (cffi:mem-ref decoder-cell :pointer) (cffi:null-pointer))
             (let* ((index
                      (check-code
                       (%av-find-best-stream
                        format-context
                        (cffi:foreign-enum-value 'media-type :video)
                        -1 -1 decoder-cell 0)
                       'open-video))
                    (decoder (cffi:mem-ref decoder-cell :pointer))
                    (stream (stream-pointer format-context index))
                    (parameters
                      (cffi:foreign-slot-value stream '(:struct av-stream)
                                               'codec-parameters)))
               (when (cffi:null-pointer-p decoder)
                 (error "No decoder is available for the video stream in ~A."
                        pathname))
               (setf codec-context (%avcodec-alloc-context3 decoder))
               (when (cffi:null-pointer-p codec-context)
                 (error "Could not allocate a decoder context for ~A." pathname))
               (check-code (%avcodec-parameters-to-context
                            codec-context parameters)
                           'open-video)
               (when (and #+darwin t #-darwin nil
                          (member hardware '(:auto :required)))
                 (handler-case (enable-videotoolbox-decoding codec-context)
                   (error (condition)
                     (when (eq hardware :required) (error condition))
                     (warn "VideoToolbox decode unavailable; using software: ~A"
                           condition))))
               (check-code (%avcodec-open2 codec-context decoder
                                           (cffi:null-pointer))
                           'open-video)
               (setf video
                     (make-instance
                      'video
                      :pathname pathname
                      :format-context format-context
                      :codec-context codec-context
                      :stream-index index
                      :width (cffi:foreign-slot-value
                              codec-context '(:struct av-codec-context) 'width)
                      :height (cffi:foreign-slot-value
                               codec-context '(:struct av-codec-context) 'height)
                      :frame-rate (rational-value stream '(:struct av-stream)
                                                  'average-frame-rate)
                      :frame (make-frame)
                      :packet (%av-packet-alloc)))
               (setf format-context nil codec-context nil)
               video)))
      ;; Only reached when something above signalled: release whatever of the
      ;; half-built decoder we had taken ownership of.
      (progn
        (when codec-context
          (cffi:with-foreign-object (cell :pointer)
            (setf (cffi:mem-ref cell :pointer) codec-context)
            (%avcodec-free-context cell)))
        (when format-context
          (cffi:with-foreign-object (cell :pointer)
            (setf (cffi:mem-ref cell :pointer) format-context)
            (%avformat-close-input cell)))))))

(defun video-open-p (video)
  (not (null (video-format-context video))))

(defun close-video (video)
  "Release VIDEO's decoder, demuxer, frame, and scaler.  Idempotent."
  ;; Teardown computes too: swscale and the demuxer both do float arithmetic
  ;; on the way out.  An unmasked trap here would signal from inside whatever
  ;; was releasing the video -- for luvcraft, the middle of closing the game --
  ;; and abandon everything that had not been released yet.
  (with-libav-native-environment
    (close-video-1 video))
  video)

(defun close-video-1 (video)
  (when (video-scaler video)
    (%sws-free-context (video-scaler video))
    (setf (video-scaler video) nil
          (video-scaler-key video) nil))
  (when (video-staging video)
    (cffi:foreign-free (video-staging video))
    (setf (video-staging video) nil
          (video-staging-size video) 0))
  (when (video-packet video)
    (cffi:with-foreign-object (cell :pointer)
      (setf (cffi:mem-ref cell :pointer) (video-packet video))
      (%av-packet-free cell))
    (setf (video-packet video) nil))
  (when (video-codec-context video)
    (cffi:with-foreign-object (cell :pointer)
      (setf (cffi:mem-ref cell :pointer) (video-codec-context video))
      (%avcodec-free-context cell))
    (setf (video-codec-context video) nil))
  (when (video-format-context video)
    (cffi:with-foreign-object (cell :pointer)
      (setf (cffi:mem-ref cell :pointer) (video-format-context video))
      (%avformat-close-input cell))
    (setf (video-format-context video) nil))
  (release-frame (video-frame video))
  video)

(defmacro with-video ((variable pathname) &body body)
  "Evaluate BODY with VARIABLE bound to PATHNAME's decoder, closing it after."
  `(let ((,variable (open-video ,pathname)))
     (unwind-protect (progn ,@body)
       (close-video ,variable))))

(defun error-again-p (code)
  (= code (- +eagain+)))

(defun decode-next-frame (video)
  "Decode until VIDEO's frame holds the next picture.  Return the frame or NIL.

NIL means the file is exhausted; VIDEO's frame is left holding the last
picture that was decoded."
  (unless (video-open-p video)
    (error "The video ~A has been closed." (video-pathname video)))
  (with-libav-native-environment
    (decode-next-frame-1 video)))

(defun decode-next-frame-1 (video)
  (let ((codec-context (video-codec-context video))
        (frame-pointer (frame-pointer (video-frame video)))
        (packet (video-packet video)))
    (loop
      ;; Drain first: the codec may still be holding pictures from packets it
      ;; was given earlier, and at end of file that is the only source left.
      (let ((code (%avcodec-receive-frame codec-context frame-pointer)))
        (cond ((zerop code) (return (video-frame video)))
              ((= code +error-eof+) (return nil))
              ((not (error-again-p code))
               (check-code code 'decode-next-frame))))
      (when (video-drained-p video)
        (return nil))
      ;; The codec wants more input.  Read packets until one belongs to our
      ;; stream, or the file ends and we flush with a null packet instead.
      (let ((code (loop for status = (%av-read-frame
                                      (video-format-context video) packet)
                        do (cond ((minusp status) (return status))
                                 ((= (cffi:foreign-slot-value
                                      packet '(:struct av-packet)
                                      'stream-index)
                                     (video-stream-index video))
                                  (return 0))
                                 (t (%av-packet-unref packet))))))
        (cond ((zerop code)
               (unwind-protect
                    (check-code (%avcodec-send-packet codec-context packet)
                                'decode-next-frame)
                 (%av-packet-unref packet)))
              (t
               ;; End of file, or a read error we treat as one.  A null packet
               ;; tells the codec to hand back everything it has buffered.
               (setf (video-drained-p video) t)
               (%avcodec-send-packet codec-context (cffi:null-pointer))))))))

(defun rewind-video (video)
  "Seek VIDEO back to its start and reset the decoder."
  (with-libav-native-environment
    (check-code (%av-seek-frame (video-format-context video)
                                (video-stream-index video) 0 +seek-backward+)
                'rewind-video)
    (%avcodec-flush-buffers (video-codec-context video)))
  (setf (video-drained-p video) nil)
  video)

;;; Converting a decoded picture to RGBA.
;;;
;;; The scaler is cached against the size it was built for, since building one
;;; per frame would discard the filter tables it exists to precompute.  A
;;; changed target size -- or a decoder that changed its mind about the source
;;; format mid-stream, which happens -- rebuilds it.

(defun ensure-video-scaler (video width height)
  (let* ((frame (video-frame video))
         (source-format (cffi:foreign-slot-value
                         (frame-pointer frame) '(:struct av-frame) 'format))
         (key (list source-format (frame-width frame) (frame-height frame)
                    width height)))
    (unless (and (video-scaler video) (equal key (video-scaler-key video)))
      (when (video-scaler video)
        (%sws-free-context (video-scaler video)))
      (let ((scaler (%sws-get-context
                     (frame-width frame) (frame-height frame) source-format
                     width height
                     (cffi:foreign-enum-value 'pixel-format :rgba)
                     (cffi:foreign-enum-value 'swscale-flags :bilinear)
                     (cffi:null-pointer) (cffi:null-pointer)
                     (cffi:null-pointer))))
        (when (cffi:null-pointer-p scaler)
          (error "Could not build a swscale context for ~Dx~D ~A to ~Dx~D RGBA."
                 (frame-width frame) (frame-height frame)
                 (pixel-format-name source-format) width height))
        (setf (video-scaler video) scaler
              (video-scaler-key video) key)))
    (video-scaler video)))

(defun ensure-video-staging (video size)
  "Return VIDEO's RGBA staging buffer, growing it to at least SIZE bytes."
  (when (< (video-staging-size video) size)
    (when (video-staging video)
      (cffi:foreign-free (video-staging video)))
    (setf (video-staging video) (cffi:foreign-alloc :uint8 :count size)
          (video-staging-size video) size))
  (video-staging video))

(defun scale-frame-into (video pointer width height pitch)
  "Convert VIDEO's current picture into POINTER as WIDTH by HEIGHT RGBA.

PITCH is the destination's byte stride, which may exceed WIDTH times four when
the caller is writing into a larger image."
  (with-libav-native-environment
    (scale-frame-into-1 video pointer width height pitch)))

(defun scale-frame-into-1 (video pointer width height pitch)
  (let ((scaler (ensure-video-scaler video width height))
        (frame (frame-pointer (video-frame video))))
    (cffi:with-foreign-objects ((planes :pointer 4) (pitches :int 4))
      (setf (cffi:mem-aref planes :pointer 0) pointer)
      (setf (cffi:mem-aref pitches :int 0) pitch)
      (dotimes (index 3)
        (setf (cffi:mem-aref planes :pointer (1+ index)) (cffi:null-pointer))
        (setf (cffi:mem-aref pitches :int (1+ index)) 0))
      (%sws-scale
       scaler
       (cffi:foreign-slot-pointer frame '(:struct av-frame) 'data)
       (cffi:foreign-slot-pointer frame '(:struct av-frame) 'pitches)
       0 (cffi:foreign-slot-value frame '(:struct av-frame) 'height)
       planes pitches))
    (values)))

(defun frame-rgba-words (video width height &key array (alpha 255))
  "Return VIDEO's current picture as a HEIGHT by WIDTH array of RGBA words.

Each word is red in its low byte through ALPHA in its high byte, which is the
packing luvcraft's block atlas uses.  ARRAY is filled and returned when given,
so a player can convert into the same array every frame.

ALPHA is written rather than taken from the conversion.  swscale leaves the
alpha of an RGBA target undefined when the source has no alpha of its own, and
in the block atlas that byte is not opacity at all -- it is the material's
surface height -- so the caller has to say what it means."
  (let ((words (or array
                   (make-array (list height width)
                               :element-type '(unsigned-byte 32))))
        (pixels (ensure-video-staging video (* width height 4))))
    (scale-frame-into video pixels width height (* width 4))
    (copy-rgba-words pixels words (* width height) (ash alpha 24))
    words))

(defun copy-rgba-words (pixels words count opacity)
  "Copy COUNT packed words out of foreign PIXELS into WORDS, forcing OPACITY.

This is a per-picture inner loop -- a modest 512-wide screen is two hundred
thousand words every time the film advances -- so it reads the foreign memory
through a system area pointer rather than one CFFI call per pixel."
  (declare (type (simple-array (unsigned-byte 32) (* *)) words)
           (type fixnum count)
           (type (unsigned-byte 32) opacity)
           (optimize (speed 3) (safety 0)))
  #+sbcl
  (let ((sap (sb-sys:int-sap (cffi:pointer-address pixels))))
    (dotimes (index count)
      (setf (row-major-aref words index)
            (logior (logand (sb-sys:sap-ref-32 sap (the fixnum (* 4 index)))
                            #x00ffffff)
                    opacity))))
  #-sbcl
  (dotimes (index count)
    (setf (row-major-aref words index)
          (logior (logand (cffi:mem-aref pixels :uint32 index) #x00ffffff)
                  opacity)))
  words)
