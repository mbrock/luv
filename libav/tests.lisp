(defpackage #:luv.libav.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:libav #:luv.libav)))

(in-package #:luv.libav.tests)

(define-test libav-loads-and-agrees-with-its-headers
  ;; LOAD-LIBAV signals LIBAV-VERSION-MISMATCH itself when the headers this
  ;; system groveled and the libraries it dlopens come from different builds,
  ;; so reaching a version string at all is the claim.
  (true (libav:load-libav))
  (true (libav:libav-loaded-p))
  (true (stringp (libav:libav-build)))
  (true (every (lambda (entry)
                 (destructuring-bind (name . triple) entry
                   (and (stringp name)
                        (= 3 (length triple))
                        (every #'integerp triple))))
               (libav:libav-versions))))

(define-test the-decoders-luv-cares-about-are-present
  (true (libav:decoder-available-p "h264"))
  (true (libav:decoder-available-p "hevc"))
  (true (libav:decoder-available-p "av1"))
  (true (not (libav:decoder-available-p "no-such-decoder"))))

(define-test hardware-decoding-is-configured-for-this-platform
  ;; The platform's own path must be there, since decoding to a GPU surface is
  ;; the reason this binding exists.  Which other types FFmpeg was built with
  ;; is not this system's business.
  (let ((types (libav:hardware-device-types)))
    (true (listp types))
    (true (every #'keywordp types))
    #+darwin
    (true (member :videotoolbox types))
    #+linux
    (true (intersection '(:vaapi :vulkan) types))
    (true (equal "videotoolbox"
                 (libav:hardware-device-type-name :videotoolbox)))))

(define-test pixel-formats-round-trip-through-ffmpegs-own-names
  (true (equal "nv12" (libav:pixel-format-name :nv12)))
  (true (equal "yuv420p" (libav:pixel-format-name :yuv420p)))
  (true (equal "videotoolbox_vld" (libav:pixel-format-name :videotoolbox))))

(define-test a-frames-groveled-layout-agrees-with-libavutil
  ;; The offsets are only trustworthy if the library reads back what Lisp
  ;; wrote through them.  Describe a picture through the groveled slots, then
  ;; let av_frame_get_buffer act on that description: a 1920-wide NV12 frame
  ;; has to come back with two planes and a stride of at least its width.
  (libav:with-frame (frame)
    (true (libav:frame-live-p frame))
    (setf (libav:frame-width frame) 1920
          (libav:frame-height frame) 1080
          (libav:frame-pixel-format frame) :nv12)
    (true (eq :nv12 (libav:frame-pixel-format frame)))
    (true (not (libav:frame-hardware-p frame)))
    (libav:allocate-frame-buffer frame)
    (true (>= (libav:frame-plane-pitch frame 0) 1920))
    (true (>= (libav:frame-plane-pitch frame 1) 1920))
    (true (not (cffi:null-pointer-p (libav:frame-plane-pointer frame 0))))
    (true (not (cffi:null-pointer-p (libav:frame-plane-pointer frame 1))))
    ;; NV12 interleaves chroma, so there is no third plane to find.
    (true (cffi:null-pointer-p (libav:frame-plane-pointer frame 2)))
    (libav:unreference-frame frame)
    (true (cffi:null-pointer-p (libav:frame-plane-pointer frame 0)))))

(define-test frame-ownership-is-explicit-and-idempotent
  (let ((frame (libav:make-frame)))
    (true (libav:frame-live-p frame))
    (libav:release-frame frame)
    (true (not (libav:frame-live-p frame)))
    (true (eq frame (libav:release-frame frame)))
    (fail (libav:frame-width frame) 'error)))

(define-test cloned-frames-retain-the-picture-independently
  (libav:with-frame (frame)
    (setf (libav:frame-width frame) 8
          (libav:frame-height frame) 8
          (libav:frame-pixel-format frame) :rgba)
    (libav:allocate-frame-buffer frame)
    (let ((clone (libav:clone-frame frame)))
      (unwind-protect
           (progn
             (libav:unreference-frame frame)
             (true (not (cffi:null-pointer-p
                         (libav:frame-plane-pointer clone 0)))))
        (libav:release-frame clone)))))

(defparameter *test-pattern*
  (asdf:system-relative-pathname "luv/libav" "libav/test-pattern.mp4")
  "A ten-frame 64x48 H.264 test pattern, small enough to keep in the tree.")

(define-test a-file-opens-and-describes-itself
  (libav:with-video (video *test-pattern*)
    (true (libav:video-open-p video))
    (true (= 64 (libav:video-width video)))
    (true (= 48 (libav:video-height video)))
    (true (= 10 (libav:video-frame-rate video)))))

(define-test decoding-drains-every-picture-in-the-file
  ;; The count is the claim: a decode loop that forgets to flush the codec at
  ;; end of file loses the pictures still buffered inside it, and a loop that
  ;; mishandles EAGAIN stops early.  Ten frames in, ten frames out.
  (libav:with-video (video *test-pattern*)
    (let ((count 0))
      (loop while (libav:decode-next-frame video) do (incf count))
      (true (= 10 count))
      ;; Exhausted stays exhausted.
      (true (null (libav:decode-next-frame video)))
      ;; And rewinding makes it whole again.
      (libav:rewind-video video)
      (let ((again 0))
        (loop while (libav:decode-next-frame video) do (incf again))
        (true (= 10 again))))))

(define-test a-decoded-picture-converts-to-packed-rgba
  (libav:with-video (video *test-pattern*)
    (true (libav:decode-next-frame video))
    (let ((frame (libav:video-frame video)))
      (true (= 64 (libav:frame-width frame)))
      (true (= 48 (libav:frame-height frame)))
      ;; Software decoding, so the picture is in ordinary memory and its
      ;; planes are real.
      (true (not (libav:frame-hardware-p frame)))
      (true (eq :yuv420p (libav:frame-pixel-format frame))))
    ;; swscale both converts and resizes, so a caller can ask for the size its
    ;; surface wants rather than the size the file happens to be.
    (let ((words (libav:frame-rgba-words video 16 16)))
      (true (equal '(16 16) (array-dimensions words)))
      (true (loop for index below (array-total-size words)
                  always (= #xff (ldb (byte 8 24) (row-major-aref words index)))))
      ;; A test pattern is not one flat colour; a conversion that silently
      ;; produced an empty image would be.
      (let ((distinct (make-hash-table)))
        (dotimes (y 16)
          (dotimes (x 16)
            (setf (gethash (aref words y x) distinct) t)))
        (true (> (hash-table-count distinct) 8))))))

(define-test a-failed-call-carries-ffmpegs-own-explanation
  (libav:with-frame (frame)
    ;; No width, height, or format: av_frame_get_buffer has nothing to size.
    (handler-case (progn (libav:allocate-frame-buffer frame) (false t "expected a failure"))
      (libav:libav-error (condition)
        (true (eq 'libav:allocate-frame-buffer
                  (libav:libav-error-operation condition)))
        (true (minusp (libav:libav-error-code condition)))
        (true (plusp (length (libav:libav-error-message condition))))))))

(define-test a-silent-film-has-no-audio-track
  ;; The test pattern has no sound; asking is not an error, it is a NIL.
  (true (null (libav:open-audio *test-pattern*))))

(define-test a-films-sound-decodes-to-mono-floats
  ;; A one-second 440 Hz sine, made by ffmpeg beside the test pattern.
  (let ((track (libav:open-audio
                (asdf:system-relative-pathname "luv/libav" "libav/test-tone.mp4"))))
    (true track)
    (unwind-protect
         (progn
           (true (= 44100 (libav:audio-sample-rate track)))
           (true (= 1 (libav:audio-channel-count track)))
           (true (and (libav:audio-duration track)
                      (< 0.9 (libav:audio-duration track) 1.2)))
           (let ((total 0) (peak 0.0) (buffer nil))
             (loop while (libav:decode-next-audio-frame track)
                   do (multiple-value-bind (samples count)
                          (libav:audio-frame-mono-samples track buffer)
                        (setf buffer samples)
                        (true (<= count (length samples)))
                        (dotimes (index count)
                          (setf peak (max peak (abs (aref samples index)))))
                        (incf total count)))
             ;; About a second of it, and audibly a tone rather than silence
             ;; or a decoder handing back garbage.
             (true (< 40000 total 50000))
             (true (< 0.1 peak 1.01)))
           ;; And again from the top.
           (libav:rewind-audio track)
           (true (libav:decode-next-audio-frame track)))
      (libav:close-audio track))))
