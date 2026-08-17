(defpackage #:luv.libav.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:libav #:luv.libav)))

(in-package #:luv.libav.tests)

(deftest libav-loads-and-agrees-with-its-headers
  ;; LOAD-LIBAV signals LIBAV-VERSION-MISMATCH itself when the headers this
  ;; system groveled and the libraries it dlopens come from different builds,
  ;; so reaching a version string at all is the claim.
  (ok (libav:load-libav))
  (ok (libav:libav-loaded-p))
  (ok (stringp (libav:libav-build)))
  (ok (every (lambda (entry)
               (destructuring-bind (name . triple) entry
                 (and (stringp name)
                      (= 3 (length triple))
                      (every #'integerp triple))))
             (libav:libav-versions))))

(deftest the-decoders-luv-cares-about-are-present
  (ok (libav:decoder-available-p "h264"))
  (ok (libav:decoder-available-p "hevc"))
  (ok (libav:decoder-available-p "av1"))
  (ok (not (libav:decoder-available-p "no-such-decoder"))))

(deftest hardware-decoding-is-configured-for-this-platform
  ;; The platform's own path must be there, since decoding to a GPU surface is
  ;; the reason this binding exists.  Which other types FFmpeg was built with
  ;; is not this system's business.
  (let ((types (libav:hardware-device-types)))
    (ok (listp types))
    (ok (every #'keywordp types))
    #+darwin
    (ok (member :videotoolbox types))
    #+linux
    (ok (intersection '(:vaapi :vulkan) types))
    (ok (equal "videotoolbox"
               (libav:hardware-device-type-name :videotoolbox)))))

(deftest pixel-formats-round-trip-through-ffmpegs-own-names
  (ok (equal "nv12" (libav:pixel-format-name :nv12)))
  (ok (equal "yuv420p" (libav:pixel-format-name :yuv420p)))
  (ok (equal "videotoolbox_vld" (libav:pixel-format-name :videotoolbox))))

(deftest a-frames-groveled-layout-agrees-with-libavutil
  ;; The offsets are only trustworthy if the library reads back what Lisp
  ;; wrote through them.  Describe a picture through the groveled slots, then
  ;; let av_frame_get_buffer act on that description: a 1920-wide NV12 frame
  ;; has to come back with two planes and a stride of at least its width.
  (libav:with-frame (frame)
    (ok (libav:frame-live-p frame))
    (setf (libav:frame-width frame) 1920
          (libav:frame-height frame) 1080
          (libav:frame-pixel-format frame) :nv12)
    (ok (eq :nv12 (libav:frame-pixel-format frame)))
    (ok (not (libav:frame-hardware-p frame)))
    (libav:allocate-frame-buffer frame)
    (ok (>= (libav:frame-plane-pitch frame 0) 1920))
    (ok (>= (libav:frame-plane-pitch frame 1) 1920))
    (ok (not (cffi:null-pointer-p (libav:frame-plane-pointer frame 0))))
    (ok (not (cffi:null-pointer-p (libav:frame-plane-pointer frame 1))))
    ;; NV12 interleaves chroma, so there is no third plane to find.
    (ok (cffi:null-pointer-p (libav:frame-plane-pointer frame 2)))
    (libav:unreference-frame frame)
    (ok (cffi:null-pointer-p (libav:frame-plane-pointer frame 0)))))

(deftest frame-ownership-is-explicit-and-idempotent
  (let ((frame (libav:make-frame)))
    (ok (libav:frame-live-p frame))
    (libav:release-frame frame)
    (ok (not (libav:frame-live-p frame)))
    (ok (eq frame (libav:release-frame frame)))
    (ok (signals (libav:frame-width frame) 'error))))

(deftest cloned-frames-retain-the-picture-independently
  (libav:with-frame (frame)
    (setf (libav:frame-width frame) 8
          (libav:frame-height frame) 8
          (libav:frame-pixel-format frame) :rgba)
    (libav:allocate-frame-buffer frame)
    (let ((clone (libav:clone-frame frame)))
      (unwind-protect
           (progn
             (libav:unreference-frame frame)
             (ok (not (cffi:null-pointer-p
                       (libav:frame-plane-pointer clone 0)))))
        (libav:release-frame clone)))))

(defparameter *test-pattern*
  (asdf:system-relative-pathname "luv/libav" "libav/test-pattern.mp4")
  "A ten-frame 64x48 H.264 test pattern, small enough to keep in the tree.")

(deftest a-file-opens-and-describes-itself
  (libav:with-video (video *test-pattern*)
    (ok (libav:video-open-p video))
    (ok (= 64 (libav:video-width video)))
    (ok (= 48 (libav:video-height video)))
    (ok (= 10 (libav:video-frame-rate video)))))

(deftest decoding-drains-every-picture-in-the-file
  ;; The count is the claim: a decode loop that forgets to flush the codec at
  ;; end of file loses the pictures still buffered inside it, and a loop that
  ;; mishandles EAGAIN stops early.  Ten frames in, ten frames out.
  (libav:with-video (video *test-pattern*)
    (let ((count 0))
      (loop while (libav:decode-next-frame video) do (incf count))
      (ok (= 10 count))
      ;; Exhausted stays exhausted.
      (ok (null (libav:decode-next-frame video)))
      ;; And rewinding makes it whole again.
      (libav:rewind-video video)
      (let ((again 0))
        (loop while (libav:decode-next-frame video) do (incf again))
        (ok (= 10 again))))))

(deftest a-decoded-picture-converts-to-packed-rgba
  (libav:with-video (video *test-pattern*)
    (ok (libav:decode-next-frame video))
    (let ((frame (libav:video-frame video)))
      (ok (= 64 (libav:frame-width frame)))
      (ok (= 48 (libav:frame-height frame)))
      ;; Software decoding, so the picture is in ordinary memory and its
      ;; planes are real.
      (ok (not (libav:frame-hardware-p frame)))
      (ok (eq :yuv420p (libav:frame-pixel-format frame))))
    ;; swscale both converts and resizes, so a caller can ask for the size its
    ;; surface wants rather than the size the file happens to be.
    (let ((words (libav:frame-rgba-words video 16 16)))
      (ok (equal '(16 16) (array-dimensions words)))
      (ok (loop for index below (array-total-size words)
                always (= #xff (ldb (byte 8 24) (row-major-aref words index)))))
      ;; A test pattern is not one flat colour; a conversion that silently
      ;; produced an empty image would be.
      (let ((distinct (make-hash-table)))
        (dotimes (y 16)
          (dotimes (x 16)
            (setf (gethash (aref words y x) distinct) t)))
        (ok (> (hash-table-count distinct) 8))))))

(deftest a-failed-call-carries-ffmpegs-own-explanation
  (libav:with-frame (frame)
    ;; No width, height, or format: av_frame_get_buffer has nothing to size.
    (handler-case (progn (libav:allocate-frame-buffer frame) (fail "expected a failure"))
      (libav:libav-error (condition)
        (ok (eq 'libav:allocate-frame-buffer
                (libav:libav-error-operation condition)))
        (ok (minusp (libav:libav-error-code condition)))
        (ok (plusp (length (libav:libav-error-message condition))))))))
