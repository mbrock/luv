;;; Video output for renderer readbacks: raw frames piped into ffmpeg.
;;;
;;; The renderer already produces tightly packed RGBA or BGRA bytes for PNG
;;; screenshots; a film is the same bytes written frame after frame into an
;;; ffmpeg child process reading rawvideo on stdin.  ffmpeg owns pixel-format
;;; conversion, H.264 encoding, and the MP4 container, so no libav encoding
;;; binding is needed here -- the existing luv/libav system remains a decoder.

(in-package #:luv)

(defun video-pixel-format-name (format)
  "The ffmpeg rawvideo -pix_fmt for a renderer readback FORMAT."
  (ecase format
    ((:rgba8-unorm :rgba8-unorm-srgb) "rgba")
    ((:bgra8-unorm :bgra8-unorm-srgb) "bgra")))

(defun call-with-video-encoder (function pathname width height
                                &key (frame-rate 30)
                                     (format :rgba8-unorm)
                                     (constant-rate-factor 18))
  "Encode a video at PATHNAME from frames FUNCTION writes.

FUNCTION receives one argument, a writer to call with each frame's packed
pixel bytes in FORMAT at WIDTH by HEIGHT.  Frames become an H.264 MP4 at
FRAME-RATE via an ffmpeg subprocess; CONSTANT-RATE-FACTOR is x264's quality
knob (lower is better, 18 is visually lossless).  An odd source dimension is
padded by one pixel because broadly playable yuv420p requires even dimensions.
Returns PATHNAME and the number of frames written."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (ensure-directories-exist pathname)
  (let* ((frame-bytes (* 4 width height))
         (frame-count 0)
         (error-output (make-string-output-stream))
         (process
           (sb-ext:run-program
            "ffmpeg"
            (list "-hide_banner" "-loglevel" "error" "-y"
                  "-f" "rawvideo"
                  "-pix_fmt" (video-pixel-format-name format)
                  "-video_size" (format nil "~Dx~D" width height)
                  "-framerate" (format nil "~D" frame-rate)
                  "-i" "-"
                  "-vf" "pad=ceil(iw/2)*2:ceil(ih/2)*2"
                  "-c:v" "libx264"
                  "-preset" "medium"
                  "-crf" (format nil "~D" constant-rate-factor)
                  "-pix_fmt" "yuv420p"
                  "-movflags" "+faststart"
                  (uiop:native-namestring pathname))
            :search t :wait nil
            :input :stream :output nil :error error-output)))
    (unwind-protect
         (handler-case
             (progn
               (funcall function
                        (lambda (pixels)
                          (unless (= (length pixels) frame-bytes)
                            (error "Expected ~D frame bytes, got ~D."
                                   frame-bytes (length pixels)))
                          (write-sequence pixels (sb-ext:process-input process))
                          (incf frame-count)))
               (close (sb-ext:process-input process))
               (sb-ext:process-wait process)
               (let ((code (sb-ext:process-exit-code process)))
                 (unless (eql code 0)
                   (error "ffmpeg exited with code ~A:~%~A"
                          code (get-output-stream-string error-output))))
               (values pathname frame-count))
           (sb-int:broken-pipe ()
             (ignore-errors (close (sb-ext:process-input process) :abort t))
             (sb-ext:process-wait process)
             (error "ffmpeg stopped accepting frames (exit ~A):~%~A"
                    (sb-ext:process-exit-code process)
                    (get-output-stream-string error-output))))
      (when (sb-ext:process-alive-p process)
        (close (sb-ext:process-input process) :abort t)
        (sb-ext:process-wait process)))))

(defmacro with-video-encoder ((writer pathname width height &rest options)
                              &body body)
  "Evaluate BODY with WRITER bound as a local frame-writing function.

Call (WRITER pixels) once per frame; see CALL-WITH-VIDEO-ENCODER for
PATHNAME, WIDTH, HEIGHT, and OPTIONS."
  (let ((function (gensym "WRITER")))
    `(call-with-video-encoder
      (lambda (,function)
        (flet ((,writer (pixels) (funcall ,function pixels)))
          ,@body))
      ,pathname ,width ,height ,@options)))
