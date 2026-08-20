;;; Films of luvcraft sessions: real-time paced frame capture piped into
;;; ffmpeg through LUV:WITH-VIDEO-ENCODER.
;;;
;;; A film never touches the swapchain.  Presentation belongs to the
;;; compositor, which stops consuming frames the moment a window is hidden
;;; or occluded -- vkQueuePresentKHR then blocks the canvas thread forever,
;;; which is exactly what happened to the first film attempt.  Instead each
;;; film frame is encoded into a texture this file owns, on the canvas
;;; thread (the ownership boundary for GPU replacement), and read back.
;;;
;;; Overlay animation (fireworks, gnomes, the marquee) advances by wall
;;; clock at each rendered frame, so a film loop paced against real time
;;; records the same motion a viewer would see, whether or not the
;;; session's own frame loop is running or its window is visible.

(in-package #:luvcraft)

(defun render-luvcraft-film-frame (session texture buffer)
  "Render SESSION once into TEXTURE on its canvas thread; return the pixels."
  (let ((device (luvcraft-session-device session)))
    (luv::call-on-sdl-canvas-thread
     (luvcraft-session-canvas session)
     (lambda ()
       (let ((encoder nil) (commands nil))
         (unwind-protect
              (progn
                (setf encoder
                      (create device
                              (make-command-encoder-descriptor
                               :label "luvcraft film frame")))
                (encode-luvcraft-frame session texture encoder
                                       :readback-buffer buffer)
                (setf commands (finish encoder))
                (submit (device-queue device) commands))
           (when commands (destroy commands))
           (when encoder (destroy encoder))))))
    (read-buffer buffer)))

(defun film-luvcraft-session (session pathname
                              &key (seconds 8) (frame-rate 30) before-frame)
  "Film SESSION for SECONDS of real time into an MP4 at PATHNAME.

Frames are captured at FRAME-RATE, paced against the wall clock so that
overlay animation driven by real time plays at its true speed.  BEFORE-FRAME,
when given, is called with the frame index before each capture, and is the
place to move the camera.  Returns PATHNAME and the frame count."
  (unless (eq :open (canvas-state (luvcraft-session-canvas session)))
    (error "Cannot film a closed luvcraft session."))
  (wait-for-luvcraft-products
   session
   :minimum (hash-table-count (luvcraft-session-desired-chunks session)))
  (let* ((context (luvcraft-session-context session))
         (extent (canvas-extent context))
         (width (first extent))
         (height (second extent))
         (device (luvcraft-session-device session))
         (frame-count (max 1 (round (* seconds frame-rate))))
         (frame-interval (/ 1.0d0 frame-rate))
         (texture
           (create device
                   (make-texture-descriptor
                    :label "luvcraft film target"
                    :size (list width height) :dimensions :2d
                    :format (canvas-format context)
                    :usage '(:render-attachment :copy-src :copy-dst))))
         (buffer
           (create device
                   (make-buffer-descriptor
                    :label "luvcraft film readback"
                    :size (* 4 width height)
                    :usage '(:copy-dst)))))
    (unwind-protect
         (luv:with-video-encoder (write-frame pathname width height
                                  :frame-rate frame-rate
                                  :format (canvas-format context))
           (let ((start (/ (get-internal-real-time)
                           (float internal-time-units-per-second 1.0d0))))
             (dotimes (frame frame-count)
               (when before-frame
                 (funcall before-frame frame))
               (write-frame
                (render-luvcraft-film-frame session texture buffer))
               (let ((wait (- (+ start (* (1+ frame) frame-interval))
                              (/ (get-internal-real-time)
                                 (float internal-time-units-per-second
                                        1.0d0)))))
                 (when (plusp wait)
                   (sleep wait))))))
      (destroy buffer)
      (destroy texture))))
