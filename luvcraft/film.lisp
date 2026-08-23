;;; Films of luvcraft sessions through LUV's shared application capture.
;;;
;;; A film never touches the swapchain.  The shared capture transaction owns
;;; its offscreen target, readback, submission, video writer, pacing, and
;;; cleanup; the Luvcraft methods in CAPTURE.LISP own readiness and frame
;;; meaning.
;;;
;;; Overlay animation (fireworks, gnomes, the marquee) advances by wall
;;; clock at each rendered frame, so a film loop paced against real time
;;; records the same motion a viewer would see, whether or not the
;;; session's own frame loop is running or its window is visible.

(in-package #:luvcraft)

(defun film-luvcraft-session (session pathname
                              &key (seconds 8) (frame-rate 30) before-frame
                                   (include-hud-p t)
                                   (include-viewmodel-p t))
  "Film SESSION for SECONDS of real time into an MP4 at PATHNAME.

Frames are captured at FRAME-RATE, paced against the wall clock so that
overlay animation driven by real time plays at its true speed.  BEFORE-FRAME,
when given, is called with the frame index before each capture, and is the
place to move the camera.  INCLUDE-HUD-P and INCLUDE-VIEWMODEL-P choose the
same compositing layers as a still capture.  Returns PATHNAME and the frame
count."
  (luv:capture-application-film
   session pathname
   :seconds seconds
   :frame-rate frame-rate
   :before-frame before-frame
   :label "luvcraft film"
   :options
   (list :include-hud-p include-hud-p
         :include-viewmodel-p include-viewmodel-p)))
