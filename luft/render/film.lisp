;;; Films of the studio: an orbiting camera rendered headlessly, frame by
;;; frame, into an MP4 through LUV:WITH-VIDEO-ENCODER.
;;;
;;; Motion is what the temporal resolve was built for, and an orbit shows
;;; both of its regimes: subpixel accumulation while the picture coheres,
;;; and history rejection whenever the style or scene is swapped mid-film.

(in-package #:luft.render)

(defun film-studio-orbit (pathname
                          &key (seconds 8) (frame-rate 30)
                               (width 960) (height 540)
                               (style :stock)
                               (styles (list style))
                               (effects (default-renderer-effects))
                               (scene (make-studio-scene))
                               (center-x 16.0) (center-y 14.0) (center-z 2.5)
                               (radius 16.0) (camera-height 9.5)
                               (field-of-view 0.85)
                               (turns 1.0))
  "Film one orbit of the studio into an MP4 at PATHNAME.

The camera circles CENTER at RADIUS and CAMERA-HEIGHT through TURNS
revolutions over SECONDS.  STYLES is the list of surface styles the film
cycles through, each getting an equal arc; a single-element list holds one
style throughout.  Returns PATHNAME and the frame count."
  (let* ((frame-count (max 1 (round (* seconds frame-rate))))
         (renderer (make-renderer :scene scene
                                  :camera (studio-camera
                                           (+ center-x radius) center-y
                                           camera-height
                                           :look-x center-x :look-y center-y
                                           :look-z center-z
                                           :field-of-view field-of-view)
                                  :width width :height height
                                  :style (first styles)
                                  :pipeline-styles styles
                                  :effects effects)))
    (unwind-protect
         (with-video-encoder (write-frame pathname width height
                              :frame-rate frame-rate
                              :format (renderer-color-format renderer))
           (dotimes (frame frame-count)
             (let* ((progress (/ (float frame 1.0) frame-count))
                    (angle (* 2.0 pi turns progress))
                    (style (nth (min (1- (length styles))
                                     (floor (* progress (length styles))))
                                styles)))
               (setf (renderer-style renderer) style
                     (renderer-camera renderer)
                     (studio-camera (+ center-x (* radius (cos angle)))
                                    (+ center-y (* radius (sin angle)))
                                    camera-height
                                    :look-x center-x :look-y center-y
                                    :look-z center-z
                                    :field-of-view field-of-view))
               (write-frame (render-pixels renderer)))))
      (destroy-renderer renderer))))
