(in-package #:luv.showcase)

;;; Small source-owned recipes for the deterministic diagnostic gazetteer.
;;; Scene construction and rendering remain owned by LUVCRAFT's named views;
;;; these wrappers only adapt their output to the capture manifest. #OK1OC8

(defun render-gazetteer-reference-plate (view-name pathname)
  "Render gazetteer VIEW-NAME to the capture protocol's exact PATHNAME."
  (let* ((directory (uiop:pathname-directory-pathname pathname))
         (rendered
           (luvcraft:capture-luvcraft-gazetteer-view view-name directory)))
    (unless (string= (uiop:native-namestring rendered)
                     (uiop:native-namestring pathname))
      (uiop:rename-file-overwriting-target rendered pathname))
    pathname))

(defun call-with-temporary-reference-frame-directory (function)
  "Call FUNCTION with a unique empty directory, then remove that directory."
  (uiop:with-temporary-file
      (:pathname marker :prefix "luv-showcase-reference-frames-"
       :type :unspecific)
    ;; WITH-TEMPORARY-FILE claims a unique name atomically.  Turn only that
    ;; exact claimed path into a directory for the consecutive PNG frames.
    (uiop:delete-file-if-exists marker)
    (let ((directory (uiop:ensure-directory-pathname marker)))
      (ensure-directories-exist directory)
      (unwind-protect
           (funcall function directory)
        (when (probe-file directory)
          (uiop:delete-directory-tree
           directory :validate t :if-does-not-exist :ignore))))))

(defun render-shadow-yard-motion (pathname)
  "Render a short consecutive sun-motion proof from the shadow-yard view."
  (let ((frame-count 24)
        (frame-rate 12))
    (call-with-temporary-reference-frame-directory
     (lambda (directory)
       (format t "~&capture shadow-yard-motion: rendering ~D consecutive frames...~%"
               frame-count)
       (finish-output)
       (let ((frames
               (luvcraft:capture-luvcraft-gazetteer-sequence
                :shadow-yard directory
                :count frame-count
                :forward-step 0.0
                :yaw-step 0.0
                :day-start 0.34
                :day-step 0.0015)))
         (unless (= frame-count (length frames))
           (error "Expected ~D shadow-yard frames, got ~D."
                  frame-count (length frames)))
         (format t "capture shadow-yard-motion: encoding ~D frames at ~D fps...~%"
                 frame-count frame-rate)
         (finish-output)
         (ensure-directories-exist pathname)
         (uiop:run-program
          (list
           "ffmpeg" "-hide_banner" "-loglevel" "error" "-nostdin" "-y"
           "-framerate" (format nil "~D" frame-rate)
           "-start_number" "0"
           "-i"
           (uiop:native-namestring
            (merge-pathnames "shadow-yard-%03d.png" directory))
           "-frames:v" (format nil "~D" frame-count)
           "-vf" "pad=ceil(iw/2)*2:ceil(ih/2)*2"
           "-c:v" "libx264" "-preset" "medium" "-crf" "18"
           "-pix_fmt" "yuv420p" "-movflags" "+faststart"
           "-progress" "pipe:1"
           (uiop:native-namestring pathname))
         :input nil :output t :error-output t)
         (format t "capture shadow-yard-motion: encoded ~A~%" pathname)
         (finish-output)))))
  pathname)

(luv:define-capture gazetteer-little-world-noon
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "The original aerial gazetteer terrain view at pinned noon for inspection.")
    (pathname)
  (render-gazetteer-reference-plate :little-world-noon pathname))

(luv:define-capture little-world-dusk
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "Generated little-world terrain under its deterministic warm dusk sky.")
    (pathname)
  (render-gazetteer-reference-plate :little-world-dusk pathname))

(luv:define-capture shadow-forest
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "Representative generated trees and terrain for cast-shadow inspection.")
    (pathname)
  (render-gazetteer-reference-plate :shadow-forest pathname))

(luv:define-capture glow-floor
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "A placed crystal proving material emission and blocklight after dark.")
    (pathname)
  (render-gazetteer-reference-plate :glow-floor pathname))

(luv:define-capture crystal-seam
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "A crystal lighting both sides of an exact chunk boundary.")
    (pathname)
  (render-gazetteer-reference-plate :crystal-seam pathname))

(luv:define-capture turtle-meadow
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "Three deterministic turtle poses for model, material, and shadow review.")
    (pathname)
  (render-gazetteer-reference-plate :turtle-meadow pathname))

(luv:define-capture shadow-yard-still
    (:figure OK1OC8 :kind :image :extension "png"
     :description
     "A low sun, flat receiver, and simple elevated casters for shadow review.")
    (pathname)
  (render-gazetteer-reference-plate :shadow-yard pathname))

(luv:define-capture shadow-yard-motion
    (:figure OK1OC8 :kind :video :extension "mp4"
     :description
     "A two-second consecutive sun sequence across the deterministic shadow yard.")
    (pathname)
  (render-shadow-yard-motion pathname))
