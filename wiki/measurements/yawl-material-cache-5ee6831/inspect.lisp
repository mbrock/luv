;;;; Untimed, fixed-resident appearance checks, independent of hilltop timing.
(in-package #:gpu-fps)

(let ((original (symbol-function 'worlds)))
  (setf (symbol-function 'worlds)
        (lambda (kind)
          ;; QUALITY_ONLY repurposes the cave slot; record that explicitly.
          (if (not (and (eq kind :cave) (uiop:getenv "QUALITY_ONLY"))) (funcall original kind)
              (let ((builder (ter:make-scene-builder
                              :horizontal-bits 11 :material-vocabulary
                              (g:luvcraft-palette-vocabulary g:*luvcraft-palette*)))
                    (world (old::make-block-world
                            :source (make-instance 'old::gazetteer-open-sky-source))))
                (labels ((cell (x y z block)
                           (ter:scene-builder-cell builder (+ 1024 x) (+ 1024 z) y
                                                   :material (g:block-placement block))
                           (old::ensure-world-chunk world (floor x 16) (floor y 16) (floor z 16))
                           (setf (old:world-block-at world x y z) block)))
                  (dotimes (x 32)
                    (dotimes (z 32) (cell x 3 z old::*grass-block*))
                    (loop for y from 4 below 11 do (cell x y 0 old::*stone-block*)))
                  (old::relight-block-world world)
                  (values (change-class (ter:finish-scene-builder builder) 'fixture-scene
                                        :source (g:make-luvcraft-world-source
                                                 :seed 7 :offset 1024 :relief :meadow))
                          world)))))))

(defun inspect-material-cache (host programs)
  (let* ((directory (uiop:ensure-directory-pathname (uiop:getenv "CACHE_INSPECT")))
         (renderer (r:viewer-renderer host))
         (drawing (r::renderer-terrain renderer))
         (camera (r:viewer-camera host))
         (solid (ter:scene-solid (r:viewer-source host)))
         (poses `(("grass-near" (1040.5 1032.5 4.7) ,(/ pi 2) -0.65)
                  ("grass-grazing" (1040.5 1032.5 4.7) ,(/ pi 2) -0.08)
                  ("stone-near" (1040.5 1026.5 7.0) ,(- (/ pi 2)) 0.0)
                  ("stone-grazing" (1040.5 1026.5 7.0) -0.15 0.0))))
    (ensure-directories-exist (merge-pathnames "inspection.sexp" directory))
    (with-open-file (record (merge-pathnames "inspection.sexp" directory)
                            :direction :output :if-exists :supersede)
      (dolist (entry programs)
        (dolist (pose poses)
          (destructuring-bind (name position yaw pitch) pose
            (luv::call-on-sdl-canvas-thread
             (r:viewer-canvas host)
             (lambda ()
               (setf *interleaved-mode* (car entry)
                     (r::terrain-scene-program drawing) (cdr entry)
                     (r:camera-position camera) (apply #'v:make-vec3 position)
                     (r:camera-yaw camera) yaw
                     (r:camera-pitch camera) pitch
                     (r:camera-field-of-view camera) 0.85
                     (r::renderer-history-valid-p renderer) nil
                     (r::renderer-previous-view renderer) nil)))
            (luv::call-with-capture-target
             (lambda (capture)
               (destructuring-bind (width height) (luv::capture-extent capture)
                 (format record "~S~%"
                         (list :variant (car entry) :pose pose :extent (list width height)
                               :temporal (r::renderer-temporal-p renderer) :fixed-solid t
                               :fixture (if (uiop:getenv "QUALITY_ONLY") :daylight-material :cave)))
                 ;; Independent history per pose/variant; one full jitter cycle
                 ;; at rest, then the same small continuous traverse for both.
                 (dotimes (frame 32)
                   (when (>= frame 16)
                     (luv::call-on-sdl-canvas-thread
                      (r:viewer-canvas host)
                      (lambda () (incf (v:vec3-x (r:camera-position camera)) 0.025))))
                   (let ((pixels (luv::render-capture-frame capture)))
                     (when (or (= frame 0) (= frame 15) (>= frame 16))
                       (luv::write-rgba-png
                        (merge-pathnames (format nil "~A-~A-~2,'0D.png" (car entry) name frame) directory)
                        pixels width height (luv::capture-format capture)))))))
             (make-instance 'luv:application-capture :application host :kind :screenshot
                            :options '(:inspector-p nil :render-scale 1.0)))
            (assert (eq solid (ter:scene-solid (r:viewer-source host))))))))))

(setf *interleaved-inspection* #'inspect-material-cache)

(when (uiop:getenv "QUALITY_ONLY")
  ;; Construct temporal renderers normally, but never alternate material
  ;; variants through one shared history or report these captures as timings.
  (setf (symbol-function 'measure)
        (lambda (kind host old-p width height)
          (declare (ignore kind width height))
          (assert (not old-p))
          (let* ((renderer (r:viewer-renderer host))
                 (drawing (r::renderer-terrain renderer))
                 (control (r::terrain-scene-program drawing))
                 (programs nil))
            (unwind-protect
                 (progn
                   (luv::call-on-sdl-canvas-thread
                    (r:viewer-canvas host)
                    (lambda ()
                      (dolist (mode '("control" "cached"))
                        (push (cons mode (block-program renderer mode)) programs))))
                   (setf *interleaved-programs* programs)
                   (inspect-material-cache host programs)
                   (list :quality-only t))
              (setf *interleaved-programs* nil)
              (luv::call-on-sdl-canvas-thread
               (r:viewer-canvas host)
               (lambda ()
                 (r::clear-renderer-frame-bind-groups renderer)
                 (setf (r::terrain-scene-program drawing) control)
                 (dolist (entry programs) (luv:destroy (cdr entry))))))))))
