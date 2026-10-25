;;;; Production A/B: original resident buffers, independent temporal histories.
(require :asdf)
(assert (uiop:getenv "BENCH_LIBRARY"))
(load "scripts/bench-gpu-fps.lisp")
(in-package #:gpu-fps)

(defvar *original-play* (symbol-function 'h:play))
(defvar *original-run-case* (symbol-function 'run-case))
(defvar *original-scene* (symbol-function 'r::encode-renderer-scene))
(defvar *capture-uniforms* nil)
(defvar *capturing* nil)

(setf (symbol-function 'h:play)
      (lambda (&rest args)
        (apply *original-play* :terrain-bevels-p (not (equal (uiop:getenv "VIS_BEVELS") "0")) args)))
(setf (symbol-function 'r::encode-renderer-scene)
      (lambda (&rest args)
        (when *capturing* (setf *capture-uniforms* (copy-seq r::*frame-uniform-data*)))
        (apply *original-scene* args)))

(defmethod luv:create :around ((device luv::metal-gpu-device) (descriptor luv::texture-descriptor))
  (let ((copy (luv::canonical-texture-descriptor descriptor)))
    (when (and (= 1 (luv::texture-descriptor-sample-count copy))
               (member (luv::texture-descriptor-format copy) '(:rgba16-float :rg16-float :depth32-float))
               (member :render-attachment (luv::texture-descriptor-usage copy)))
      (pushnew :copy-src (luv::texture-descriptor-usage copy)))
    (call-next-method device copy)))

(defun production-frame (host renderer target width height enabled &optional capture)
  (luv.objective-c:with-autorelease-pool ()
    (let* ((r::*terrain-visibility-p* (and enabled (not (uiop:getenv "VIS_NULL"))))
           (*capturing* capture)
           (device (r::renderer-device renderer))
           (queue (luv:device-queue device))
           (reader (luv:queue-gpu-time-reader queue))
           (encoder (luv:create device (luv:make-command-encoder-descriptor)))
           (commands nil) (readbacks nil) (start (now)))
      (setf (r:viewer-renderer host) renderer)
      (unwind-protect
           (progn
             (r::encode-viewer-frame host encoder target (list width height))
             (when capture
               (ensure-directories-exist capture)
               (with-open-file (out (concatenate 'string capture ".camera.txt")
                                    :direction :output :if-exists :supersede)
                 (loop for value across *capture-uniforms* do (format out "~,9F~%" value)))
               (dolist (entry (append (list (list target "bgra8")
                                            (list (r::renderer-scene-texture renderer) "rgba16")
                                            (list (r::renderer-depth-texture renderer) "depth32"))
                                      (when (r::renderer-temporal-p renderer)
                                        (list (list (r::renderer-motion-texture renderer) "rg16")))))
                 (let* ((texture (first entry))
                        (size (luv::gpu-texture-size texture))
                        (bytes (* (first size) (second size)
                                  (luv:texture-format-bytes-per-texel (luv::gpu-texture-format texture))))
                        (buffer (luv:create device (luv:make-buffer-descriptor :size bytes :usage '(:copy-dst)))))
                   (push (list (second entry) buffer) readbacks)
                   (luv:encode encoder (luv::make-gpu-copy-texture-to-buffer-command
                                        :source texture :destination buffer)))))
             (setf commands (luv:finish encoder))
             (let* ((encoded (now)) (submission (luv:submit queue commands)))
               (luv:submitted-work-done queue)
               (let ((completed (now)))
                 (when capture
                   (dolist (entry readbacks)
                     (let ((bytes (luv:read-buffer (second entry))))
                       (with-open-file (out (concatenate 'string capture "." (first entry))
                                            :direction :output :if-exists :supersede
                                            :element-type '(unsigned-byte 8))
                         (write-sequence bytes out))
                       (when (equal (first entry) "bgra8")
                         (luv::write-rgba-png (concatenate 'string capture ".png") bytes
                                              width height :bgra8-unorm-srgb)))))
                 (loop with deadline = (+ completed 5d0)
                       for seconds = (funcall reader submission)
                       when seconds return (list (* 1000 seconds) (* 1000 (- encoded start))
                                                  (* 1000 (- completed start)))
                       do (assert (< (now) deadline)) (sleep 0.001)))))
        (dolist (entry readbacks) (luv:destroy (second entry)))
        (when commands (luv:destroy commands))
        (luv:destroy encoder)))))

(defun production-measure (kind host old-p width height)
  (assert (not old-p))
  (let* ((original (r:viewer-renderer host))
         (device (r::renderer-device original))
         (candidate nil)
         (target (luv:create device (luv:make-texture-descriptor
                                    :label "visibility production comparison" :size (list width height)
                                    :format :bgra8-unorm-srgb :dimensions :2d
                                    :usage '(:render-attachment :copy-src :texture-binding))))
         (root (uiop:ensure-directory-pathname (uiop:getenv "VIS_OUTPUT")))
         (prefix (merge-pathnames (format nil "~(~A~)-~Dx~D/" kind width height) root))
         (camera (r:viewer-camera host))
         (position (r:camera-position camera))
         (yaw (r:camera-yaw camera)) (pitch (r:camera-pitch camera))
         (order (if (equal (uiop:getenv "VIS_REVERSE") "1") '(t nil nil t) '(nil t t nil)))
         (samples (make-hash-table)))
    (ensure-directories-exist prefix)
    (unwind-protect
         (luv::call-on-sdl-canvas-thread
          (r:viewer-canvas host)
          (lambda ()
            (let ((start (now)))
              (setf candidate (apply #'r:make-renderer device :bgra8-unorm-srgb
                                     (list width height) (r::renderer-component-options original)))
              (r::renderer-set-meshes
               candidate
               (loop for key in (r::renderer-slot-order original)
                     collect (cons key (r::mesh-slot-prepared-mesh
                                        (gethash key (r::renderer-mesh-slots original))))))
              (with-open-file (out (merge-pathnames "setup.sexp" prefix)
                                   :direction :output :if-exists :supersede)
                (print (list :clone-and-upload-ms (* 1000 (- (now) start))
                             :options (r::renderer-component-options original)
                             :residents (length (r::renderer-slot-order original))) out)))
            (dolist (renderer (list original candidate))
              (setf (r::renderer-frame-index renderer) 0
                    (r::renderer-previous-view renderer) nil
                    (r::renderer-history-valid-p renderer) nil))
            (with-open-file (out (merge-pathnames "timings.tsv" prefix)
                                 :direction :output :if-exists :supersede)
              (format out "frame~Cvariant~Cgpu_ms~Ccpu_encode_ms~Ccompletion_ms~%" #\Tab #\Tab #\Tab #\Tab)
              (let ((warmup (env-integer "BENCH_WARMUP" 32)) (frames (env-integer "BENCH_FRAMES" 120)))
                (assert (zerop (mod warmup 4))) (assert (zerop (mod frames 4)))
                (dotimes (i (+ warmup frames))
                  (let* ((enabled (nth (mod i 4) order))
                         (values (production-frame host (if enabled candidate original) target
                                                   width height enabled)))
                    (when (>= i warmup)
                      (push (first values) (gethash enabled samples))
                      (format out "~D~C~A~C~,9F~C~,9F~C~,9F~%" (- i warmup) #\Tab
                              (if enabled "visibility" "control") #\Tab
                              (first values) #\Tab (second values) #\Tab (third values)))))))
            (dolist (enabled '(nil t))
              (production-frame host (if enabled candidate original) target width height enabled
                                (namestring (merge-pathnames (if enabled "visibility" "control") prefix))))
            (dotimes (i (env-integer "VIS_MOTION" 0))
              (let* ((phase (/ (float i) (max 1 (1- (env-integer "VIS_MOTION" 0)))))
                     (wave (sin (* 2 pi phase))))
                (setf (r:camera-position camera)
                      (r::make-vec3 (+ (r::vec3-x position) (* 24 phase))
                                   (+ (r::vec3-y position) (* 7 wave))
                                   (+ (r::vec3-z position) (* 65 phase)))
                      (r:camera-yaw camera) (float (+ yaw (* .16 wave)) 1.0)
                      (r:camera-pitch camera) (float (+ pitch (* .08 wave)) 1.0)))
              (when (uiop:getenv "VIS_GRAZE")
                (let ((phase (/ (float i) (max 1 (1- (env-integer "VIS_MOTION" 0))))))
                  (setf (r:camera-position camera)
                        (r::make-vec3 (+ (r::vec3-x position) (* 4 phase))
                                     (+ (r::vec3-y position) (* 4 phase))
                                     (+ 18.0 (* 27.0 phase)))
                        (r:camera-pitch camera) (float (+ -0.2 (* .4 phase)) 1.0))))
              (dolist (enabled '(nil t))
                (production-frame
                 host (if enabled candidate original) target width height enabled
                 (namestring (merge-pathnames (format nil "motion/~A/~4,'0D"
                                                      (if enabled "visibility" "control") i) prefix)))))
            ;; Remove then republish a resident while both renderers remain alive.
            ;; Renumbering all subsequent IDs must never read stale local streams.
            (when (uiop:getenv "VIS_EDIT")
              (let* ((key (first (r::renderer-slot-order original)))
                     (prepared (r::mesh-slot-prepared-mesh (gethash key (r::renderer-mesh-slots original)))))
                (dolist (state '("removed" "restored"))
                  (dolist (enabled '(nil t))
                    (let ((renderer (if enabled candidate original)))
                      (if (equal state "removed") (r::renderer-update-meshes renderer nil (list key))
                          (r::renderer-set-mesh renderer key prepared))
                      (production-frame host renderer target width height enabled
                                        (namestring (merge-pathnames
                                                     (format nil "edit/~A/~A" state
                                                             (if enabled "visibility" "control")) prefix))))))))
            (when (uiop:getenv "VIS_RESIZE")
              (dolist (size (list (list (+ width 128) (+ height 72)) (list width height)))
                (luv:destroy target)
                (setf target (luv:create device (luv:make-texture-descriptor
                                                :size size :format :bgra8-unorm-srgb :dimensions :2d
                                                :usage '(:render-attachment :copy-src :texture-binding))))
                (dolist (enabled '(nil t))
                  (production-frame
                   host (if enabled candidate original) target (first size) (second size) enabled
                   (namestring (merge-pathnames (format nil "resize/~Dx~D/~A" (first size) (second size)
                                                        (if enabled "visibility" "control")) prefix))))))
            (with-open-file (out (merge-pathnames "extent.sexp" prefix)
                                 :direction :output :if-exists :supersede)
              (print (list :internal (r::renderer-render-extent candidate)
                           :temporal (r::renderer-temporal-resolve-kind candidate)) out))
            (loop for enabled in '(nil t) do
              (format t "~&PRODUCTION ~A ~A ~S~%" kind (if enabled :visibility :control)
                      (summary (gethash enabled samples))))))
      (luv:submitted-work-done (luv:device-queue device))
      (setf (r:viewer-renderer host) original (r:camera-position camera) position
            (r:camera-yaw camera) yaw (r:camera-pitch camera) pitch)
      (when candidate (r:destroy-renderer candidate))
      (luv:destroy target))))

(setf (symbol-function 'measure) #'production-measure
      (symbol-function 'run-case)
      (lambda (kind backend width height)
        (unless (eq backend :old) (funcall *original-run-case* kind backend width height)))
      r::*terrain-bevels-p* (not (equal (uiop:getenv "VIS_BEVELS") "0")))
(when (uiop:getenv "VIS_SHADER") (setf *default-reconstruction* :shader))
(main)
