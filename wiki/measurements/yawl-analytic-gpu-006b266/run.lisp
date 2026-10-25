;;;; Isolated native Metal 4 experiment. Never loads the managed image.
;;;; PROBE_CONFIG names a Lisp list of cases; PROBE_INPUT / PROBE_OUTPUT are dirs.
(require :asdf)
(asdf:load-system "luft/renderer")
(load "wiki/measurements/yawl-analytic-gpu-006b266/shaders.lisp")
(in-package #:analytic-gpu)

(defun seconds ()
  (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(defun read-bytes (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (assert (= (read-sequence bytes stream) (length bytes)))
      bytes)))

(defun write-bytes (path bytes)
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
    (write-sequence bytes stream)))

(defun frame (device program binding indices count instances targets depth readbacks)
  (luv.objective-c:with-autorelease-pool ()
    (let* ((queue (luv:device-queue device))
           (start (seconds))
           (encoder (luv:create device (luv:make-command-encoder-descriptor)))
           (commands nil))
      (unwind-protect
           (progn
             (let ((pass
                     (luv:begin-render-pass
                      encoder
                      (luv:make-render-pass-descriptor
                       :label "bounded plane comparison"
                       :color-attachments
                       (loop for target in targets for i from 0 collect
                         `(:view ,target :load-op :clear :store-op :store
                           :clear-value ,(if (zerop i) #(0.055 0.075 0.11 0.0)
                                            #(0.0 0.0 0.0 0.0))))
                       :depth-stencil-attachment
                       `(:view ,depth :depth-load-op :clear :depth-store-op :store
                         :depth-clear-value 1.0)))))
               (luv:set-pipeline pass (r::program-pipeline program))
               (luv:set-bind-group pass 0 binding)
               (luv:draw-indexed pass indices :uint32 count instances)
               (luv:end-pass pass))
             (loop for texture in (append targets (list depth)) for readback in readbacks do
               (luv:encode encoder (luv::make-gpu-copy-texture-to-buffer-command
                                    :source texture :destination readback)))
             (setf commands (luv:finish encoder))
             (let* ((encoded (seconds)) (submission (luv:submit queue commands)))
               (luv:submitted-work-done queue)
               (let ((completed (seconds))
                     (reader (luv:queue-gpu-time-reader queue)))
                 (assert reader)
                 (loop with deadline = (+ (seconds) 5d0)
                       for time = (funcall reader submission)
                       when time return (list (* 1000 time) (* 1000 (- encoded start))
                                              (* 1000 (- completed start)))
                       do (when (> (seconds) deadline) (error "Missing GPU feedback"))
                          (sleep 0.001)))))
        (when commands (luv:destroy commands))
        (luv:destroy encoder)))))

(defun run-case (device programs case input output timings)
  (destructuring-bind (&key name scene width height camera (grid 1)
                           (modes '("reference" "merged" "analytic" "proxy"))
                           (warmup 0) (frames 0)) case
    (let ((owner (make-instance 'r::gpu-resource-owner)))
      (unwind-protect
           (flet ((own (descriptor) (r::own-gpu-resource owner device descriptor)))
             (let* ((diagnostics-p (zerop frames))
                    (camera-buffer (own (luv:make-buffer-descriptor :size (* 4 (length camera))
                                                                   :usage '(:storage :copy-dst))))
                    (edges (read-bytes (merge-pathnames (format nil "~A-edges.f32" scene) input)))
                    (edge-buffer (own (luv:make-buffer-descriptor :size (length edges)
                                                                 :usage '(:storage :copy-dst))))
                    (targets (loop repeat (if diagnostics-p 2 1) collect
                               (own (luv:make-texture-descriptor
                                     :size (list width height) :dimensions :2d :format :rgba16-float
                                     :usage '(:render-attachment :copy-src)))))
                    (depth (own (luv:make-texture-descriptor
                                 :size (list width height) :dimensions :2d :format :depth32-float
                                 :usage '(:render-attachment :copy-src))))
                    (readbacks (when diagnostics-p
                                 (loop for size in '(8 8 4) collect
                                   (own (luv:make-buffer-descriptor :size (* size width height)
                                                                    :usage '(:copy-dst))))))
                    (variants nil))
               (luv:write-buffer camera-buffer (make-array (length camera) :element-type 'single-float
                                                          :initial-contents (mapcar #'float camera)))
               (luv:write-buffer edge-buffer edges)
               (dolist (mode modes)
                 (let* ((analytic-p (string= mode "analytic"))
                        (geometry (if (member mode '("analytic" "proxy") :test #'equal)
                                      "analytic" (if (string= mode "reference-copy") "reference" mode)))
                        (program (gethash (list analytic-p diagnostics-p (string= mode "reference-copy")) programs))
                        (vertices (read-bytes (merge-pathnames (format nil "~A-~A-vertices.f32" scene geometry) input)))
                        (indices (read-bytes (merge-pathnames (format nil "~A-~A-indices.u32" scene geometry) input)))
                        (vertex-buffer (own (luv:make-buffer-descriptor :size (length vertices)
                                                                       :usage '(:storage :copy-dst))))
                        (index-buffer (own (luv:make-buffer-descriptor :size (length indices)
                                                                      :usage '(:index :copy-dst))))
                        (binding (r::own-gpu-object
                                  owner (apply #'r::make-program-binding program device
                                               :vertices vertex-buffer :camera camera-buffer
                                               (when analytic-p (list :edges edge-buffer))))))
                   (luv:write-buffer vertex-buffer vertices)
                   (luv:write-buffer index-buffer indices)
                   (push (list mode program binding index-buffer (/ (length indices) 4)) variants)))
               (let ((order (append modes (reverse modes))))
                 (dotimes (i (+ warmup (if diagnostics-p (length modes) frames)))
                   (let* ((mode (if diagnostics-p (nth i modes) (nth (mod i (length order)) order)))
                          (entry (assoc mode variants :test #'equal))
                          (sample (apply #'frame device (append (cdr entry)
                                                               (list (* grid grid) targets depth readbacks)))))
                     (when (and (not diagnostics-p) (>= i warmup))
                       (format timings "~A~C~D~C~A~C~,9F~C~,9F~C~,9F~%"
                               name #\Tab (- i warmup) #\Tab mode #\Tab
                               (first sample) #\Tab (second sample) #\Tab (third sample)))
                     (when diagnostics-p
                       (loop for readback in readbacks for suffix in '("rgba16" "normal16" "depth32") do
                         (write-bytes (merge-pathnames (format nil "~A-~A.~A" name mode suffix) output)
                                      (luv:read-buffer readback)))))))
               (finish-output timings)
               (format t "~&CASE ~A ~Dx~D grid ~D frames ~D~%" name width height grid frames)))
        (luv:destroy owner)))))

(defun main ()
  (let* ((input (uiop:ensure-directory-pathname (uiop:getenv "PROBE_INPUT")))
         (output (uiop:ensure-directory-pathname (uiop:getenv "PROBE_OUTPUT")))
         (cases (with-open-file (stream (uiop:getenv "PROBE_CONFIG"))
                  (let ((*read-eval* nil)) (read stream))))
         (device (luv:request-gpu-device (make-instance 'luv:metal-gpu-provider)))
         (programs (make-hash-table :test #'equal)))
    (ensure-directories-exist output)
    (unwind-protect
         (progn
           (format t "~&DEVICE ~A~%" (luv.objective-c:objective-c-string
                                     (luv.metal:device-name (luv::metal-native-object device))))
           (dolist (diagnostics-p '(nil t))
             (dolist (variant '(nil t :copy))
               (let ((analytic-p (eq variant t)))
                 (setf (gethash (list analytic-p diagnostics-p (eq variant :copy)) programs)
                       (r::make-drawing-program
                        device :label (format nil "~A diagnostic=~S" variant diagnostics-p)
                        :vertex (vertex-specification)
                        :fragment (fragment-specification analytic-p diagnostics-p)
                        :targets (loop repeat (if diagnostics-p 2 1) collect '(:format :rgba16-float))
                        :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                         :depth-compare :less))))))
           (with-open-file (stream (merge-pathnames "timings.tsv" output)
                                   :direction :output :if-exists :supersede)
             (format stream "case~Cframe~Cvariant~Cgpu_ms~Ccpu_encode_ms~Ccompletion_ms~%"
                     #\Tab #\Tab #\Tab #\Tab #\Tab)
             (dolist (case cases) (run-case device programs case input output stream))))
      (maphash (lambda (key program) (declare (ignore key)) (luv:destroy program)) programs)
      (luv:destroy device))))

(luv.objective-c:with-autorelease-pool () (main))
