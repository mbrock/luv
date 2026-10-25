;;;; Disposable real-terrain comparison. BENCH_LIBRARY=1, native-1x only.
(require :asdf)
(assert (uiop:getenv "BENCH_LIBRARY"))
(load "scripts/bench-gpu-fps.lisp")
(load (merge-pathnames "shaders.lisp" *load-truename*))
(load (merge-pathnames "geometry.lisp" *load-truename*))
(load (merge-pathnames "appearance.lisp" *load-truename*))
(load (merge-pathnames "baked.lisp" *load-truename*))
(in-package #:terrain-lod)

(defvar *state* nil)
(defvar *mode* "control")
(defvar *frame* nil)
(defvar *skip-terrain* nil)
(defvar *capturing* nil)
(defvar *original-scene* (symbol-function 'r::encode-renderer-scene))
(defvar *original-frame-binding* (symbol-function 'r::renderer-frame-component-binding))
(defvar *original-run-case* (symbol-function 'gpu-fps::run-case))
(defvar *original-play* (symbol-function 'luvcraft.luft.game:play))

;; PLAY's explicit NIL default overrides the renderer special. Set the host
;; argument, and check the realized component below, before comparing bevels.
(setf (symbol-function 'luvcraft.luft.game:play)
      (lambda (&rest arguments)
        (apply *original-play* :terrain-bevels-p t arguments)))

(defmethod luv:create :around ((device luv::metal-gpu-device) (descriptor luv::texture-descriptor))
  ;; Identical readback capability in all variants; no copies in timed frames.
  (let ((copy (luv::canonical-texture-descriptor descriptor)))
    (when (and (= 1 (luv::texture-descriptor-sample-count copy))
               (member (luv::texture-descriptor-format copy) '(:rgba16-float :depth32-float))
               (member :render-attachment (luv::texture-descriptor-usage copy)))
      (pushnew :copy-src (luv::texture-descriptor-usage copy)))
    (call-next-method device copy)))

(defun merged-p () (not (null (search "merged" *mode*))))
(defun visibility-p () (not (null (search "visibility" *mode*))))
(defun baked-p () (not (null (search "baked" *mode*))))
(defun atlas-p () (member *mode* '("control" "copy" "atlas-lod") :test #'equal))
(defun geometry-key () (if (merged-p) :merged :reference))

(defun upload (owner device label words &optional (usage :storage))
  (let ((buffer (r::own-gpu-resource
                 owner device (luv:make-buffer-descriptor
                               :label label :size (max 16 (* 4 (length words)))
                               :usage (list usage :copy-dst)))))
    (when (plusp (length words)) (luv:write-buffer buffer words))
    buffer))

(defun material-fragment (limit &optional deferred)
  ;; Match every hook in MAKE-LUVCRAFT-TERRAIN-DRAWING, not only atmosphere.
  (let ((s::*terrain-screen-filter-p* t)
        (s::*terrain-edge-shaping-p* nil)
        (s::*terrain-material-character-function* 's::luvcraft-material-character)
        (s::*terrain-blocklight-function* 's::luvcraft-blocklight)
        (s::*terrain-cloud-visibility-function* 's::luvcraft-cloud-visibility)
        (s::*surface-atmosphere-function* 's::luvcraft-atmospheric-surface))
    (let ((fragment (appearance-fragment-specification limit)))
      (if deferred (deferred-fragment-specification fragment) fragment))))

(defun shared-corner-indices (references)
  "Keep expanded references addressable for visibility IDs, but allow the
forward control to reuse identical corners through the hardware vertex cache."
  (let ((seen (make-hash-table :test #'eql))
        (indices (make-array (/ (length references) 2) :element-type '(unsigned-byte 32))))
    (dotimes (i (length indices))
      (let* ((key (logior (ash (aref references (* 2 i)) 32)
                         (aref references (1+ (* 2 i)))))
             (index (or (gethash key seen) (setf (gethash key seen) i))))
        (setf (aref indices i) index)))
    indices))

(defun make-state (renderer modes)
  (assert (getf (r::renderer-component-options renderer) :terrain-bevels-p))
  (let* ((owner (make-instance 'r::gpu-resource-owner))
         (device (r::renderer-device renderer))
         (residents (loop for key in (r::renderer-slot-order renderer)
                          collect (r::mesh-slot-resident (gethash key (r::renderer-mesh-slots renderer)))))
         (populations (mapcar #'r::resident-population-population residents))
         (geometry (build-geometry populations))
         (state (list :owner owner :renderer renderer :geometry geometry
                      :residents residents :programs (make-hash-table :test #'equal)
                      :buffers (make-hash-table :test #'equal) :ranges (make-hash-table :test #'eq)
                      :targets (make-hash-table :test #'equal)))
         (success nil))
    (unwind-protect
         (progn
           (assert (every (lambda (population)
                            (equalp (r::render-population-descriptor-words population)
                                    (r::render-population-descriptor-words (first populations))))
                          populations))
           (dolist (key '(:sites :appearances :lights :vertices :normals :reference :merged))
             (setf (gethash key (getf state :buffers))
                   (upload owner device (format nil "LOD ~A" key) (getf geometry key))))
           (dolist (key '(:reference :merged))
             (let* ((indices (shared-corner-indices (getf geometry key)))
                    (buffer (r::own-gpu-resource
                             owner device (luv:make-buffer-descriptor
                                           :label "LOD shared corner indices"
                                           :size (max 16 (* 4 (length indices)))
                                           :usage '(:index :copy-dst)))))
               (when (plusp (length indices)) (luv:write-buffer buffer indices))
               (setf (gethash (ecase key (:reference :reference-indices) (:merged :merged-indices))
                              (getf state :buffers)) buffer)))
           (setf (gethash :descriptors (getf state :buffers))
                 (upload owner device "LOD material descriptors"
                         (r::render-population-descriptor-words (first populations))))
           (when (some (lambda (mode) (search "baked" mode)) modes)
             (let ((baked (build-baked geometry
                                       (r::render-population-descriptor-words (first populations)))))
               (setf (getf state :baked-stats) (getf baked :stats))
               (dolist (key '(:reference-vertices :merged-vertices :attributes :reference-indices :merged-indices))
                 (setf (gethash (list :baked key) (getf state :buffers))
                       (upload owner device (format nil "LOD baked ~A" key) (getf baked key)
                               (if (member key '(:reference-indices :merged-indices)) :index :storage))))))
           (loop for resident in residents for range in (getf geometry :residents)
                 do (assert (eq (r::resident-population-population resident) (getf range :population)))
                    (setf (gethash resident (getf state :ranges)) range))
           (let ((programs (getf state :programs)))
             (setf (gethash "control" programs) (r::terrain-scene-program (r::renderer-terrain renderer)))
             (dolist (mode (remove "control" modes :test #'equal))
               (let* ((deferred (search "visibility" mode))
                      (limit (if (search "lod" mode)
                                 (read-from-string (or (uiop:getenv "LOD_LIMIT") "1.0")) 0.0))
                      (fragment (material-fragment limit deferred)))
                 (setf (gethash mode programs)
                       (r::own-gpu-object
                        owner
                        (r:make-drawing-program
                         device :label (format nil "real terrain ~A" mode)
                         :vertex (cond (deferred (s:present-vertex-specification))
                                       ((member mode '("copy" "atlas-lod") :test #'equal)
                                        (s:terrain-vertex-specification))
                                       ((search "baked" mode) (baked-vertex-specification))
                                       (t (direct-vertex-specification)))
                         :fragment fragment
                         :targets '((:format :rgba16-float))
                         :depth-stencil (if deferred '(:format :depth32-float
                                                       :depth-write-enabled nil :depth-compare :always)
                                            '(:format :depth32-float
                                              :depth-write-enabled t :depth-compare :less)))))))
             (when (some (lambda (mode) (search "visibility" mode)) modes)
               (setf (gethash "visibility-pass" programs)
                     (r::own-gpu-object
                      owner (r:make-drawing-program
                             device :label "terrain visibility IDs"
                             :vertex (visibility-vertex-specification)
                             :fragment (visibility-fragment-specification)
                             :targets '((:format :rgba8-unorm))
                             :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                              :depth-compare :less))))))
           (setf success t)
           state)
      (unless success (luv:destroy owner)))))

(defun visibility-view (renderer)
  (let* ((extent (r::renderer-render-extent renderer))
         (targets (getf *state* :targets))
         (owner (getf *state* :owner))
         (device (r::renderer-device renderer)))
    (or (gethash extent targets)
        (setf (gethash (copy-list extent) targets)
              (let ((texture (r::own-gpu-resource
                              owner device
                              (luv:make-texture-descriptor
                               :label "exact terrain visibility"
                               :size extent :format :rgba8-unorm :dimensions :2d
                               :usage '(:render-attachment :texture-binding :copy-src)))))
                (r::own-gpu-resource owner device
                                    (luv:make-texture-view-descriptor :texture texture)))))))

(defun binding (renderer frame program &key visibility)
  (let* ((buffers (getf *state* :buffers))
         (values (list :sites (gethash :sites buffers)
                       :terrain-vertices (gethash :vertices buffers)
                       :terrain-appearances (gethash :appearances buffers)
                       :terrain-lights (gethash :lights buffers)
                       :terrain-normals (gethash :normals buffers)
                       :material-descriptors (gethash :descriptors buffers)
                       :references (gethash (geometry-key) buffers)
                       :baked-vertices (gethash (list :baked (if (merged-p) :merged-vertices :reference-vertices))
                                                buffers)
                       :baked-attributes (gethash '(:baked :attributes) buffers)
                       :camera-state (r::renderer-frame-state-camera-buffer frame)
                       :shadow-map (r::renderer-shadow-view renderer)
                       :shadow-sampler (r::renderer-shadow-sampler renderer)
                       :visibility-ids visibility))
         (inputs (loop for resource in (r::program-inputs program)
                       for key = (q:shader-resource-key resource)
                       append (list key (getf values key)))))
    (apply #'r::renderer-frame-program-binding renderer frame
           (list :lod program (geometry-key) visibility) program inputs)))

(defun draw-geometry (renderer frame pass program resident)
  (let* ((range (gethash resident (getf *state* :ranges)))
         (first (getf range (if (merged-p) :merged-first :reference-first)))
         (count (getf range (if (merged-p) :merged-count :reference-count))))
    (assert range () "A new resident entered an immutable measurement.")
    (when (plusp count)
      (luv:set-pipeline pass (r::program-pipeline program))
      (luv:set-bind-group pass 0 (binding renderer frame program))
      (if (visibility-p)
          (luv:draw pass count 1 first)
          (luv:draw-indexed pass (if (baked-p)
                                    (gethash (list :baked (if (merged-p) :merged-indices :reference-indices))
                                             (getf *state* :buffers))
                                    (gethash (if (merged-p) :merged-indices :reference-indices)
                                             (getf *state* :buffers)))
                            :uint32 count 1 first)))))

(defmethod r:encode-terrain :around
    ((drawing r:terrain-drawing) pass original-binding resident &key shadow-p)
  (cond ((or shadow-p (null *state*)) (call-next-method))
        (*skip-terrain* nil)
        ((atlas-p) (call-next-method))
        (t (draw-geometry (getf *state* :renderer) *frame* pass
                          (gethash *mode* (getf *state* :programs)) resident))))

(defmethod r::encode-scene-drawing :around ((drawing r::sky-drawing) pass original-binding)
  (unless *skip-terrain* (call-next-method)))

(defun encode-scene (renderer frame encoder player-p construction-p &key repetition-p)
  (when (and *state* *capturing*)
    (setf (getf *state* :uniforms) (copy-seq r::*frame-uniform-data*)))
  (let ((*frame* frame))
    (if (not (and *state* (visibility-p)))
        (funcall *original-scene* renderer frame encoder player-p construction-p :repetition-p repetition-p)
        (let* ((view (visibility-view renderer))
               (texture (luv::gpu-texture-view-texture view))
               (program (gethash "visibility-pass" (getf *state* :programs)))
               (pass (luv:begin-render-pass
                      encoder (luv:make-render-pass-descriptor
                               :label "real terrain visibility"
                               :color-attachments `((:view ,texture :load-op :clear :store-op :store
                                                     :clear-value #(0.0 0.0 0.0 0.0)))
                               :depth-stencil-attachment
                               `(:view ,(r::renderer-depth-view renderer)
                                 :depth-load-op :clear :depth-store-op :store :depth-clear-value 1.0)))))
          (dolist (resident (getf *state* :residents))
            (when (r::resident-drawn-p resident nil)
              (draw-geometry renderer frame pass program resident)))
          (luv:end-pass pass)
          (luv:prepare-texture encoder texture :texture-binding)
          ;; Lighting does not read depth: the attachment remains available
          ;; to unchanged scene objects after the pixel-owned terrain draw.
          (let ((lighting (luv:begin-render-pass
                           encoder (luv:make-render-pass-descriptor
                                    :label "pixel-owned terrain material"
                                    :color-attachments
                                    `((:view ,(r::renderer-scene-view renderer)
                                       :load-op :clear :store-op :store :clear-value #(0.0 0.0 0.0 1.0)))
                                    :depth-stencil-attachment
                                    `(:view ,(r::renderer-depth-view renderer)
                                      :depth-load-op :load :depth-store-op :store))))
                (shade (gethash *mode* (getf *state* :programs))))
            (r::encode-scene-drawing
             (r::renderer-sky renderer) lighting
             (r::renderer-frame-drawing-binding renderer frame (r::renderer-sky renderer)))
            (luv:set-pipeline lighting (r::program-pipeline shade))
            (luv:set-bind-group lighting 0 (binding renderer frame shade :visibility view))
            (luv:draw lighting 3)
            (let ((*skip-terrain* t))
              (r::encode-renderer-scene-drawings
               renderer frame lighting player-p construction-p repetition-p)))))))

(setf (symbol-function 'r::encode-renderer-scene) #'encode-scene)
(setf (symbol-function 'r::renderer-frame-component-binding)
      (lambda (frame key make-binding)
        (funcall *original-frame-binding* frame
                 (if *state* (list *mode* key) key) make-binding)))

(defun write-bytes (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence bytes out)))

(defun submit-frame (host target width height &optional capture)
  (luv.objective-c:with-autorelease-pool ()
    (let* ((renderer (r:viewer-renderer host))
           (device (r::renderer-device renderer))
           (queue (luv:device-queue device))
           (reader (luv:queue-gpu-time-reader queue))
           (encoder (luv:create device (luv:make-command-encoder-descriptor)))
           (commands nil) (readbacks nil) (start (gpu-fps::now)))
      (unwind-protect
           (progn
             (let ((*capturing* capture))
               (r::encode-viewer-frame host encoder target (list width height)))
             (assert (equal (r::renderer-render-extent renderer) (list width height)))
             (when capture
               (with-open-file (out (concatenate 'string capture ".camera.txt")
                                    :direction :output :if-exists :supersede)
                 (loop for value across (getf *state* :uniforms) do (format out "~,9F~%" value)))
               (dolist (entry (list (list target 4 "bgra8")
                                    (list (r::renderer-scene-texture renderer) 8 "rgba16")
                                    (list (r::renderer-depth-texture renderer) 4 "depth32")))
                 (let ((buffer (luv:create device (luv:make-buffer-descriptor
                                                  :size (* width height (second entry)) :usage '(:copy-dst)))))
                   (push (cons (third entry) buffer) readbacks)
                   (luv:encode encoder (luv::make-gpu-copy-texture-to-buffer-command
                                        :source (first entry) :destination buffer)))))
             (setf commands (luv:finish encoder))
             (let* ((encoded (gpu-fps::now)) (submission (luv:submit queue commands)))
               (luv:submitted-work-done queue)
               (let ((completed (gpu-fps::now)))
                 (when capture
                   (dolist (entry readbacks)
                     (let ((bytes (luv:read-buffer (cdr entry))))
                       (write-bytes (concatenate 'string capture "." (car entry)) bytes)
                       (when (equal (car entry) "bgra8")
                         (luv::write-rgba-png (concatenate 'string capture ".png") bytes
                                              width height :bgra8-unorm-srgb)))))
                 (loop with deadline = (+ completed 5d0)
                       for seconds = (funcall reader submission)
                       when seconds return (list (* 1000 seconds) (* 1000 (- encoded start))
                                                  (* 1000 (- completed start)))
                       do (assert (< (gpu-fps::now) deadline)) (sleep 0.001)))))
        (dolist (entry readbacks) (luv:destroy (cdr entry)))
        (when commands (luv:destroy commands))
        (luv:destroy encoder)))))

(defun select-mode (renderer mode)
  (setf *mode* mode
        (r::terrain-scene-program (r::renderer-terrain renderer))
        (gethash (if (atlas-p) mode "control") (getf *state* :programs))))

(defun capture-motion (kind host target width height prefix modes)
  "Fixed-residency camera flight, identical poses for every representation.
This is a motion/quality experiment, never a streaming-throughput claim."
  (let* ((camera (r:viewer-camera host))
         (position (r:camera-position camera))
         (yaw (r:camera-yaw camera)) (pitch (r:camera-pitch camera))
         (frames (gpu-fps::env-integer "LOD_MOTION" 0))
         (renderer (r:viewer-renderer host)))
    (unwind-protect
         (dotimes (i frames)
           (let* ((phase (/ (float i) (max 1 (1- frames))))
                  (wave (sin (* 2 pi phase)))
                  (travel (if (eq kind :cave) 2.0 24.0))
                  (rise (if (eq kind :cave) 0.4 (* 65.0 phase))))
             (setf (r:camera-position camera)
                   (r::make-vec3 (+ (r::vec3-x position) (* travel phase))
                                (+ (r::vec3-y position) (* 0.3 travel wave))
                                (+ (r::vec3-z position) rise))
                   (r:camera-yaw camera) (float (+ yaw (* 0.16 wave)) 1.0)
                   (r:camera-pitch camera) (float (+ pitch (* 0.08 wave)) 1.0)))
           (dolist (mode modes)
             (select-mode renderer mode)
             (let ((path (merge-pathnames (format nil "motion/~A/~4,'0D" mode i) prefix)))
               (ensure-directories-exist path)
               (submit-frame host target width height (namestring path)))))
      (setf (r:camera-position camera) position (r:camera-yaw camera) yaw
            (r:camera-pitch camera) pitch))))

(defun measure (kind host old-p width height)
  (assert (not old-p))
  (assert (eq gpu-fps::*benchmark-mode* :native-1x))
  (let* ((renderer (r:viewer-renderer host))
         (device (r::renderer-device renderer))
         (original (r::terrain-scene-program (r::renderer-terrain renderer)))
         (modes (uiop:split-string (or (uiop:getenv "LOD_MODES")
                                      "control,copy,merged,visibility,visibility-merged,atlas-lod,visibility-merged-lod")
                                  :separator ","))
         (order (append modes (reverse modes)))
         (root (uiop:ensure-directory-pathname (or (uiop:getenv "LOD_OUTPUT")
                                                  ".amp/in/artifacts/terrain-lod/")))
         (prefix (merge-pathnames (format nil "~(~A~)-~Dx~D/" kind width height) root))
         (target (luv:create device (luv:make-texture-descriptor
                                    :label "real terrain comparison target" :size (list width height)
                                    :dimensions :2d :format :bgra8-unorm-srgb
                                    :usage '(:render-attachment :copy-src :texture-binding))))
         (samples (make-hash-table :test #'equal)))
    (ensure-directories-exist prefix)
    (unwind-protect
         (luv::call-on-sdl-canvas-thread
          (r:viewer-canvas host)
          (lambda ()
            (setf *state* (make-state renderer modes))
            (with-open-file (out (merge-pathnames "geometry.sexp" prefix)
                                 :direction :output :if-exists :supersede)
              (print (getf (getf *state* :geometry) :stats) out))
            (when (getf *state* :baked-stats)
              (with-open-file (out (merge-pathnames "baked.sexp" prefix)
                                   :direction :output :if-exists :supersede)
                (print (getf *state* :baked-stats) out)))
            (with-open-file (out (merge-pathnames "timings.tsv" prefix)
                                 :direction :output :if-exists :supersede)
              (format out "frame~Cvariant~Cgpu_ms~Ccpu_encode_ms~Ccompletion_ms~%" #\Tab #\Tab #\Tab #\Tab)
              (let ((warmup (gpu-fps::env-integer "BENCH_WARMUP" 56))
                    (frames (gpu-fps::env-integer "BENCH_FRAMES" 280)))
                (assert (zerop (mod warmup (length order))))
                (assert (zerop (mod frames (length order))))
                (dotimes (i (+ warmup frames))
                  (select-mode renderer (nth (mod i (length order)) order))
                  (let ((sample (submit-frame host target width height)))
                    (when (>= i warmup)
                      (push (first sample) (gethash *mode* samples))
                      (format out "~D~C~A~C~,9F~C~,9F~C~,9F~%" (- i warmup) #\Tab *mode* #\Tab
                              (first sample) #\Tab (second sample) #\Tab (third sample)))))
                (finish-output out)))
            (unless (uiop:getenv "LOD_NO_CAPTURE")
              (dolist (mode modes)
                (select-mode renderer mode)
                (submit-frame host target width height (namestring (merge-pathnames mode prefix)))))
            (when (uiop:getenv "LOD_MOTION")
              (capture-motion kind host target width height prefix modes))
            (maphash (lambda (mode values)
                       (format t "~&LOD ~A ~A ~S~%" kind mode (gpu-fps::summary values))) samples)
            (list :mode :native-1x :extent (list width height)
                  :variants (loop for mode in modes collect (list mode (gpu-fps::summary (gethash mode samples)))))))
      (luv:submitted-work-done (luv:device-queue device))
      (r::clear-renderer-frame-bind-groups renderer)
      (setf (r::terrain-scene-program (r::renderer-terrain renderer)) original)
      (when *state* (luv:destroy (getf *state* :owner)))
      (setf *state* nil *mode* "control")
      (luv:destroy target))))

(setf (symbol-function 'gpu-fps::measure) #'measure
      (symbol-function 'gpu-fps::run-case)
      (lambda (kind backend width height)
        (unless (eq backend :old) (funcall *original-run-case* kind backend width height)))
      r::*terrain-bevels-p* t)

(gpu-fps::main)
