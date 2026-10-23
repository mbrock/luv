;;;; Fixed-target GPU benchmark, run from the checkout root:
;;;; VK_DRIVER_FILES=/path/to/hardware_icd.json SDL_VIDEODRIVER=offscreen \
;;;;   ./env sbcl --script scripts/bench-gpu-fps.lisp
;;;; On macOS omit SDL_VIDEODRIVER. Optional environment: BENCH_FRAMES (300),
;;;; BENCH_WARMUP (60), BENCH_OUTPUT (build/gpu-fps.sexp), BENCH_SIZES
;;;; (1280x720,1920x1080), BENCH_SCENES
;;;; (comma-separated alpine,meadow,cave,balls,turtle,terminal,companion,
;;;; live-alpine,live-meadow,atelier). BENCH_CAPTURE_DIR optionally saves PNGs.
;;;; BENCH_REPEATS (1) repeats the matrix; BENCH_SKY_OFF / BENCH_TERRAIN_OFF
;;;; opt into controlled ablations (do not compare them to full-frame FPS).
;;;; BENCH_MSAA (4) controls Luft's non-temporal samples; old Luvcraft uses1.
;;;; Native 720p and 1080p, TAA and dynamic resolution OFF on both renderers.
;;;; No swapchain/readback in the timed loop: serialized completion throughput,
;;;; NOT monitor presentation FPS. CPU encode and GPU execution are separate.
(require :asdf)
(asdf:load-system "luvcraft/luft/game")
(asdf:load-system "luvcraft")

(defpackage #:gpu-fps
  (:use #:cl)
  (:local-nicknames (#:r #:luft.render) (#:h #:luvcraft.luft.game)
                    (#:g #:luvcraft.luft) (#:ter #:luft.terrain)
                    (#:v #:luv.arithmetic.lisp.vec3) (#:old #:luvcraft)))
(in-package #:gpu-fps)

(defparameter *source-revision*
  (string-trim '(#\Newline #\Return)
               (uiop:run-program '("git" "rev-parse" "HEAD") :output :string)))

(cffi:defcstruct timespec (seconds :long) (nanoseconds :long))
(cffi:defcfun ("clock_gettime" clock-gettime) :int (clock :int) (time :pointer))
(defun now ()
  (cffi:with-foreign-object (time '(:struct timespec))
    (assert (zerop (clock-gettime #+darwin 6 #-darwin 1 time)))
    (+ (float (cffi:foreign-slot-value time '(:struct timespec) 'seconds) 1d0)
       (* 1d-9 (cffi:foreign-slot-value time '(:struct timespec) 'nanoseconds)))))
(defun env-integer (name default)
  (let ((value (uiop:getenv name))) (if value (parse-integer value) default)))
(defvar *query-pool* nil)
(defvar *query-index* 0)
(defvar *pass-records* nil)
(defvar *pass-starts* (make-hash-table :test #'eq))
(defvar *phase-table* nil)
(defvar *phase-lock* (sb-thread:make-mutex :name "benchmark phases"))
(defvar *skip-sky-pipeline* nil)
(defvar *pass-pipelines* (make-hash-table :test #'eq :weakness :key))

(defmethod luv:encode :after ((pass luv::gpu-render-pass-encoder) (command luv::gpu-set-pipeline-command))
  (when *skip-sky-pipeline*
    (setf (gethash pass *pass-pipelines*) (luv::gpu-set-pipeline-command-pipeline command))))

(defmethod luv:encode :around ((pass luv::gpu-render-pass-encoder) (command luv::gpu-draw-command))
  (unless (and *skip-sky-pipeline* (eq *skip-sky-pipeline* (gethash pass *pass-pipelines*)))
    (call-next-method)))

(defun call-with-phase-profiling (function)
  "Temporarily time real setup work, including worker calls; inclusive times."
  (let ((saved nil))
    (unwind-protect
         (progn
           (dolist (name '(luft:mesh-star-chunk r::make-render-population
                           r::star-lattice-light-words r::%upload-render-population
                           old::make-block-mesh-snapshot old::mesh-block-snapshot-halo
                           old::relight-block-world ter::finish-scene-builder))
             (let ((original (symbol-function name)) (key name))
               (push (cons name original) saved)
               (setf (symbol-function name)
                     (lambda (&rest arguments)
                       (let ((table *phase-table*) (start (now)))
                         (unwind-protect (apply original arguments)
                           (when table
                             (sb-thread:with-mutex (*phase-lock*)
                               (let ((entry (or (gethash key table)
                                                (setf (gethash key table) (list 0 0d0)))))
                                 (incf (first entry))
                                 (incf (second entry) (* 1000 (- (now) start))))))))))))
           (funcall function))
      (dolist (entry saved) (setf (symbol-function (car entry)) (cdr entry))))))

(defun phase-summary ()
  (sb-thread:with-mutex (*phase-lock*)
    (loop for name being the hash-keys of *phase-table* using (hash-value value)
          collect (list name :calls (first value) :inclusive-ms (second value)))))

#+darwin
(defmethod luv:show-canvas :around ((canvas luv::sdl-canvas))
  ;; Batch Cocoa cannot activate an application with activation policy zero.
  ;; This benchmark never presents; retain the already-created hidden window.
  canvas)

(defclass fixture-scene (ter:scene)
  ((source :initarg :source :reader fixture-source)))
(defmethod ter:streaming-scene-source ((scene fixture-scene))
  ;; Companion presentation needs the original world's coordinate conversion,
  ;; not a streaming worker. The finite fixture retains that source explicitly.
  (fixture-source scene))

#+linux
(progn
  ;; Benchmark-local Vulkan ABI: no instrumentation on production frames.
  (cffi:defcstruct query-info
    (s-type :uint32) (next :pointer) (flags :uint32) (type :uint32)
    (count :uint32) (statistics :uint32))
  (cffi:defcfun ("vkCreateQueryPool" create-query-pool) :int
    (device :pointer) (info :pointer) (allocator :pointer) (pool :pointer))
  (cffi:defcfun ("vkDestroyQueryPool" destroy-query-pool) :void
    (device :pointer) (pool :pointer) (allocator :pointer))
  (cffi:defcfun ("vkCmdResetQueryPool" reset-query-pool) :void
    (command :pointer) (pool :pointer) (first :uint32) (count :uint32))
  (cffi:defcfun ("vkCmdWriteTimestamp" write-timestamp) :void
    (command :pointer) (stage :uint32) (pool :pointer) (index :uint32))
  (cffi:defcfun ("vkGetQueryPoolResults" query-results) :int
    (device :pointer) (pool :pointer) (first :uint32) (count :uint32)
    (size :size) (data :pointer) (stride :uint64) (flags :uint32))
  (cffi:defcfun ("vkGetPhysicalDeviceProperties" physical-properties) :void
    (device :pointer) (data :pointer))
  (defun stamp (encoder)
    (assert (< *query-index* 256) () "Benchmark timestamp pool exhausted")
    (prog1 *query-index*
      (write-timestamp (luv::vulkan-command-encoder-command-buffer encoder)
                       #x10000 *query-pool* *query-index*) ; ALL_COMMANDS
      (incf *query-index*)))
  (defmethod luv:begin-render-pass :around
      ((encoder luv::vulkan-gpu-command-encoder) (descriptor luv::render-pass-descriptor))
    (if *query-pool*
        (let* ((index (stamp encoder)) (start (now)) (pass (call-next-method)))
          (setf (gethash pass *pass-starts*)
                (list index start (or (luv::gpu-descriptor-label descriptor)
                                      (format nil "pass-~D" (floor index 2)))))
          pass)
        (call-next-method)))
  (defmethod luv:end-pass :around ((pass luv::vulkan-gpu-render-pass-encoder))
    (if *query-pool*
        (let ((entry (gethash pass *pass-starts*)))
          (multiple-value-prog1 (call-next-method)
            (push (list (third entry) (first entry)
                        (stamp (luv::vulkan-render-pass-command-encoder pass))
                        (* 1000 (- (now) (second entry)))) *pass-records*)
            (remhash pass *pass-starts*)))
        (call-next-method))))

#+darwin
(progn
  (defmethod luv:begin-render-pass :around
      ((encoder luv::metal-gpu-command-encoder) (descriptor luv::render-pass-descriptor))
    (let* ((start (now)) (pass (call-next-method)))
      (setf (gethash pass *pass-starts*)
            (list nil start (or (luv::gpu-descriptor-label descriptor) "unlabelled")))
      pass))
  (defmethod luv:end-pass :around ((pass luv::metal-render-pass-encoder))
    (let ((entry (gethash pass *pass-starts*)))
      (multiple-value-prog1 (call-next-method)
        (push (list (third entry) nil nil (* 1000 (- (now) (second entry)))) *pass-records*)
        (remhash pass *pass-starts*)))))

#+linux
(defun timestamp-valid-bits (device)
  (let ((physical (luv::vulkan-device-physical-device device)))
    (cffi:with-foreign-object (count :uint32)
      (setf (cffi:mem-ref count :uint32) 0)
      (vk:get-physical-device-queue-family-properties physical count (cffi:null-pointer))
      (cffi:with-foreign-object (properties '(:struct luv.vulkan::queue-family-properties)
                                          (cffi:mem-ref count :uint32))
        (vk:get-physical-device-queue-family-properties physical count properties)
        (let ((bits (cffi:foreign-slot-value
                     (cffi:mem-aptr properties '(:struct luv.vulkan::queue-family-properties)
                                    (luv::vulkan-device-queue-family device))
                     '(:struct luv.vulkan::queue-family-properties) 'luv.vulkan::timestamp-valid-bits)))
          (assert (plusp bits) () "Selected Vulkan queue has no timestamp support")
          bits)))))

(defun device-description (device)
  #+linux
  (cffi:with-foreign-object (properties :uint8 824)
    (physical-properties (luv::vulkan-device-physical-device device) properties)
    ;; Vulkan's stable 64-bit VkPhysicalDeviceProperties ABI; verified against
    ;; offsetof(limits.timestampPeriod)=720, sizeof=824 in vulkan_core.h.
    (list :name (cffi:foreign-string-to-lisp (cffi:inc-pointer properties 20))
          :vendor (cffi:mem-ref properties :uint32 8)
          :device-type (cffi:mem-ref properties :uint32 16)
          :driver-version (cffi:mem-ref properties :uint32 4)
          :timestamp-ns (cffi:mem-ref properties :float 720)
          :timestamp-valid-bits (timestamp-valid-bits device)))
  #+darwin
  (list :name (old::benchmark-metal-device-name device)))

(defun make-timer (device)
  #+linux
  (cffi:with-foreign-objects ((info '(:struct query-info)) (out :pointer))
    (loop for i below (cffi:foreign-type-size '(:struct query-info))
          do (setf (cffi:mem-aref info :uint8 i) 0))
    (setf (cffi:foreign-slot-value info '(:struct query-info) 's-type) 11
          (cffi:foreign-slot-value info '(:struct query-info) 'type) 2
          (cffi:foreign-slot-value info '(:struct query-info) 'count) 256)
    (assert (zerop (create-query-pool (luv::vulkan-handle device) info
                                    (cffi:null-pointer) out)))
    (cffi:mem-ref out :pointer))
  #-linux (declare (ignore device))
  #-linux nil)

(defun summary (values)
  (when values
    (let* ((sorted (sort (copy-seq values) #'<)) (n (length sorted)))
      (flet ((percentile (p) (elt sorted (1- (ceiling (* n p))))))
        (list :n n :mean (/ (reduce #'+ sorted) n)
              :p50 (percentile 0.50) :p95 (percentile 0.95) :p99 (percentile 0.99))))))

(defun worlds (kind)
  "Finite identical 32x32 terrain, with generated seed-7 light on Luft.
The old renderer relights the same occupied cells with its own light solver."
  (let* ((builder (ter:make-scene-builder :horizontal-bits 11
                   :material-vocabulary (g:luvcraft-palette-vocabulary g:*luvcraft-palette*)))
         (world (old::make-block-world :source (make-instance 'old::gazetteer-open-sky-source))))
    (labels ((cell (x y z block)
               (when block
                 (ter:scene-builder-cell builder (+ 1024 x) (+ 1024 z) y
                                         :material (g:block-placement block))
                 (old::ensure-world-chunk world (floor x 16) (floor y 16) (floor z 16))
                 (setf (old:world-block-at world x y z) block))))
      (if (member kind '(:alpine :meadow))
          (ter:generate-world-source-chunk
           (g:make-luvcraft-world-source :seed 7 :relief kind :offset 0) 0
           (lambda (x y z material)
             (when (and (< x 32) (< y 32))
               (cell x z y (old:block-kind-named (ter:material-placement-name material))))))
          (progn
            (dotimes (x 32) (dotimes (z 32) (cell x 3 z old::*grass-block*)))
            (when (eq kind :cave)
              (dotimes (x 32)
                (dotimes (z 32)
                  (cell x 11 z old::*stone-block*)
                  (when (or (zerop x) (= x 31) (zerop z) (= z 31))
                    (loop for y from 4 below 11 do (cell x y z old::*stone-block*)))))
              (cell 16 6 16 old::*lantern-block*))
            (when (eq kind :terminal)
              (loop for x from 12 to 20 do
                (loop for y from 4 to 8 do (cell x y 16 old::*terminal-block*))))))
      (old::relight-block-world world)
      (values (change-class (ter:finish-scene-builder builder) 'fixture-scene
                            :source (g:make-luvcraft-world-source :seed 7 :offset 1024
                                                                 :relief (if (eq kind :alpine) :alpine :meadow)))
              world))))

(defun fixture-physics (host old-p)
  (if old-p (old::luvcraft-session-physics host) (h::viewer-physics host)))

(defun place-ride-camera (host old-p ride)
  (let ((pose (old::luvcraft-focus-camera-pose ride host)))
    (if old-p
        (old::set-camera-pose (old:luvcraft-session-camera host) pose)
        (let ((camera (r:viewer-camera host)))
          (setf (r:camera-position camera) (h::host-position host (old::camera-pose-position pose))
                (r:camera-yaw camera) (- (/ pi 2) (old::camera-pose-yaw pose))
                (r:camera-pitch camera) (old::camera-pose-pitch pose)
                (r:camera-field-of-view camera) (old::camera-pose-field-of-view pose))))))

(defun prepare-fixture (kind host old-p world)
  (case kind
    (:balls
     ;; Same solver and exact cells on both hosts. Keep 100 bodies awake so
     ;; the warmup cannot turn this into a sleeping/static ball benchmark.
     (let ((physics (old::make-physics-world :terrain world)))
       (dotimes (i 100)
         (old::spawn-physics-body physics (+ 12 (* 0.7 (mod i 10)))
                                 (+ 5 (* 0.6 (floor i 100)))
                                 (+ 10 (* 0.7 (mod (floor i 10) 10)))
                                 :radius 0.25 :never-sleep-p t))
       (if old-p (setf (old::luvcraft-session-physics host) physics)
           (setf (h::viewer-physics host) physics))))
    (:turtle
     (let* ((animal (make-instance 'old::turtle :seed 7
                                  :position (v:make-vec3 16d0 4d0 12d0)
                                  :yaw 0d0 :heading 0d0))
            (ride (old::activate-luvcraft-critter animal host)))
       (if old-p
           (progn
             (setf (fill-pointer (old::critter-population-critters (old::luvcraft-session-critters host))) 0)
             (old::add-critter (old::luvcraft-session-critters host) animal)
             (setf (old::luvcraft-session-modal-focus host) ride))
           (progn
             (setf (h::viewer-animals host) (list animal)
                   (h::viewer-animals-initialized-p host) t
                   (r:viewer-modal-focus host) ride)))
       (place-ride-camera host old-p ride)))
    (:terminal
     (luv.ghostty:load-libghostty-vt)
     (let ((device
             (if old-p
                 (let ((terminal (old::open-terminal-display host 16 6 16 :back :fixture "")))
                   (old::attach-terminal-display-shell terminal)
                   (old:focus-luvcraft-session host terminal)
                   (dotimes (i 120) (old::advance-luvcraft-focus-camera host (/ 1d0 60)))
                   (old::terminal-display-device terminal))
                 (let ((terminal (r::focus-viewer-terminal host)))
                   (assert terminal)
                   (dotimes (i 120) (r::advance-viewer-camera host (/ 1d0 60) nil))
                   (r::world-terminal-device terminal)))))
       (luv.terminal:send-pty-device-text
        device (format nil "printf '\\033[2J\\033[HLUVCRAFT GPU BENCHMARK\\n'; i=1; while [ $i -le 24 ]; do printf 'line %02d | seed 7 | fixed native target\\n' \"$i\"; i=$((i+1)); done~%"))
       (sleep 0.3)
       (let ((text (luv.terminal:call-with-pty-device-terminal device #'luv.ghostty:terminal-text)))
         (assert (search "line 24" text)))
       (unless old-p (r::advance-viewer-world-text host))))
    (:companion
     (let* ((turn (make-instance 'luv.application-agent:turn :prompt "Benchmark thought"))
            (agent
              (if old-p
                  (make-instance 'luvcraft.agent:gnome :session host
                                 :position (v:make-vec3 16d0 4d0 12d0) :x 16 :y 4 :z 12)
                  (let ((inhabitant (h::make-inhabitant host :gnome (v:make-vec3 1040d0 1036d0 4d0))))
                    (push inhabitant (h::viewer-agents host))
                    (setf (h::viewer-agents-initialized-p host) t)
                    (h::ensure-inhabitant-presentation host inhabitant)))))
       (setf (luv.application-agent:turn-thought turn)
             "I am thinking about the light in this little block world."
             (luv.application-agent:turn-status turn) :thinking
             (luv.application-agent:turn-started turn) (get-internal-real-time))
       (when old-p (old:add-luvcraft-overlay host agent))
       (luvcraft.agent::ensure-gnome-cloud agent turn)))))

(defun advance-fixture (kind host old-p)
  (case kind
    (:balls (old::step-physics-world (fixture-physics host old-p) (/ 1f0 60)))
    (:turtle
     (let* ((ride (if old-p (old::luvcraft-session-modal-focus host) (r:viewer-modal-focus host)))
            (animal (old::critter-ride-critter ride))
            (contact (if old-p (old:luvcraft-session-world host)
                         (make-instance 'h::animal-contact :solid (ter:scene-solid (r:viewer-source host))))))
       (old::urge-critter animal 1d0 0d0 (/ 1d0 60))
       (old::advance-critter animal contact (/ 1d0 60))
       (old::carry-luvcraft-player host animal)
       (place-ride-camera host old-p ride)))
    (:terminal
     (unless old-p (r::advance-viewer-world-text host)))
    (:companion
     (unless old-p
       (dolist (overlay (r::viewer-overlays host)) (r::refresh-viewer-overlay overlay host))))))

(defun measure (kind host old-p width height)
  (let* ((canvas (if old-p (old:luvcraft-session-canvas host) (r:viewer-canvas host)))
         (device (if old-p (old:luvcraft-session-device host)
                     (r::renderer-device (r:viewer-renderer host))))
         (queue (luv:device-queue device))
         (description (device-description device))
         (timer (make-timer device))
         (reader (luv:queue-gpu-time-reader queue))
         (target (luv:create device (luv::make-texture-descriptor
                                    :label "benchmark target" :dimensions :2d
                                    :size (list width height 1) :format :bgra8-unorm-srgb
                                    :usage '(:render-attachment :copy-src :copy-dst :texture-binding))))
         (cpu nil) (wall nil) (gpu nil) (simulation nil) (cpu-frame nil)
         (passes (make-hash-table :test #'equal))
         (cpu-passes (make-hash-table :test #'equal)))
    (declare (ignorable reader))
    (format t "~&DEVICE ~S SDL=~A ~Dx~D~%" description (sdl3:get-current-video-driver) width height)
    (when (getf description :device-type)
      (assert (/= 4 (getf description :device-type)) () "Software Vulkan device rejected"))
    (labels ((frame (record-p)
               (let* ((*query-pool* timer) (*query-index* 0) (*pass-records* nil)
                      (start (now))
                      (encoder (luv:create device (luv::make-command-encoder-descriptor)))
                      (commands nil))
                 (unwind-protect
                      (progn
                        (let ((before (now)))
                          (advance-fixture kind host old-p)
                          (when record-p (push (* 1000 (- (now) before)) simulation)))
                        #+linux
                        (progn (reset-query-pool (luv::vulkan-command-encoder-command-buffer encoder) timer 0 256)
                               (stamp encoder))
                        (if old-p
                            (old::encode-luvcraft-frame host target encoder)
                            (r::encode-viewer-frame host encoder target (list width height)))
                        #+linux (stamp encoder)
                        (setf commands (luv:finish encoder))
                        (let* ((encoded (now)) (submission (luv:submit queue commands)))
                          (luv:submitted-work-done queue)
                          (when record-p
                            (push (* 1000 (- encoded start)) cpu-frame)
                            (push (- (* 1000 (- encoded start)) (first simulation)) cpu)
                            (push (* 1000 (- (now) start)) wall)
                            #+linux
                            (cffi:with-foreign-object (data :uint64 *query-index*)
                              (assert (zerop (query-results (luv::vulkan-handle device) timer 0 *query-index*
                                                          (* 8 *query-index*) data 8 3)))
                              (flet ((duration (a b)
                                       (* 1d-6 (getf description :timestamp-ns)
                                          (ldb (byte (getf description :timestamp-valid-bits) 0)
                                               (- (cffi:mem-aref data :uint64 b) (cffi:mem-aref data :uint64 a))))))
                                (push (duration 0 (1- *query-index*)) gpu)
                                (dolist (pass *pass-records*)
                                  (push (duration (second pass) (third pass))
                                        (gethash (first pass) passes)))))
                            #-linux
                            (progn
                              (assert reader () "Backend has no GPU timestamp reader")
                              ;; MTL4 commit feedback trails the completion event.
                              ;; The completed wall time above excludes this wait.
                              (loop with deadline = (+ (now) 5d0)
                                    for seconds = (funcall reader submission)
                                    when seconds do (push (* 1000 seconds) gpu) (return)
                                    do (when (> (now) deadline)
                                         (error "GPU feedback missing for submission ~D" submission))
                                       (sleep 0.001)))
                            (dolist (pass *pass-records*)
                              (push (fourth pass) (gethash (first pass) cpu-passes))))))
                   (when commands (luv:destroy commands))
                   (luv:destroy encoder)))))
      (unwind-protect
           (luv::call-on-sdl-canvas-thread
            canvas
            (lambda ()
              (let ((*skip-sky-pipeline*
                      (when (uiop:getenv "BENCH_SKY_OFF")
                        (if old-p (old::luvcraft-session-sky-native-pipeline host)
                            (r::program-pipeline (r::scene-drawing-program (r::renderer-sky (r:viewer-renderer host))))))))
                (dotimes (i (env-integer "BENCH_WARMUP" 60)) (frame nil))
                (dotimes (i (env-integer "BENCH_FRAMES" 300)) (frame t))
                (assert (= (length gpu) (env-integer "BENCH_FRAMES" 300)))
                (let ((actual (if old-p (old::luvcraft-session-render-extent host)
                                  (r::renderer-render-extent (r:viewer-renderer host)))))
                  (assert (equalp actual (list width height)) () "Render extent ~S != requested ~S" actual (list width height)))
                (list :device description :extent (list width height) :taa nil :dynamic-resolution nil
                      :scene-samples (if old-p 1 (r::renderer-scene-sample-count (r:viewer-renderer host)))
                      :exposure (cond (old-p :host-default) ((eq kind :atelier) :automatic) (t 0.45))
                      :sky (null (uiop:getenv "BENCH_SKY_OFF")) :terrain (null (uiop:getenv "BENCH_TERRAIN_OFF"))
                      :cpu-encode-ms (summary cpu) :completion-ms (summary wall)
                      :cpu-frame-ms (summary cpu-frame)
                      :simulation-ms (summary simulation)
                      :completion-fps (/ 1000 (getf (summary wall) :mean))
                      :gpu-ms (summary gpu)
                      :cpu-passes (loop for label being the hash-keys of cpu-passes using (hash-value values)
                                        collect (list label (summary values)))
                      :gpu-passes (loop for label being the hash-keys of passes using (hash-value values)
                                        collect (list label (summary values)))))))
        (luv:submitted-work-done queue)
        (luv:destroy target)
        #+linux (destroy-query-pool (luv::vulkan-handle device) timer (cffi:null-pointer))))))

(defun settle-live-viewer (host)
  (let ((scene (r:viewer-source host)))
    (loop repeat 1800
          when (luv::call-on-sdl-canvas-thread
                (r:viewer-canvas host)
                (lambda ()
                  (r::advance-viewer-streaming host)
                  (and (plusp (hash-table-count (ter:streaming-scene-loaded scene)))
                       (null (ter:streaming-scene-replacement scene))
                       (zerop (hash-table-count (ter:streaming-scene-load-outstanding scene)))
                       (ter::authored-world-residency-ready-p scene))))
            return t
          do (sleep 0.1)
          finally (error "Live terrain never settled"))))

(defun run-live-case (kind backend width height)
  "Actual unbounded seed-7 game residency, or the actual default atelier.
Unlike the finite cell-matched fixtures, host residency footprints differ."
  (setf *phase-table* (make-hash-table :test #'eq))
  (let* ((started (now)) (old-p (eq backend :old)) (host nil)
         (relief (if (eq kind :live-alpine) :alpine :meadow)))
    (unwind-protect
         (progn
           (setf host
                 (cond
                   ((eq kind :atelier)
                    (r:start-viewer :world-pathname nil :width width :height height :frames-per-second 1))
                   (old-p
                    (old:start-luvcraft
                     :world (old:make-empty-little-block-world :seed 7 :relief relief :chunk-height 64)
                     :width width :height height :visible-p nil :high-pixel-density-p nil
                     :frames-per-second nil :residency-radius 6
                     :sky-clock (make-instance 'old::sky-clock :pinned-day-fraction 0.5)
                     :camera (make-instance 'old:fly-camera :position (v:make-vec3 -15 45 -24)
                                            :yaw (- (/ pi 2) 0.9) :pitch -0.55 :field-of-view 0.85)))
                   (t
                    (h:play :seed 7 :relief relief :pathname nil :player-p nil
                            :width width :height height :frames-per-second 1 :fixed-exposure 0.45
                            :camera (r:make-fly-camera :position (v:make-vec3 1009 1000 45)
                                                       :yaw 0.9 :pitch -0.55 :field-of-view 0.85)))))
           (let ((canvas (if old-p (old:luvcraft-session-canvas host) (r:viewer-canvas host))))
             (setf (luv:canvas-clock canvas) (luv:make-demand-clock))
             (if old-p
                 (old::wait-for-luvcraft-products host :minimum (hash-table-count (old::luvcraft-session-desired-chunks host)) :timeout 180d0)
                 (progn
                   (setf (r::viewer-running-p host) nil)
                   (unless (eq kind :atelier)
                     (setf (h::viewer-animals-initialized-p host) t
                           (h::viewer-agents-initialized-p host) t
                           (old::sky-clock-pinned-day-fraction (h::viewer-sky-clock host)) 0.5))
                   (settle-live-viewer host)))
             (let ((phases (phase-summary)) (setup (- (now) started)))
               (setf *phase-table* nil)
               (append (list :scene kind :backend backend :setup-seconds setup :setup-profile phases
                             :resident-chunks (if old-p (hash-table-count (old::luvcraft-session-chunk-products host))
                                                  (hash-table-count (ter:streaming-scene-loaded (r:viewer-source host)))))
                       (measure kind host old-p width height)))))
      (when host (if old-p (old:stop-luvcraft host) (r:stop-viewer host))))))

(defun run-case (kind backend width height)
  (when (member kind '(:live-alpine :live-meadow :atelier))
    (return-from run-case (run-live-case kind backend width height)))
  (setf *phase-table* (make-hash-table :test #'eq))
  (let* ((started (now)) (old-p (eq backend :old)) (host nil))
    (multiple-value-bind (scene world) (worlds kind)
      (let* ((terrain-p (member kind '(:alpine :meadow)))
             (eye (if terrain-p '(-15 -24 45) '(16 8 6)))
             (yaw (if terrain-p 0.9 (/ pi 2)))
             (pitch (if terrain-p -0.55 0.0)))
        (unwind-protect
             (progn
               (setf host
                     (cond
                       (old-p
                         (old:start-luvcraft
                          :world world :width width :height height :visible-p nil :high-pixel-density-p nil
                          :frames-per-second nil :residency-radius 3
                          :sky-clock (make-instance 'old::sky-clock :pinned-day-fraction 0.5)
                          :camera (make-instance 'old:fly-camera
                                                 :position (v:make-vec3 (first eye) (third eye) (second eye))
                                                 :yaw (- (/ pi 2) yaw) :pitch pitch :field-of-view 0.85)))
                       (t (r:start-game-host
                          :solid scene :player-p nil :width width :height height :frames-per-second 1
                          :world-pathname nil :fixed-exposure 0.45
                          :camera (r:make-fly-camera
                                   :position (v:make-vec3 (+ 1024 (first eye))
                                                         (+ 1024 (second eye)) (third eye))
                                   :yaw yaw :pitch pitch :field-of-view 0.85)
                          :sky-factory 'h::make-luvcraft-sky-drawing
                          :terrain-factory 'h::make-luvcraft-terrain-drawing
                          :torch-factory 'h::make-luvcraft-torch-drawing
                          :finishing-factory 'h::make-luvcraft-image-finishing
                          :figure-factory 'h::make-creature-figure-drawing
                          :frame-class 'h:luvcraft-viewer
                          :frame-initargs (list :luvcraft-world world
                                               :hotbar (list (g:block-placement old::*stone-block*)))))))
               (let ((canvas (if old-p (old:luvcraft-session-canvas host) (r:viewer-canvas host))))
                 (setf (luv:canvas-clock canvas) (luv:make-demand-clock))
                 (unless old-p
                   (setf (r::viewer-running-p host) nil
                         (r:viewer-mode host) (make-instance 'r:first-person-mode))
                   (unless (eq kind :atelier)
                     (setf (old::sky-clock-pinned-day-fraction (h::viewer-sky-clock host)) 0.5)))
                 (when old-p (old::wait-for-luvcraft-products host :minimum (hash-table-count (old::luvcraft-session-desired-chunks host)) :timeout 120d0))
                 (luv::call-on-sdl-canvas-thread canvas (lambda () (prepare-fixture kind host old-p world)))
                 (let ((startup (- (now) started)) (phases (phase-summary)))
                   (setf *phase-table* nil)
                   (let ((result (append (list :scene kind :backend backend :setup-seconds startup
                                               :setup-profile phases)
                                         (measure kind host old-p width height))))
                     (when (uiop:getenv "BENCH_CAPTURE_DIR")
                       (let ((path (merge-pathnames (format nil "~(~A~)-~(~A~)-~Dx~D.png" kind backend width height)
                                                    (uiop:ensure-directory-pathname (uiop:getenv "BENCH_CAPTURE_DIR")))))
                         (if old-p (old:capture-luvcraft-screenshot host path)
                             (r:capture-viewer-frame path host))))
                     result))))
          (when host (if old-p (old:stop-luvcraft host) (r:stop-viewer host))))))))

(defun run ()
  (setf luv::*canvas-watchdog-fatal-seconds* nil
        luv::*canvas-dispatch-timeout* 600.0
        luv::*fixed-render-scale* 1.0
        old::*luvcraft-temporal-p* nil
        old::*luvcraft-benchmark-terrain-p* (null (uiop:getenv "BENCH_TERRAIN_OFF"))
        r::*benchmark-terrain-p* (null (uiop:getenv "BENCH_TERRAIN_OFF"))
        r::*scene-sample-count* (env-integer "BENCH_MSAA" 4)
        r::*temporal-upscaling-p* nil
        r::*dynamic-resolution-frame-rate* nil)
  (let ((output (or (uiop:getenv "BENCH_OUTPUT") "build/gpu-fps.sexp"))
        (sizes (mapcar (lambda (s)
                         (mapcar #'parse-integer (uiop:split-string s :separator "x")))
                       (uiop:split-string (or (uiop:getenv "BENCH_SIZES") "1280x720,1920x1080")
                                          :separator ",")))
        (scenes (mapcar (lambda (s) (intern (string-upcase s) :keyword))
                        (uiop:split-string (or (uiop:getenv "BENCH_SCENES") "alpine,meadow,cave,balls,turtle,terminal,companion,live-alpine,live-meadow,atelier") :separator ","))))
    (assert (plusp (env-integer "BENCH_FRAMES" 300)))
    (assert (not (minusp (env-integer "BENCH_WARMUP" 60))))
    (assert (plusp (env-integer "BENCH_REPEATS" 1)))
    (ensure-directories-exist output)
    (with-open-file (stream output :direction :output :if-exists :supersede)
      (format stream ";; Fixed-target serialized completion; milliseconds; nearest-rank percentiles.~%")
      (luv:call-with-sdl-main-thread
       (lambda ()
         (dotimes (repeat (env-integer "BENCH_REPEATS" 1))
           (dolist (scene scenes)
             (assert (member scene '(:alpine :meadow :cave :balls :turtle :terminal :companion :live-alpine :live-meadow :atelier)))
             (dolist (size sizes)
               (assert (and (= 2 (length size)) (every #'plusp size)))
               (dolist (backend (if (eq scene :atelier) '(:atelier) '(:luft :old)))
                 (let ((result (append (list :repeat (1+ repeat) :host (machine-instance)
                                            :source-revision *source-revision*
                                            :frames (env-integer "BENCH_FRAMES" 300)
                                            :warmup (env-integer "BENCH_WARMUP" 60))
                                       (run-case scene backend (first size) (second size)))))
                   (print result stream) (terpri stream) (finish-output stream)
                   (format t "~&RESULT ~S~%" result) (finish-output)))))))))))

(defun main () (call-with-phase-profiling #'run))

(unless (uiop:getenv "BENCH_LIBRARY") (main))
