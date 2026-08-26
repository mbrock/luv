(defpackage #:luft.mesh-query-profile
  (:use #:cl)
  (:export #:run-mesh-query-profile))

(in-package #:luft.mesh-query-profile)

(defparameter *profile-phases*
  '(:aggregate :occupancy :halo-index :z-bounds :selection
    :planning :materials :projection))

(defvar *profile-sink* nil)

(defstruct (mesh-query-profile-case
             (:constructor %make-mesh-query-profile-case))
  scene
  store
  chunk
  (chunk-key 0 :type fixnum)
  domain
  stock-function
  chamfer-stock-function
  algebra
  (grid-x 0 :type fixnum)
  (grid-y 0 :type fixnum)
  (x0 0 :type fixnum)
  (x1 0 :type fixnum)
  (y0 0 :type fixnum)
  (y1 0 :type fixnum)
  (ox1 0 :type fixnum)
  (oy1 0 :type fixnum)
  field
  halo-chunks
  halo-tables
  z0
  z1
  packed-sites
  workspace
  (material-count 0 :type fixnum)
  (singular-count 0 :type fixnum)
  output)

(defstruct phase-measurement
  phase
  (iterations 0 :type fixnum)
  (seconds 0d0 :type double-float)
  (bytes 0 :type integer))

(defun %largest-streaming-chunk (store)
  (let ((best-key nil)
        (best-chunk nil)
        (best-count -1))
    (maphash
     (lambda (key chunk)
       (let ((count (luft:chain-count chunk)))
         (when (or (> count best-count)
                   (and (= count best-count)
                        (or (null best-key) (< key best-key))))
           (setf best-key key
                 best-chunk chunk
                 best-count count))))
     store)
    (unless best-chunk
      (error "The mountain sanctuary has no streaming chunks."))
    (values best-key best-chunk)))

(defun %call-with-boundary-resolution (case function)
  (handler-bind
      ((luft:missing-chunk
         (lambda (condition)
           (multiple-value-bind (chunk present-p)
               (gethash (luft:missing-chunk-key condition)
                        (mesh-query-profile-case-store case))
             (if present-p
                 (invoke-restart 'luft:use-chunk chunk)
                 (invoke-restart 'luft:treat-as-air)))))
       (luft:outside-domain
         (lambda (condition)
           (declare (ignore condition))
           (invoke-restart 'luft:treat-as-air))))
    (funcall function)))

(defun %run-occupancy (case)
  (setf (mesh-query-profile-case-field case)
        (luft::%materialize-occupancy
         (mesh-query-profile-case-chunk case)
         (mesh-query-profile-case-x0 case)
         (mesh-query-profile-case-x1 case)
         (mesh-query-profile-case-y0 case)
         (mesh-query-profile-case-y1 case))))

(defun %run-halo-index (case)
  (setf (mesh-query-profile-case-halo-tables case)
        (loop for chunk in (mesh-query-profile-case-halo-chunks case)
              collect (luft::%chunk-cells-table chunk))))

(defun %run-z-bounds (case)
  (%call-with-boundary-resolution
   case
   (lambda ()
     (multiple-value-bind (z0 z1)
         (luft::%width-one-chunk-star-z-bounds
          (mesh-query-profile-case-chunk case)
          (mesh-query-profile-case-field case)
          (mesh-query-profile-case-grid-x case)
          (mesh-query-profile-case-grid-y case)
          (mesh-query-profile-case-x0 case)
          (mesh-query-profile-case-x1 case)
          (mesh-query-profile-case-y0 case)
          (mesh-query-profile-case-y1 case))
       (setf (mesh-query-profile-case-z0 case) z0
             (mesh-query-profile-case-z1 case) z1)
       z0))))

(defun %run-selection (case)
  (let ((sites (mesh-query-profile-case-packed-sites case)))
    (setf (fill-pointer sites) 0)
    (when (mesh-query-profile-case-z0 case)
      (%call-with-boundary-resolution
       case
       (lambda ()
         (luft::%gather-width-one-query-sites
          (mesh-query-profile-case-field case)
          (mesh-query-profile-case-domain case)
          (mesh-query-profile-case-x0 case)
          (mesh-query-profile-case-x1 case)
          (mesh-query-profile-case-y0 case)
          (mesh-query-profile-case-y1 case)
          :air
          (mesh-query-profile-case-z0 case)
          (mesh-query-profile-case-z1 case)
          sites))))
    sites))

(defun %run-planning (case)
  (let ((sites (mesh-query-profile-case-packed-sites case))
        (workspace (mesh-query-profile-case-workspace case)))
    (luft::%prepare-width-one-query-relations workspace (length sites))
    (multiple-value-bind (material-count singular-count)
        (luft::%plan-width-one-query
         sites workspace
         (mesh-query-profile-case-x0 case)
         (mesh-query-profile-case-x1 case)
         (mesh-query-profile-case-y0 case)
         (mesh-query-profile-case-y1 case)
         (mesh-query-profile-case-ox1 case)
         (mesh-query-profile-case-oy1 case))
      (setf (mesh-query-profile-case-material-count case) material-count
            (mesh-query-profile-case-singular-count case) singular-count)
      workspace)))

(defun %run-materials (case)
  (luft::%materialize-width-one-query-materials
   (mesh-query-profile-case-workspace case)
   (mesh-query-profile-case-domain case)
   (mesh-query-profile-case-stock-function case)
   (mesh-query-profile-case-algebra case)
   (mesh-query-profile-case-x0 case)
   (mesh-query-profile-case-y0 case)
   (mesh-query-profile-case-material-count case)))

(defun %run-projection (case)
  (setf (mesh-query-profile-case-output case)
        (luft::%finish-width-one-query
         (mesh-query-profile-case-workspace case)
         (mesh-query-profile-case-domain case)
         (mesh-query-profile-case-algebra case)
         (mesh-query-profile-case-singular-count case)
         (mesh-query-profile-case-x0 case)
         (mesh-query-profile-case-y0 case))))

(defun %run-aggregate (case)
  (%call-with-boundary-resolution
   case
   (lambda ()
     (setf (mesh-query-profile-case-output case)
           (luft:mesh-chunk
            (mesh-query-profile-case-chunk case)
            (mesh-query-profile-case-chunk-key case)
            :source-stock-function
            (mesh-query-profile-case-stock-function case)
            :chamfer-stock-function
            (mesh-query-profile-case-chamfer-stock-function case)
            :chamfer-algebra (mesh-query-profile-case-algebra case)
            :outside-domain-policy :air
            :bevel-width 1)))))

(defun %phase-function (phase)
  (ecase phase
    (:aggregate #'%run-aggregate)
    (:occupancy #'%run-occupancy)
    (:halo-index #'%run-halo-index)
    (:z-bounds #'%run-z-bounds)
    (:selection #'%run-selection)
    (:planning #'%run-planning)
    (:materials #'%run-materials)
    (:projection #'%run-projection)))

(defun %run-iterations (case function iterations)
  (dotimes (iteration iterations)
    (declare (ignore iteration))
    (setf *profile-sink* (funcall function case))))

(defun %elapsed-seconds (start)
  (/ (- (get-internal-real-time) start)
     (coerce internal-time-units-per-second 'double-float)))

(defun %time-iterations (case function iterations)
  (let ((bytes-before (sb-ext:get-bytes-consed))
        (start (get-internal-real-time)))
    (%run-iterations case function iterations)
    (values (%elapsed-seconds start)
            (- (sb-ext:get-bytes-consed) bytes-before))))

(defun %calibrate-iterations (case function target-seconds)
  (let ((threshold (max 0.001d0 (min 0.02d0 (/ target-seconds 3d0)))))
    (loop for iterations fixnum = 1 then (* iterations 2)
          do (multiple-value-bind (seconds bytes)
                 (%time-iterations case function iterations)
               (declare (ignore bytes))
               (when (or (>= seconds threshold)
                         (>= iterations 1048576))
                 (return
                   (max 1
                        (ceiling (* iterations target-seconds)
                                 (max seconds 1d-9)))))))))

(defun %measure-phase (case phase target-seconds)
  (let* ((function (%phase-function phase))
         (iterations (%calibrate-iterations case function target-seconds)))
    (sb-ext:gc :full t)
    (multiple-value-bind (seconds bytes)
        (%time-iterations case function iterations)
      (make-phase-measurement
       :phase phase :iterations iterations :seconds seconds :bytes bytes))))

(defun %phase-profile-pathname (directory phase)
  (merge-pathnames
   (make-pathname :name (string-downcase (symbol-name phase)) :type "txt")
   directory))

(defun %profile-phase
    (case measurement directory profile-seconds sample-interval)
  (let* ((phase (phase-measurement-phase measurement))
         (function (%phase-function phase))
         (seconds-per-iteration
           (/ (phase-measurement-seconds measurement)
              (phase-measurement-iterations measurement)))
         (iterations
           (max 1 (ceiling profile-seconds
                           (max seconds-per-iteration 1d-9))))
         (maximum-samples
           (max 1000 (ceiling (* 4d0 profile-seconds) sample-interval)))
         (start nil)
         (actual-seconds 0d0))
    (format t "Profiling ~A for ~:D iterations...~%" phase iterations)
    (sb-ext:gc :full t)
    (sb-sprof:reset)
    (unwind-protect
         (progn
           (sb-sprof:start-profiling
            :mode :cpu
            :sample-interval sample-interval
            :max-samples maximum-samples
            :threads (list sb-thread:*current-thread*))
           (setf start (get-internal-real-time))
           (%run-iterations case function iterations)
           (setf actual-seconds (%elapsed-seconds start)))
      (sb-sprof:stop-profiling))
    (with-open-file
        (stream (%phase-profile-pathname directory phase)
                :direction :output :if-exists :supersede
                :if-does-not-exist :create)
      (format stream
              "LUFT mesh query phase: ~A~%~
               Requested sampling time: ~,3F seconds~%~
               Actual workload time: ~,3F seconds~%~
               Workload iterations: ~:D~%~
               Sample interval: ~,6F seconds~2%~
               Flat report sorted by self samples~%~
               ==================================~2%"
              phase profile-seconds actual-seconds iterations sample-interval)
      (sb-sprof:report :type :flat :max 100 :sort-by :samples
                       :stream stream :show-progress nil)
      (format stream
              "~2%Flat report sorted by cumulative samples~%~
               ========================================~2%")
      (sb-sprof:report :type :flat :max 100 :sort-by :cumulative-samples
                       :stream stream :show-progress nil))))

(defun %make-profile-case ()
  (let* ((scene (luft.render::make-streaming-scene
                 (luft.render::make-mountain-sanctuary-scene)))
         (store (luft.render::streaming-scene-store scene))
         (program (luft.render::scene-material-program scene)))
    (multiple-value-bind (chunk-key chunk)
        (%largest-streaming-chunk store)
      (let* ((domain (luft:chain-domain chunk))
             (grid-x (luft:chunk-key-x chunk-key))
             (grid-y (luft:chunk-key-y chunk-key))
             (x0 (luft:chunk-origin-x chunk-key))
             (y0 (luft:chunk-origin-y chunk-key))
             (x1 (min (+ x0 luft:+chunk-size+)
                      (luft:world-domain-x-limit domain)))
             (y1 (min (+ y0 luft:+chunk-size+)
                      (luft:world-domain-y-limit domain)))
             (ox1 (if (>= (+ x0 luft:+chunk-size+)
                          (luft:world-domain-x-limit domain))
                      (1+ (luft:world-domain-x-limit domain))
                      (+ x0 luft:+chunk-size+)))
             (oy1 (if (>= (+ y0 luft:+chunk-size+)
                          (luft:world-domain-y-limit domain))
                      (1+ (luft:world-domain-y-limit domain))
                      (+ y0 luft:+chunk-size+)))
             (halo-chunks
               (loop for (dx dy) in '((-1 0) (0 -1) (-1 -1))
                     for nx = (+ grid-x dx)
                     for ny = (+ grid-y dy)
                     for neighbor =
                       (and (<= 0 nx) (<= 0 ny)
                            (gethash (luft::%chunk-morton nx ny) store))
                     when neighbor collect neighbor))
             (case
               (%make-mesh-query-profile-case
                :scene scene :store store :chunk chunk :chunk-key chunk-key
                :domain domain
                :stock-function
                (luft.render::make-scene-face-stock-function scene)
                :chamfer-stock-function
                (luft.render::make-compiled-material-chamfer-stock-function
                 program)
                :algebra
                (luft.render::material-program-chamfer-algebra program)
                :grid-x grid-x :grid-y grid-y
                :x0 x0 :x1 x1 :y0 y0 :y1 y1 :ox1 ox1 :oy1 oy1
                :halo-chunks halo-chunks
                :packed-sites
                (luft::%borrow-width-one-sites
                 (max 16 (* 8 (luft:chain-count chunk))))
                :workspace (luft::%borrow-width-one-query-workspace))))
        (%run-occupancy case)
        (%run-z-bounds case)
        (%run-selection case)
        (%run-planning case)
        (%run-materials case)
        (%run-projection case)
        case))))

(defun %milliseconds-per-iteration (measurement)
  (* 1000d0
     (/ (phase-measurement-seconds measurement)
        (phase-measurement-iterations measurement))))

(defun %bytes-per-iteration (measurement)
  (round (phase-measurement-bytes measurement)
         (phase-measurement-iterations measurement)))

(defun %write-summary
    (stream case measurements timing-seconds profile-seconds sample-interval)
  (let* ((workspace (mesh-query-profile-case-workspace case))
         (faces (luft::width-one-query-workspace-faces workspace))
         (bands (luft::width-one-query-workspace-bands workspace))
         (fans (luft::width-one-query-workspace-fans workspace))
         (aggregate (find :aggregate measurements
                          :key #'phase-measurement-phase))
         (aggregate-ms (%milliseconds-per-iteration aggregate)))
    (format stream
            "LUFT mesh query statistical profile~2%~
             Runtime: ~A ~A on ~A~%~
             Workload: mountain-sanctuary chunk ~D (~D, ~D)~%~
             Cells: ~:D~%~
             Selected sites: ~:D~%~
             Relation rows: ~:D faces, ~:D bands, ~:D fans~%~
             Material rows: ~:D~%~
             Singular stars: ~:D~%~
             Output triangles: ~:D~2%~
             Timing target: ~,3F seconds per phase~%~
             Sampling target: ~,3F seconds per phase at ~,6F seconds~2%"
            (lisp-implementation-type) (lisp-implementation-version)
            (machine-type)
            (mesh-query-profile-case-chunk-key case)
            (mesh-query-profile-case-grid-x case)
            (mesh-query-profile-case-grid-y case)
            (luft:chain-count (mesh-query-profile-case-chunk case))
            (length (mesh-query-profile-case-packed-sites case))
            (luft::width-one-query-faces-count faces)
            (luft::width-one-query-patches-count bands)
            (luft::width-one-query-patches-count fans)
            (mesh-query-profile-case-material-count case)
            (mesh-query-profile-case-singular-count case)
            (luft:surface-mesh-triangle-count
             (mesh-query-profile-case-output case))
            timing-seconds profile-seconds sample-interval)
    (format stream
            "Phase          ms/op    % aggregate     MiB/op   iterations~%~
             ------------- ------- --------------- ---------- -----------~%")
    (dolist (measurement measurements)
      (let ((milliseconds (%milliseconds-per-iteration measurement)))
        (format stream "~13A ~7,3F ~14,1F ~10,3F ~11:D~%"
                (string-downcase
                 (symbol-name (phase-measurement-phase measurement)))
                milliseconds
                (* 100d0 (/ milliseconds aggregate-ms))
                (/ (%bytes-per-iteration measurement) 1048576d0)
                (phase-measurement-iterations measurement))))
    (format stream
            "~%Aggregate reconstructs the complete query on every iteration.~%~
             Halo-index rebuilds the neighbor tables resolved during Z-bounds.~%~
             Isolated stages reuse prepared inputs and warmed workspace capacities;~%~
             their percentages expose attribution but need not sum to 100%.~%~
             Each phase report contains self-sorted and cumulative-sorted samples.~%")))

(defun %write-summary-csv (stream measurements)
  (format stream "phase,iterations,seconds,ms_per_op,bytes_per_op~%")
  (dolist (measurement measurements)
    (format stream "~(~A~),~D,~,6F,~,6F,~D~%"
            (phase-measurement-phase measurement)
            (phase-measurement-iterations measurement)
            (phase-measurement-seconds measurement)
            (%milliseconds-per-iteration measurement)
            (%bytes-per-iteration measurement))))

(defun run-mesh-query-profile
    (&key (output-directory "build/luft-mesher-profile")
          (profile-seconds 2d0)
          (sample-interval 0.0005d0)
          (timing-seconds 0.25d0))
  "Measure and statistically profile the production mesh query by phase."
  (check-type profile-seconds (double-float (0d0)))
  (check-type sample-interval (double-float (0d0)))
  (check-type timing-seconds (double-float (0d0)))
  (let ((directory (uiop:ensure-directory-pathname output-directory)))
    (ensure-directories-exist (merge-pathnames "summary.txt" directory))
    (luft:with-surface-mesh-workspace ()
      (let* ((case (%make-profile-case))
             (measurements
               (loop for phase in *profile-phases*
                     collect
                     (progn
                       (format t "Timing ~A...~%" phase)
                       (%measure-phase case phase timing-seconds)))))
        (dolist (measurement measurements)
          (%profile-phase case measurement directory
                          profile-seconds sample-interval))
        (let ((summary
                (with-output-to-string (stream)
                  (%write-summary stream case measurements
                                  timing-seconds profile-seconds
                                  sample-interval))))
          (write-string summary)
          (with-open-file (stream (merge-pathnames "summary.txt" directory)
                                  :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string summary stream)))
        (with-open-file (stream (merge-pathnames "summary.csv" directory)
                                :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
          (%write-summary-csv stream measurements))
        (format t "~%Reports written under ~A~%" (namestring directory))
        measurements))))
