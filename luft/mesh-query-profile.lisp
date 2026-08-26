(defpackage #:luft.mesh-query-profile
  (:use #:cl)
  (:export #:run-mesh-query-profile
           #:run-mesh-cohort-benchmark))

(in-package #:luft.mesh-query-profile)

(defparameter *profile-phases*
  '(:aggregate :chain-facts :occupancy :halo-index :z-bounds :selection
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

(defun %run-chain-facts (case)
  "Derive owner and halo chain facts uncached: the cold construction cost."
  (cons (luft::%derive-chain-chunk-facts (mesh-query-profile-case-chunk case))
        (loop for chunk in (mesh-query-profile-case-halo-chunks case)
              collect (luft::%derive-chain-chunk-facts chunk))))

(defun %run-occupancy (case)
  (setf (mesh-query-profile-case-field case)
        (luft::%chain-facts-occupancy-field
         (luft::%chain-chunk-facts (mesh-query-profile-case-chunk case))
         (mesh-query-profile-case-domain case)
         (mesh-query-profile-case-x0 case)
         (mesh-query-profile-case-x1 case)
         (mesh-query-profile-case-y0 case)
         (mesh-query-profile-case-y1 case))))

(defun %run-halo-index (case)
  (setf (mesh-query-profile-case-halo-tables case)
        (loop for chunk in (mesh-query-profile-case-halo-chunks case)
              collect (luft::%chain-chunk-facts chunk))))

(defun %run-z-bounds (case)
  (%call-with-boundary-resolution
   case
   (lambda ()
     (multiple-value-bind (z0 z1)
         (luft::%width-one-chunk-star-z-bounds
          (mesh-query-profile-case-chunk case)
          (mesh-query-profile-case-field case)
          (mesh-query-profile-case-grid-x case)
          (mesh-query-profile-case-grid-y case))
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
    (:chain-facts #'%run-chain-facts)
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
            "~%Aggregate reconstructs the complete query on every iteration~%~
             over the published chain-facts cache, so it is the warm cost.~%~
             Chain-facts derives owner and halo records uncached: the cold~%~
             cost paid once per new chain identity.  Occupancy, halo-index,~%~
             and Z-bounds consume the cache as production does.~%~
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

;;; ---------------------------------------------------------------------------
;;; Chain-facts cohort benchmark
;;;
;;; The chain-facts experiment is only proved by cohorts: a cold cohort pays
;;; every derivation, a warm cohort re-meshes EQ-identical chains against the
;;; published cache, and a one-edit cohort replaces exactly one chain and
;;; re-meshes the owners whose seams can see it.

(defstruct (cohort-run (:constructor %make-cohort-run))
  (label "" :type string)
  (chunks 0 :type fixnum)
  (seconds 0d0 :type double-float)
  (bytes 0 :type integer)
  (builds 0 :type fixnum)
  (hits 0 :type fixnum)
  (sites 0 :type fixnum)
  (faces 0 :type fixnum)
  (bands 0 :type fixnum)
  (fans 0 :type fixnum)
  (triangles 0 :type fixnum))

(defun %store-keys (store)
  (sort (loop for key being the hash-keys of store collect key) #'<))

(defun %run-mesh-cohort (label scene store keys)
  "Mesh every chunk in KEYS from STORE, recording reuse and output stats."
  (let* ((program (luft.render::scene-material-program scene))
         (stock-function (luft.render::make-scene-face-stock-function scene))
         (chamfer-stock-function
           (luft.render::make-compiled-material-chamfer-stock-function
            program))
         (algebra (luft.render::material-program-chamfer-algebra program))
         (sites 0) (faces 0) (bands 0) (fans 0) (triangles 0))
    (setf luft::*chain-facts-build-count* 0
          luft::*chain-facts-hit-count* 0)
    (sb-ext:gc :full t)
    (let ((bytes-before (sb-ext:get-bytes-consed))
          (start (get-internal-real-time)))
      (dolist (key keys)
        (let ((chunk (gethash key store)))
          (handler-bind
              ((luft:missing-chunk
                 (lambda (condition)
                   (multiple-value-bind (neighbor present-p)
                       (gethash (luft:missing-chunk-key condition) store)
                     (if present-p
                         (invoke-restart 'luft:use-chunk neighbor)
                         (invoke-restart 'luft:treat-as-air)))))
               (luft:outside-domain
                 (lambda (condition)
                   (declare (ignore condition))
                   (invoke-restart 'luft:treat-as-air))))
            (incf triangles
                  (luft:surface-mesh-triangle-count
                   (luft:mesh-chunk
                    chunk key
                    :source-stock-function stock-function
                    :chamfer-stock-function chamfer-stock-function
                    :chamfer-algebra algebra
                    :outside-domain-policy :air
                    :bevel-width 1))))
          (incf sites
                (length (luft::surface-mesh-workspace-width-one-sites
                         luft::*surface-mesh-workspace*)))
          (let ((workspace (luft::%borrow-width-one-query-workspace)))
            (incf faces
                  (luft::width-one-query-faces-count
                   (luft::width-one-query-workspace-faces workspace)))
            (incf bands
                  (luft::width-one-query-patches-count
                   (luft::width-one-query-workspace-bands workspace)))
            (incf fans
                  (luft::width-one-query-patches-count
                   (luft::width-one-query-workspace-fans workspace))))))
      (%make-cohort-run
       :label label :chunks (length keys)
       :seconds (%elapsed-seconds start)
       :bytes (- (sb-ext:get-bytes-consed) bytes-before)
       :builds luft::*chain-facts-build-count*
       :hits luft::*chain-facts-hit-count*
       :sites sites :faces faces :bands bands :fans fans
       :triangles triangles))))

(defun %cohort-edited-chain (chunk)
  "Cancel CHUNK's middle cell, returning the new chain and the removed site."
  (let* ((sites (luft:chain-sites chunk))
         (cell (aref sites (floor (length sites) 2)))
         (builder (luft:make-chain-builder (luft:chain-domain chunk)
                                           :initial-capacity 1)))
    (luft:chain-builder-add-site builder (luft:opposite-site cell))
    (values (luft:chain+ chunk (luft:finish-chain-builder builder)) cell)))

(defun %cohort-affected-keys (key store)
  "The resident owners whose width-one seams can see the chunk at KEY."
  (let ((grid-x (luft:chunk-key-x key))
        (grid-y (luft:chunk-key-y key)))
    (loop for (dx dy) in '((0 0) (1 0) (0 1) (1 1))
          for neighbor = (luft::%chunk-morton (+ grid-x dx) (+ grid-y dy))
          when (nth-value 1 (gethash neighbor store))
            collect neighbor)))

(defun %write-cohort-report (stream runs edited-key removed-cell)
  (format stream
          "LUFT mesher chain-facts cohort benchmark~2%~
           Runtime: ~A ~A on ~A~%~
           Fixture: mountain-sanctuary streaming store~%~
           Edit: cancelled cell ~D of chunk ~D, re-meshed the owners whose~%~
           seams can see it~2%"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) removed-cell edited-key)
  (format stream
          "Cohort     chunks ms-total ms/chunk    MiB builds   hits~:
   sites   faces   bands    fans triangles~%~
           ---------- ------ -------- -------- ------ ------ ------~:
 ------- ------- ------- ------- ---------~%")
  (dolist (run runs)
    (let ((milliseconds (* 1000d0 (cohort-run-seconds run))))
      (format stream
              "~10A ~6D ~8,2F ~8,2F ~6,1F ~6D ~6D ~7:D ~7:D ~7:D ~7:D ~9:D~%"
              (cohort-run-label run)
              (cohort-run-chunks run)
              milliseconds
              (/ milliseconds (max 1 (cohort-run-chunks run)))
              (/ (cohort-run-bytes run) 1048576d0)
              (cohort-run-builds run)
              (cohort-run-hits run)
              (cohort-run-sites run)
              (cohort-run-faces run)
              (cohort-run-bands run)
              (cohort-run-fans run)
              (cohort-run-triangles run))))
  (let ((cold (find "cold" runs :key #'cohort-run-label :test #'string=))
        (warms (remove-if-not
                (lambda (label) (eql 0 (search "warm" label)))
                runs :key #'cohort-run-label)))
    (format stream
            "~%Warm cohorts ~:[DIVERGE FROM~;reproduce~] the cold census ~
             (sites, rows, triangles).~%"
            (every (lambda (warm)
                     (and (= (cohort-run-sites cold) (cohort-run-sites warm))
                          (= (cohort-run-faces cold) (cohort-run-faces warm))
                          (= (cohort-run-bands cold) (cohort-run-bands warm))
                          (= (cohort-run-fans cold) (cohort-run-fans warm))
                          (= (cohort-run-triangles cold)
                             (cohort-run-triangles warm))))
                   warms))
    (format stream
            "Cold pays one facts build per resident chain; warm cohorts~%~
             build nothing; the edit builds exactly the one replaced chain.~%")))

(defun run-mesh-cohort-benchmark
    (&key (output "build/luft-mesher-cohort.txt") (warm-iterations 5))
  "Cold, warm, and one-edit cohort measurements of the production mesher."
  (check-type warm-iterations (integer 1))
  (luft:with-surface-mesh-workspace ()
    (let* ((scene (luft.render::make-streaming-scene
                   (luft.render::make-mountain-sanctuary-scene)))
           (store (luft.render::streaming-scene-store scene))
           (keys (%store-keys store))
           (runs '()))
      ;; Cold: no published facts survive.
      (luft::%reset-chain-facts :clear-cache t)
      (push (%run-mesh-cohort "cold" scene store keys) runs)
      (dotimes (iteration warm-iterations)
        (push (%run-mesh-cohort (format nil "warm-~D" (1+ iteration))
                                scene store keys)
              runs))
      (multiple-value-bind (edited-key chunk) (%largest-streaming-chunk store)
        (multiple-value-bind (edited removed) (%cohort-edited-chain chunk)
          (let ((edited-store (make-hash-table :test #'eql)))
            (maphash (lambda (key chain)
                       (setf (gethash key edited-store) chain))
                     store)
            (setf (gethash edited-key edited-store) edited)
            (push (%run-mesh-cohort
                   "one-edit" scene edited-store
                   (%cohort-affected-keys edited-key edited-store))
                  runs)
            (let ((report
                    (with-output-to-string (stream)
                      (%write-cohort-report
                       stream (reverse runs) edited-key removed))))
              (write-string report)
              (ensure-directories-exist output)
              (with-open-file (stream output
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (write-string report stream))
              (format t "~%Report written to ~A~%" output)
              (reverse runs))))))))
