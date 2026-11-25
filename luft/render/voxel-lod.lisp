(in-package #:luft.render)

;;; Live Level-0 (stride = 2) 3D coarse voxel LOD residency (#4R3Y36).
;;;
;;; Reuses the renderer's pooled TERRAIN-ARENAS (SITES, APPEARANCES, LIGHTS)
;;; and unified visibility buffer rather than the 2.5D distant-quads arena.
;;; Coarse slices live on an independent replacement lifecycle so background
;;; tile arrivals never invalidate MetalFX temporal history.

(defparameter *voxel-lod-p* nil
  "Render Level-0 (stride = 2) 3D beveled coarse voxel stars beyond near chunks.")

(defparameter *voxel-lod-occupancy-threshold* 4
  "Minimum solid subcells (1..8) in a 2x2x2 block for coarse solidity.")

(defparameter *voxel-lod-cull-caves-p* t
  "Plug sealed deep-underground pockets unreachable from exterior air (#CLIAOO).")

(defparameter *voxel-lod-frame-budget* 2
  "Maximum stride-2 tile publications per update. The interactive worker produces one result at a time.")

(declaim (inline %voxel-lod-tile-key))
(defun %voxel-lod-tile-key (tx ty)
  (declare (type fixnum tx ty))
  (logior (ash (logand (+ tx #x100000) #x1FFFFF) 21)
          (logand (+ ty #x100000) #x1FFFFF)))

(defstruct (voxel-lod-gpu-tile (:copier nil))
  (tx 0 :type fixnum :read-only t)
  (ty 0 :type fixnum :read-only t)
  (resident nil)
  (star-count 0 :type fixnum)
  (neighbor-mask 0 :type (unsigned-byte 8))
  (near-mask 0 :type (unsigned-byte 4))
  (dirty-p nil :type boolean))

(defclass voxel-lod-state (gpu-resource-owner)
  ((renderer :initarg :renderer :reader voxel-lod-renderer)
   (source :initform nil :accessor voxel-lod-source)
   (worker :initform nil :accessor voxel-lod-worker)
   (pending :initform nil :accessor voxel-lod-pending)
   (edits :initform nil :accessor voxel-lod-edits)
   (stride-four :initform nil :accessor voxel-lod-stride-four)
   (coarse-cache :initform (make-hash-table :test #'eql) :reader voxel-lod-coarse-cache)
   (gpu-tiles :initform (make-hash-table :test #'eql) :reader voxel-lod-gpu-tiles)
   (active-bounds :initform nil :accessor voxel-lod-active-bounds)
   (last-threshold :initform *voxel-lod-occupancy-threshold* :accessor voxel-lod-last-threshold)
   (last-cull-caves-p :initform *voxel-lod-cull-caves-p* :accessor voxel-lod-last-cull-caves-p)
   (last-journal-head :initform -1 :accessor voxel-lod-last-journal-head)
   (stride-two-scratch :initform nil :accessor voxel-lod-stride-two-scratch)
   (stride-four-scratch :initform nil :accessor voxel-lod-stride-four-scratch)))

(defun make-voxel-lod-state (renderer)
  (make-instance 'voxel-lod-state :renderer renderer))

(defun %retire-voxel-lod-gpu-tile (state key tile &key immediate-p)
  (declare (ignore immediate-p))
  (remhash key (voxel-lod-gpu-tiles state))
  (let ((resident (voxel-lod-gpu-tile-resident tile)))
    (when resident
      (setf (voxel-lod-gpu-tile-resident tile) nil)
      (with-release-warnings
        (releasing :retired-voxel-lod-tile
          (release-owned-gpu-object state resident))))))

(defun clear-voxel-lod-gpu-tiles (state)
  "Release every active GPU coarse-star tile while optionally keeping CPU coarse cache."
  (when state
    (clear-stride-four-ring state)
    (let ((entries nil))
      (maphash (lambda (k tile) (push (cons k tile) entries))
               (voxel-lod-gpu-tiles state))
      (dolist (entry entries)
        (%retire-voxel-lod-gpu-tile state (car entry) (cdr entry))))
    (setf (voxel-lod-active-bounds state) nil (voxel-lod-pending state) nil)))

(defun clear-voxel-lod (state)
  "Evict all GPU tiles and cached coarse voxel tiles."
  (when state
    (clear-voxel-lod-gpu-tiles state)
    (clrhash (voxel-lod-coarse-cache state))
    (setf (voxel-lod-last-journal-head state) -1 (voxel-lod-edits state) nil)
    (setf (voxel-lod-stride-two-scratch state) nil (voxel-lod-stride-four-scratch state) nil)))

(defmethod destroy ((state voxel-lod-state))
  (clear-voxel-lod state)
  (when (voxel-lod-worker state)
    (production:stop-production-system (voxel-lod-worker state))
    (setf (voxel-lod-worker state) nil))
  (stop-stride-four-worker state)
  (call-next-method))

(defun %upload-prepared-coarse-population (renderer instances appearances runs lights translucent-runs)
  (let* ((arenas (renderer-terrain-arenas renderer))
         (resident (make-instance 'resident-population :arenas arenas))
         (completed-p nil))
    (unwind-protect
         (with-gpu-construction (resident)
           (let ((site-count (floor (length instances) 4)))
             (multiple-value-bind (site-slice appearance-slice light-slice)
                 (terrain-arenas-allocate-sites arenas site-count)
               (setf (resident-population-site-slice resident) site-slice
                     (resident-population-appearance-slice resident) appearance-slice
                     (resident-population-light-slice resident) light-slice)
               (when (plusp site-count)
                 (gpu-buffer-arena-write (terrain-arenas-sites-arena arenas)
                                         site-slice instances)
                 (gpu-buffer-arena-write (terrain-arenas-appearances-arena arenas)
                                         appearance-slice appearances)
                 (gpu-buffer-arena-write (terrain-arenas-lights-arena arenas)
                                         light-slice lights)))
             (setf (resident-population-star-runs resident) runs
                   (resident-population-translucent-runs resident) translucent-runs
                   (resident-population-bounds resident) (site-bounds instances)))
           (setf completed-p t)
           resident)
      (unless completed-p
        (%free-resident-slices resident :immediate-p t)))))

(defun %voxel-lod-neighbor-mask (coarse-cache tx ty)
  (let ((mask 0)
        (bit 0))
    (loop for dx from -1 to 1 do
      (loop for dy from -1 to 1
            unless (and (zerop dx) (zerop dy))
              do (when (gethash (%voxel-lod-tile-key (+ tx dx) (+ ty dy)) coarse-cache)
                   (setf mask (logior mask (ash 1 bit))))
                 (incf bit)))
    mask))

(defun %voxel-lod-near-mask (mesh-slots tx ty)
  (logior (if (distant-level0-covered-by-near-p (1- tx) ty mesh-slots) 1 0)
          (if (distant-level0-covered-by-near-p (1+ tx) ty mesh-slots) 2 0)
          (if (distant-level0-covered-by-near-p tx (1- ty) mesh-slots) 4 0)
          (if (distant-level0-covered-by-near-p tx (1+ ty) mesh-slots) 8 0)))

(defun %invalidate-edited-coarse-tiles (state source)
  "Mark coarse tiles whose chunk saw journal edits since LAST-JOURNAL-HEAD dirty."
  (let* ((journal (world-source-journal source))
         (head (world-journal-head journal))
         (prev (voxel-lod-last-journal-head state)))
    (when (or (/= head prev) (null (voxel-lod-edits state)))
      (setf (voxel-lod-pending state) nil
            (voxel-lod-edits state) (alexandria:copy-hash-table (world-source-edits source))))
    (when (and (>= prev 0) (/= head prev))
      (let* ((coarse-cache (voxel-lod-coarse-cache state))
             (gpu-tiles (voxel-lod-gpu-tiles state))
             (events (world-journal-events journal))
             (watermark (world-journal-watermark journal))
             (start (max 0 (- prev watermark)))
             (touched-keys nil))
        (cond
          ((not (<= watermark prev head))
           ;; PREV is outside the retained history.
           (clrhash coarse-cache)
           (maphash (lambda (k tile)
                      (declare (ignore k))
                      (setf (voxel-lod-gpu-tile-dirty-p tile) t))
                    gpu-tiles))
          (t
           (loop for i from start below (length events)
                 for ev = (aref events i)
                 for site = (world-event-site ev)
                 for tx = (ash (luft:site-x site) (- luft:+chunk-bits+))
                 for ty = (ash (luft:site-y site) (- luft:+chunk-bits+))
                 do (pushnew (cons tx ty) touched-keys :test #'equal))
           (dolist (pair touched-keys)
             (let ((tx (car pair))
                   (ty (cdr pair)))
               (remhash (%voxel-lod-tile-key tx ty) coarse-cache)
               (loop for dx from -1 to 1 do
                 (loop for dy from -1 to 1
                       for tile = (gethash (%voxel-lod-tile-key (+ tx dx) (+ ty dy)) gpu-tiles)
                       when tile
                         do (setf (voxel-lod-gpu-tile-dirty-p tile) t)))))))))
    (setf (voxel-lod-last-journal-head state) head)))

(defclass stride-two-request (production:production-request)
  ((tx :initarg :tx :reader stride-two-tx)
   (ty :initarg :ty :reader stride-two-ty)
   (seams :initarg :seams :reader stride-two-seams)
   (build :initarg :build :reader stride-two-build)))

(defmethod production:perform-production-request ((request stride-two-request))
  (funcall (stride-two-build request)))

(defun stride-two-seam-mask (tx ty bounds mesh-slots)
  (let ((near (%voxel-lod-near-mask mesh-slots tx ty)))
    (logior near
            (if (and (= tx (aref bounds 1)) (not (logbitp 1 near))) 16 0)
            (if (and (= ty (aref bounds 3)) (not (logbitp 3 near))) 32 0))))

(defun make-stride-two-request (state tx ty seams descriptors use-scratch-p)
  ;; Capture inputs on the owner. The worker never looks into live residency or
  ;; the live journal. Scratch is exclusive to this producer once it starts.
  (let* ((source (voxel-lod-source state))
         (edits (voxel-lod-edits state)) (head (voxel-lod-last-journal-head state))
         (threshold (max 1 (min 8 *voxel-lod-occupancy-threshold*)))
         (cull-p *voxel-lod-cull-caves-p*)
         (cache (voxel-lod-coarse-cache state)) (key (%voxel-lod-tile-key tx ty))
         (cached (gethash key cache)) (neighbors (make-array '(3 3)))
         (scratch (when (and use-scratch-p (null cached))
                    (or (voxel-lod-stride-two-scratch state)
                        (setf (voxel-lod-stride-two-scratch state)
                              (make-coarse-tile-scratch (world-source-domain source)))))))
    (dotimes (i 3)
      (dotimes (j 3)
        (setf (aref neighbors i j) (gethash (%voxel-lod-tile-key (+ tx i -1) (+ ty j -1)) cache))))
    (make-instance
     'stride-two-request :key key :tx tx :ty ty :seams seams :build
     (lambda ()
       (let ((tile (or cached
                       (build-world-source-coarse-tile
                        source tx ty :threshold threshold :cull-interior-caves-p cull-p
                        :scratch scratch
                        :edits edits :head head))))
         (multiple-value-bind (sites appearances lights stars)
             (mesh-coarse-voxel-tile
              tile (lambda (dx dy) (aref neighbors (1+ dx) (1+ dy))) descriptors
              :near-xm-p (logbitp 0 seams) :near-xp-p (logbitp 1 seams)
              :near-ym-p (logbitp 2 seams) :near-yp-p (logbitp 3 seams)
              :close-x-plus-p (logbitp 4 seams) :close-y-plus-p (logbitp 5 seams))
           (list* tile stars (multiple-value-list
                              (sort-sites-by-star sites (pack-terrain-appearance-codes appearances) lights nil)))))))))

(defun publish-stride-two-tile (state request value)
  (destructuring-bind (tile stars &rest packed) value
    (let* ((key (production:production-request-key request))
           (tx (stride-two-tx request)) (ty (stride-two-ty request))
           (cache (voxel-lod-coarse-cache state)) (gpu (voxel-lod-gpu-tiles state))
           (old (gethash key gpu))
           (resident (when (plusp stars)
                       (own-gpu-object state (apply #'%upload-prepared-coarse-population
                                                    (voxel-lod-renderer state) packed)))))
      (unless (gethash key cache)
        (loop for dx from -1 to 1 do
          (loop for dy from -1 to 1
                for neighbor = (gethash (%voxel-lod-tile-key (+ tx dx) (+ ty dy)) gpu)
                when neighbor do (setf (voxel-lod-gpu-tile-dirty-p neighbor) t))))
      (when old (%retire-voxel-lod-gpu-tile state key old))
      (setf (gethash key cache) tile
            (gethash key gpu) (make-voxel-lod-gpu-tile
                               :tx tx :ty ty :resident resident :star-count stars
                               :near-mask (logand 15 (stride-two-seams request))
                               :neighbor-mask (%voxel-lod-neighbor-mask cache tx ty))))))

(defun drain-stride-two-worker (state bounds mesh-slots)
  (let* ((worker (voxel-lod-worker state))
         (result (when worker (production:receive-production-result-no-hang worker))))
    (when result
      (let* ((request (production:production-result-request result))
             (tx (stride-two-tx request)) (ty (stride-two-ty request)))
        (when (eq request (voxel-lod-pending state))
          (setf (voxel-lod-pending state) nil)
          (when (and (<= (aref bounds 0) tx (aref bounds 1))
                     (<= (aref bounds 2) ty (aref bounds 3))
                     (not (distant-level0-covered-by-near-p tx ty mesh-slots))
                     (= (stride-two-seams request) (stride-two-seam-mask tx ty bounds mesh-slots)))
            (when (production:production-result-condition result)
              (error (production:production-result-condition result)))
            (publish-stride-two-tile state request (production:production-result-value result))
            t))))))

(defun retarget-stride-two-tiles (state domain x y)
  (multiple-value-bind (xmin xmax ymin ymax) (distant-ring-bounds 0 x y)
    (let* ((bounds (vector (max 0 xmin) (min xmax (1- (floor (luft:world-domain-x-limit domain) 64)))
                           (max 0 ymin) (min ymax (1- (floor (luft:world-domain-y-limit domain) 64)))))
           (previous (voxel-lod-active-bounds state)))
      (unless (equalp previous bounds)
        (maphash
         (lambda (key tile)
           (let ((tx (voxel-lod-gpu-tile-tx tile)) (ty (voxel-lod-gpu-tile-ty tile)))
             (cond
               ((not (and (<= (1- (aref bounds 0)) tx (1+ (aref bounds 1)))
                          (<= (1- (aref bounds 2)) ty (1+ (aref bounds 3)))))
                (%retire-voxel-lod-gpu-tile state key tile))
               ((and previous
                     (or (not (eq (= tx (aref bounds 1)) (= tx (aref previous 1))))
                         (not (eq (= ty (aref bounds 3)) (= ty (aref previous 3))))))
                (setf (voxel-lod-gpu-tile-dirty-p tile) t)))))
         (voxel-lod-gpu-tiles state))
        (setf (voxel-lod-active-bounds state) bounds))
      bounds)))

(defun stride-two-candidates (state bounds mesh-slots x y)
  (let ((volumes nil) (aprons nil))
    (loop for ty from (aref bounds 2) to (aref bounds 3) do
      (loop for tx from (aref bounds 0) to (aref bounds 1)
            unless (distant-level0-covered-by-near-p tx ty mesh-slots)
              do (let* ((key (%voxel-lod-tile-key tx ty)) (tile (gethash key (voxel-lod-gpu-tiles state)))
                        (seams (stride-two-seam-mask tx ty bounds mesh-slots)))
                   (when (or (null tile) (voxel-lod-gpu-tile-dirty-p tile)
                             (/= (logand 15 seams) (voxel-lod-gpu-tile-near-mask tile)))
                     (let* ((dx (- (+ (* tx 64) 32) x)) (dy (- (+ (* ty 64) 32) y))
                            (item (list (+ (* dx dx) (* dy dy)) tx ty seams)))
                       ;; A clamped apron already draws. Give new and edited
                       ;; volumes precedence, then refresh their seams.
                       (if (or (null tile) (null (gethash key (voxel-lod-coarse-cache state))))
                           (push item volumes)
                           (push item aprons)))))))
    (nconc (sort volumes #'< :key #'first) (sort aprons #'< :key #'first))))

(defun update-renderer-voxel-lod
    (renderer source camera-x camera-y &key (budget *voxel-lod-frame-budget*) background-p)
  "Publish at most BUDGET stride-two tiles. Interactive hosts use a single CPU
producer. Explicit updates and startup preroll can fill synchronously."
  (let ((state (and renderer (renderer-voxel-lod renderer))))
    (unless (and state *voxel-lod-p* source (world-source-distant-terrain-p source))
      (when state (clear-voxel-lod-gpu-tiles state))
      (return-from update-renderer-voxel-lod 0))
    (unless (and (eq source (voxel-lod-source state))
                 (= *voxel-lod-occupancy-threshold* (voxel-lod-last-threshold state))
                 (eq *voxel-lod-cull-caves-p* (voxel-lod-last-cull-caves-p state)))
      (clear-voxel-lod state)
      (setf (voxel-lod-source state) source
            (voxel-lod-last-threshold state) *voxel-lod-occupancy-threshold*
            (voxel-lod-last-cull-caves-p state) *voxel-lod-cull-caves-p*))
    (%invalidate-edited-coarse-tiles state source)
    (update-stride-four-ring renderer state source camera-x camera-y)
    (let* ((bounds (retarget-stride-two-tiles state (world-source-domain source) camera-x camera-y))
           (mesh-slots (renderer-mesh-slots renderer))
           (published (if (and (plusp budget) (drain-stride-two-worker state bounds mesh-slots)) 1 0))
           (worker (voxel-lod-worker state)))
      (when (or (>= published budget)
                (and background-p worker
                     (or (voxel-lod-pending state) (plusp (production:production-system-pending-count worker)))))
        (return-from update-renderer-voxel-lod published))
      (let ((candidates (stride-two-candidates state bounds mesh-slots camera-x camera-y)))
        (when candidates
          (let ((descriptors (compile-terrain-material-descriptors (world-source-material-vocabulary source))))
            (terrain-arenas-ensure-descriptors (renderer-terrain-arenas renderer) descriptors)
            (dolist (candidate candidates)
              (destructuring-bind (distance tx ty seams) candidate
                (declare (ignore distance))
                (let ((request (make-stride-two-request state tx ty seams descriptors (or background-p (null worker)))))
                  (when background-p
                    (unless worker
                      (setf worker (production:make-single-worker-production-system :name "Luft stride-two terrain")
                            (voxel-lod-worker state) worker))
                    (production:schedule-production-request worker request)
                    (setf (voxel-lod-pending state) request)
                    (return))
                  ;; A synchronous update can supersede an outstanding request.
                  (setf (voxel-lod-pending state) nil)
                  (publish-stride-two-tile state request (production:perform-production-request request))
                  (when (>= (incf published) budget) (return))))))))
      published)))

(defun renderer-voxel-lod-visible-residents (renderer)
  "Active Level-0 coarse RESIDENT-POPULATION objects passing frustum culling."
  (let ((state (and *voxel-lod-p* renderer (renderer-voxel-lod renderer))))
    (when state
      (let ((bounds (voxel-lod-active-bounds state))
            (gpu-tiles (voxel-lod-gpu-tiles state))
            (mesh-slots (renderer-mesh-slots renderer))
            (visible nil))
        (when bounds
          (loop for ty from (aref bounds 2) to (aref bounds 3) do
            (loop for tx from (aref bounds 0) to (aref bounds 1)
                  unless (distant-level0-covered-by-near-p tx ty mesh-slots)
                    do (let* ((tile (gethash (%voxel-lod-tile-key tx ty) gpu-tiles))
                              (resident (and tile (voxel-lod-gpu-tile-resident tile))))
                         (when (and resident (resident-drawn-p resident nil))
                           (push resident visible))))))
        (nconc (nreverse visible) (stride-four-visible-residents state))))))

(defun voxel-lod-tile-present-p (renderer tx ty)
  "Whether Level-0 coarse 3D tile (TX, TY) is ready to replace heightfield L0."
  (let ((state (and *voxel-lod-p* renderer (renderer-voxel-lod renderer))))
    (and state
         (nth-value 1 (gethash (%voxel-lod-tile-key tx ty) (voxel-lod-gpu-tiles state)))
         t)))

(defun voxel-lod-stats (renderer)
  "Return an inspectable plist of Level-0 coarse voxel LOD stats."
  (let ((state (and renderer (renderer-voxel-lod renderer))))
    (when state
      (let ((total-stars 0))
        (maphash (lambda (k tile)
                   (declare (ignore k))
                   (incf total-stars (voxel-lod-gpu-tile-star-count tile)))
                 (voxel-lod-gpu-tiles state))
        (list :stride-four (stride-four-ring-stats state)
              :enabled *voxel-lod-p*
              :threshold *voxel-lod-occupancy-threshold*
              :cull-caves-p *voxel-lod-cull-caves-p*
              :gpu-tile-count (hash-table-count (voxel-lod-gpu-tiles state))
              :cached-coarse-count (hash-table-count (voxel-lod-coarse-cache state))
              :total-stars total-stars)))))
