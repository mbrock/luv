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
  "Maximum Level-0 coarse voxel tiles built/meshed per interactive frame.")

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
   (stride-four :initform nil :accessor voxel-lod-stride-four)
   (coarse-cache :initform (make-hash-table :test #'eql) :reader voxel-lod-coarse-cache)
   (gpu-tiles :initform (make-hash-table :test #'eql) :reader voxel-lod-gpu-tiles)
   (active-bounds :initform nil :accessor voxel-lod-active-bounds)
   (last-threshold :initform *voxel-lod-occupancy-threshold* :accessor voxel-lod-last-threshold)
   (last-cull-caves-p :initform *voxel-lod-cull-caves-p* :accessor voxel-lod-last-cull-caves-p)
   (last-journal-head :initform -1 :accessor voxel-lod-last-journal-head)
   (grid-scratch :initform (make-array (* 64 64 256) :element-type '(unsigned-byte 8)
                                                      :initial-element 0)
                 :reader voxel-lod-grid-scratch)
   (emission-scratch :initform (make-array (* 64 64 256) :element-type '(unsigned-byte 16)
                                                          :initial-element 0)
                     :reader voxel-lod-emission-scratch)
   (fibers-scratch :initform nil :accessor voxel-lod-fibers-scratch)))

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
    (setf (voxel-lod-active-bounds state) nil)))

(defun clear-voxel-lod (state)
  "Evict all GPU tiles and cached coarse voxel tiles."
  (when state
    (clear-voxel-lod-gpu-tiles state)
    (clrhash (voxel-lod-coarse-cache state))
    (setf (voxel-lod-last-journal-head state) -1)
    (setf (voxel-lod-fibers-scratch state) nil)))

(defmethod destroy ((state voxel-lod-state))
  (clear-voxel-lod state)
  (stop-stride-four-worker state)
  (call-next-method))

(defun %upload-coarse-star-population (renderer site-words appearance-codes light-words)
  "Sort and upload coarse star arrays directly into RENDERER's pooled TERRAIN-ARENAS."
  (declare (type (simple-array (unsigned-byte 32) (*)) site-words light-words)
           (type (simple-array (unsigned-byte 8) (*)) appearance-codes))
  (multiple-value-call #'%upload-prepared-coarse-population renderer
    (sort-sites-by-star site-words (pack-terrain-appearance-codes appearance-codes) light-words nil)))

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
    (when (and (>= prev 0) (/= head prev))
      (let* ((coarse-cache (voxel-lod-coarse-cache state))
             (gpu-tiles (voxel-lod-gpu-tiles state))
             (events (world-journal-events journal))
             (watermark (world-journal-watermark journal))
             (start (max 0 (- prev watermark)))
             (touched-keys nil))
        (cond
          ((< prev watermark)
           ;; Journal compacted past PREV: rebuild cache cleanly.
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

(defun update-renderer-voxel-lod
    (renderer source camera-x camera-y &key (budget *voxel-lod-frame-budget*))
  "Maintain Level-0 (stride = 2) coarse star tiles around (CAMERA-X, CAMERA-Y)."
  (let ((state (and renderer (renderer-voxel-lod renderer))))
    (unless (and state *voxel-lod-p* source (world-source-distant-terrain-p source))
      (when state
        (clear-voxel-lod-gpu-tiles state))
      (return-from update-renderer-voxel-lod 0))
    (unless (eq source (voxel-lod-source state))
      (clear-voxel-lod state)
      (setf (voxel-lod-source state) source))
    (when (or (/= *voxel-lod-occupancy-threshold* (voxel-lod-last-threshold state))
              (not (eq *voxel-lod-cull-caves-p* (voxel-lod-last-cull-caves-p state))))
      (clear-voxel-lod state)
      (setf (voxel-lod-last-threshold state) *voxel-lod-occupancy-threshold*
            (voxel-lod-last-cull-caves-p state) *voxel-lod-cull-caves-p*))
    (update-stride-four-ring renderer state source camera-x camera-y)
    (%invalidate-edited-coarse-tiles state source)
    (let* ((domain (world-source-domain source))
           (max-domain-tx (1- (floor (luft:world-domain-x-limit domain) luft:+chunk-size+)))
           (max-domain-ty (1- (floor (luft:world-domain-y-limit domain) luft:+chunk-size+)))
           (coarse-cache (voxel-lod-coarse-cache state))
           (gpu-tiles (voxel-lod-gpu-tiles state))
           (mesh-slots (renderer-mesh-slots renderer))
           (arenas (renderer-terrain-arenas renderer))
           (vocab (world-source-material-vocabulary source))
           (descriptors (compile-terrain-material-descriptors vocab)))
      (terrain-arenas-ensure-descriptors arenas descriptors)
      (multiple-value-bind (rmin-x rmax-x rmin-y rmax-y)
          (distant-ring-bounds 0 camera-x camera-y)
        (let ((min-x (max 0 rmin-x))
              (max-x (min max-domain-tx rmax-x))
              (min-y (max 0 rmin-y))
              (max-y (min max-domain-ty rmax-y)))
          (setf (voxel-lod-active-bounds state) (vector min-x max-x min-y max-y))
          ;; Evict GPU tiles outside 1-tile hysteresis margin or now covered by near chunks.
          (let ((evicted nil))
            (maphash (lambda (key tile)
                       (let ((tx (voxel-lod-gpu-tile-tx tile))
                             (ty (voxel-lod-gpu-tile-ty tile)))
                         (when (or (< tx (1- min-x)) (> tx (1+ max-x))
                                   (< ty (1- min-y)) (> ty (1+ max-y)))
                           (push (cons key tile) evicted))))
                     gpu-tiles)
            (dolist (entry evicted)
              (%retire-voxel-lod-gpu-tile state (car entry) (cdr entry))))
          ;; Collect tiles needing initial build or neighbor/edit re-mesh.
          (let ((pending nil))
            (loop for ty from min-y to max-y do
              (loop for tx from min-x to max-x
                    unless (distant-level0-covered-by-near-p tx ty mesh-slots)
                      do (let* ((key (%voxel-lod-tile-key tx ty))
                                (existing (gethash key gpu-tiles))
                                (nmask (%voxel-lod-near-mask mesh-slots tx ty))
                                (dx (- (+ (* tx 64) 32.0f0) camera-x))
                                (dy (- (+ (* ty 64) 32.0f0) camera-y))
                                (dist2 (+ (* dx dx) (* dy dy))))
                           (when (or (null existing)
                                     (voxel-lod-gpu-tile-dirty-p existing)
                                     (/= nmask (voxel-lod-gpu-tile-near-mask existing)))
                             (push (list dist2 tx ty key existing nmask) pending)))))
            (setf pending (sort pending #'< :key #'first))
            (unless (voxel-lod-fibers-scratch state)
              (setf (voxel-lod-fibers-scratch state)
                    (luft:make-chunk-fibers domain (luft:chunk-key-at 0 0))))
            (let ((built 0)
                  (thresh (max 1 (min 8 *voxel-lod-occupancy-threshold*)))
                  (cull-p *voxel-lod-cull-caves-p*)
                  (grid (voxel-lod-grid-scratch state))
                  (egrid (voxel-lod-emission-scratch state))
                  (fibers (voxel-lod-fibers-scratch state)))
              (dolist (item pending)
                (when (>= built budget)
                  (return))
                (destructuring-bind (dist2 tx ty key existing nmask) item
                  (declare (ignore dist2))
                  (let* ((had-coarse-p (nth-value 1 (gethash key coarse-cache)))
                         (ctile (or (gethash key coarse-cache)
                                    (setf (gethash key coarse-cache)
                                          (build-world-source-coarse-tile
                                           source tx ty
                                           :threshold thresh
                                           :cull-interior-caves-p cull-p
                                           :grid-scratch grid
                                           :emission-scratch egrid
                                           :fibers-scratch fibers))))
                         (near-xm-p (logbitp 0 nmask))
                         (near-xp-p (logbitp 1 nmask))
                         (near-ym-p (logbitp 2 nmask))
                         (near-yp-p (logbitp 3 nmask))
                         (close-x-p (and (= tx max-x) (not near-xp-p)))
                         (close-y-p (and (= ty max-y) (not near-yp-p))))
                    (unless had-coarse-p
                      ;; Existing neighbor tiles whose borders clamped may want a seam refresh.
                      (loop for ndx from -1 to 1 do
                        (loop for ndy from -1 to 1
                              unless (and (zerop ndx) (zerop ndy))
                                do (let ((ntile (gethash (%voxel-lod-tile-key (+ tx ndx) (+ ty ndy))
                                                         gpu-tiles)))
                                     (when ntile
                                       (setf (voxel-lod-gpu-tile-dirty-p ntile) t))))))
                    (multiple-value-bind (site-words app-codes light-words star-count)
                        (mesh-coarse-voxel-tile
                         ctile
                         (lambda (dx dy)
                           (gethash (%voxel-lod-tile-key (+ tx dx) (+ ty dy)) coarse-cache))
                         descriptors
                         :close-x-plus-p close-x-p
                         :close-y-plus-p close-y-p
                         :near-xm-p near-xm-p
                         :near-xp-p near-xp-p
                         :near-ym-p near-ym-p
                         :near-yp-p near-yp-p)
                      (when existing
                        (%retire-voxel-lod-gpu-tile state key existing))
                      (let ((resident
                              (when (plusp star-count)
                                (own-gpu-object
                                 state (%upload-coarse-star-population
                                        renderer site-words app-codes light-words)))))
                        (setf (gethash key gpu-tiles)
                              (make-voxel-lod-gpu-tile
                               :tx tx :ty ty
                               :resident resident
                               :star-count star-count
                               :neighbor-mask (%voxel-lod-neighbor-mask coarse-cache tx ty)
                               :near-mask nmask
                               :dirty-p nil))))
                    (incf built))))
              built)))))))

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
