(in-package #:luft.render)

;;; Level 1 publishes through the same terrain arenas as the inner ring (#Q6UGN3).
;;; The canvas owns this table, the edit snapshot and every GPU allocation. One
;;; producer builds and sorts a single immutable request at a time. A slot keeps
;;; its old resident while a replacement or a neighbor apron is being prepared.

(defvar *stride-four-mode* :heightfield
  "Level-1 control while voxel LOD is enabled: :heightfield, :direct, :exact, or :auto.")

(defstruct stride-four-slot
  tx ty volume provenance resident (stars 0) (dirty t) (remesh t) (inner-mask 0))

(defstruct stride-four-ring
  worker pending bounds (head -1) edits descriptors
  (slots (make-hash-table :test #'eql)) (publications 0) (refinements 0))

(defclass stride-four-request (production:production-request)
  ((slot :initarg :slot :reader request-ring-slot)
   (kind :initarg :kind :reader request-ring-kind)
   (mode :initform *stride-four-mode* :reader request-ring-mode)
   (build :initarg :build :reader request-ring-build)))

(defmethod production:perform-production-request ((request stride-four-request))
  (funcall (request-ring-build request)))

(defun clear-stride-four-ring (state)
  (let ((ring (voxel-lod-stride-four state)))
    (when (and ring (stride-four-ring-bounds ring))
      (maphash (lambda (key slot)
                 (declare (ignore key))
                 (when (stride-four-slot-resident slot)
                   (release-owned-gpu-object state (stride-four-slot-resident slot))))
               (stride-four-ring-slots ring))
      (clrhash (stride-four-ring-slots ring))
      (setf (stride-four-ring-bounds ring) nil
            (stride-four-ring-head ring) -1
            (stride-four-ring-edits ring) nil
            (stride-four-ring-descriptors ring) nil
            (stride-four-ring-pending ring) nil))))

(defun stop-stride-four-worker (state)
  (let ((ring (voxel-lod-stride-four state)))
    (when ring
      (production:stop-production-system (stride-four-ring-worker ring))
      (setf (voxel-lod-stride-four state) nil))))

(defun stride-four-tile-present-p (renderer tx ty)
  (let* ((state (and *voxel-lod-p* (renderer-voxel-lod renderer)))
         (ring (and state (voxel-lod-stride-four state)))
         (slot (and ring (gethash (distant-tile-key 1 tx ty) (stride-four-ring-slots ring)))))
    (and (not (eq *stride-four-mode* :heightfield)) slot (stride-four-slot-volume slot))))

(defun stride-four-visible-residents (state)
  (let ((ring (voxel-lod-stride-four state)))
    (when (and ring (not (eq *stride-four-mode* :heightfield)))
      (loop for slot being the hash-values of (stride-four-ring-slots ring)
            for resident = (stride-four-slot-resident slot)
            when (and resident (resident-drawn-p resident nil)) collect resident))))

(defun stride-four-ring-stats (state)
  (let ((ring (voxel-lod-stride-four state)))
    (when ring
      (let ((slots (stride-four-ring-slots ring)))
        (list :mode *stride-four-mode* :demand (hash-table-count slots)
              :direct (loop for s being the hash-values of slots count (eq :direct (stride-four-slot-provenance s)))
              :exact (loop for s being the hash-values of slots count (eq :exact (stride-four-slot-provenance s)))
              :stars (loop for s being the hash-values of slots sum (stride-four-slot-stars s))
              :publications (stride-four-ring-publications ring)
              :refinements (stride-four-ring-refinements ring))))))

(defun touch-stride-four-neighbors (slots tx ty)
  (loop for dx from -1 to 1 do
    (loop for dy from -1 to 1
          unless (and (zerop dx) (zerop dy))
            do (let ((neighbor (gethash (distant-tile-key 1 (+ tx dx) (+ ty dy)) slots)))
                 (when neighbor (setf (stride-four-slot-remesh neighbor) t))))))

(defun retarget-stride-four-ring (ring state x y)
  (let ((snap (list (floor x 128) (floor y 128))))
    (unless (equal snap (stride-four-ring-bounds ring))
      (let ((outer (multiple-value-list (distant-ring-bounds 1 x y)))
            (inner (multiple-value-call #'vector (distant-ring-bounds 0 x y)))
            (slots (stride-four-ring-slots ring)))
        (flet ((inner-p (tx ty) (distant-tile-covered-by-inner-ring-p 1 tx ty inner)))
          (destructuring-bind (xmin xmax ymin ymax) outer
            (maphash (lambda (key slot)
                       (let ((tx (stride-four-slot-tx slot)) (ty (stride-four-slot-ty slot)))
                         (unless (and (<= xmin tx xmax) (<= ymin ty ymax) (not (inner-p tx ty)))
                           (touch-stride-four-neighbors slots tx ty)
                           (when (stride-four-slot-resident slot)
                             (release-owned-gpu-object state (stride-four-slot-resident slot)))
                           (remhash key slots))))
                     slots)
            (loop for tx from xmin to xmax do
              (loop for ty from ymin to ymax unless (inner-p tx ty) do
                (let* ((key (distant-tile-key 1 tx ty))
                       (slot (or (gethash key slots)
                                 (setf (gethash key slots) (make-stride-four-slot :tx tx :ty ty))))
                       (mask (logior (if (inner-p (1- tx) ty) 1 0) (if (inner-p (1+ tx) ty) 2 0)
                                     (if (inner-p tx (1- ty)) 4 0) (if (inner-p tx (1+ ty)) 8 0))))
                  (unless (= mask (stride-four-slot-inner-mask slot))
                    (setf (stride-four-slot-inner-mask slot) mask (stride-four-slot-remesh slot) t)))))))
        (setf (stride-four-ring-bounds ring) snap
              (stride-four-ring-pending ring) nil)))))

(defun snapshot-stride-four-edits (ring source)
  ;; Workers only read immutable generator parameters and this owner-captured edit table.
  (let* ((journal (world-source-journal source))
         (head (world-journal-head journal))
         (previous (stride-four-ring-head ring)))
    (unless (= head previous)
      (let ((slots (stride-four-ring-slots ring))
            (watermark (world-journal-watermark journal)))
        (if (<= watermark previous head)
            (loop for i from (- previous watermark) below (- head watermark)
                  for site = (world-event-site (aref (world-journal-events journal) i))
                  for slot = (gethash (distant-tile-key 1 (floor (luft:site-x site) 128)
                                                        (floor (luft:site-y site) 128)) slots)
                  when slot do (setf (stride-four-slot-dirty slot) t))
            (loop for slot being the hash-values of slots do (setf (stride-four-slot-dirty slot) t))))
      (setf (stride-four-ring-edits ring) (alexandria:copy-hash-table (world-source-edits source))
            (stride-four-ring-head ring) head
            (stride-four-ring-pending ring) nil))))

(defun exact-stride-four-available-p (source tx ty)
  (let ((domain (world-source-domain source)))
    (and (<= 0 tx) (<= (* 128 (1+ tx)) (luft:world-domain-x-limit domain))
         (<= 0 ty) (<= (* 128 (1+ ty)) (luft:world-domain-y-limit domain)))))

(defun next-stride-four-job (ring source x y)
  (let ((best nil) (kind nil) (score most-positive-fixnum))
    (maphash
     (lambda (key slot)
       (declare (ignore key))
       (let* ((tx (stride-four-slot-tx slot)) (ty (stride-four-slot-ty slot))
              (has (stride-four-slot-provenance slot))
              (want (if (and (member *stride-four-mode* '(:auto :exact))
                             (exact-stride-four-available-p source tx ty))
                        :exact :direct)))
         (multiple-value-bind (tier job)
             (cond ((or (null has) (stride-four-slot-dirty slot))
                    (values 0 (if (eq *stride-four-mode* :auto) :direct want)))
                   ((and (not (eq has want)) (not (eq *stride-four-mode* :auto))) (values 0 want))
                   ((stride-four-slot-remesh slot) (values 1 :remesh))
                   ((not (eq has want)) (values 2 want)))
           (when tier
             (let ((distance (+ (* tier 10000000) (expt (- (+ (* tx 128) 64) x) 2)
                                (expt (- (+ (* ty 128) 64) y) 2))))
               (when (< distance score) (setf best slot kind job score distance)))))))
     (stride-four-ring-slots ring))
    (values best kind)))

(defun make-stride-four-job (ring state source slot kind)
  (let* ((tx (stride-four-slot-tx slot)) (ty (stride-four-slot-ty slot))
         (key (distant-tile-key 1 tx ty))
         (volume (stride-four-slot-volume slot))
         (edits (stride-four-ring-edits ring)) (head (stride-four-ring-head ring))
         (threshold *voxel-lod-occupancy-threshold*)
         (descriptors (stride-four-ring-descriptors ring))
         (mask (stride-four-slot-inner-mask slot))
         (neighbors (make-array '(3 3) :initial-element nil))
         (children (when (eq kind :exact) (make-array '(2 2) :initial-element nil))))
    (dotimes (i 3)
      (dotimes (j 3)
        (let ((s (gethash (distant-tile-key 1 (+ tx i -1) (+ ty j -1)) (stride-four-ring-slots ring))))
          (when s (setf (aref neighbors i j) (stride-four-slot-volume s))))))
    (when (and children (not *voxel-lod-cull-caves-p*))
      (dotimes (i 2)
        (dotimes (j 2)
          (let ((child (gethash (%voxel-lod-tile-key (+ (* tx 2) i) (+ (* ty 2) j))
                                (voxel-lod-coarse-cache state))))
            (when (and child (= head (coarse-voxel-tile-journal-head child)))
              (setf (aref children i j) child))))))
    (make-instance
     'stride-four-request :key key :slot slot :kind kind
     :build
     (lambda ()
       (let ((tile (ecase kind
                     (:direct (build-native-stride-four-tile source tx ty :edits edits :head head))
                     (:exact
                      (dotimes (i 2)
                        (dotimes (j 2)
                          (unless (aref children i j)
                            (setf (aref children i j)
                                  (build-world-source-coarse-tile
                                   source (+ (* tx 2) i) (+ (* ty 2) j) :threshold threshold
                                   :cull-interior-caves-p nil :edits edits :head head)))))
                      (reduce-stride-two-children tx ty children :threshold threshold))
                     (:remesh volume))))
         (multiple-value-bind (sites apps lights stars)
             (mesh-coarse-voxel-tile tile (lambda (dx dy) (aref neighbors (1+ dx) (1+ dy))) descriptors
                                     :near-xm-p (logbitp 0 mask) :near-xp-p (logbitp 1 mask)
                                     :near-ym-p (logbitp 2 mask) :near-yp-p (logbitp 3 mask))
           (list* tile stars mask
                  (multiple-value-list (sort-sites-by-star sites (pack-terrain-appearance-codes apps)
                                                          lights nil)))))))))

(defun publish-stride-four-result (result ring state renderer)
  (let* ((request (production:production-result-request result))
         (slot (request-ring-slot request))
         (kind (request-ring-kind request)))
    (when (eq request (stride-four-ring-pending ring))
      (setf (stride-four-ring-pending ring) nil)
      (when (and (eq slot (gethash (production:production-request-key request) (stride-four-ring-slots ring)))
                 (eq *stride-four-mode* (request-ring-mode request)))
        (when (production:production-result-condition result)
          (error (production:production-result-condition result)))
        (destructuring-bind (tile stars mask &rest packed) (production:production-result-value result)
          (let ((old (stride-four-slot-resident slot))
                (new (when (plusp stars)
                       (own-gpu-object state (apply #'%upload-prepared-coarse-population renderer packed)))))
            (unless (eq kind :remesh)
              (when (and (eq kind :exact) (eq :direct (stride-four-slot-provenance slot)))
                (incf (stride-four-ring-refinements ring)))
              (touch-stride-four-neighbors (stride-four-ring-slots ring)
                                           (stride-four-slot-tx slot) (stride-four-slot-ty slot))
              (setf (stride-four-slot-provenance slot) kind))
            (setf (stride-four-slot-volume slot) tile (stride-four-slot-resident slot) new
                  (stride-four-slot-stars slot) stars (stride-four-slot-dirty slot) nil
                  (stride-four-slot-remesh slot) (/= mask (stride-four-slot-inner-mask slot)))
            (incf (stride-four-ring-publications ring))
            (when old (release-owned-gpu-object state old))))))))

(defun update-stride-four-ring (renderer state source x y)
  (cond
    ((or (eq *stride-four-mode* :heightfield) (not (world-source-stride-four-p source)))
     (clear-stride-four-ring state))
    (t
     (let* ((ring (or (voxel-lod-stride-four state)
                      (setf (voxel-lod-stride-four state)
                            (make-stride-four-ring :worker (production:make-single-worker-production-system
                                                          :name "Luft stride-four terrain")))))
            (worker (stride-four-ring-worker ring)))
       (unless (stride-four-ring-descriptors ring)
         (setf (stride-four-ring-descriptors ring)
               (compile-terrain-material-descriptors (world-source-material-vocabulary source))))
       (retarget-stride-four-ring ring state x y)
       (snapshot-stride-four-edits ring source)
       (let ((result (production:receive-production-result-no-hang worker)))
         (when result (publish-stride-four-result result ring state renderer)))
       (unless (or (stride-four-ring-pending ring) (plusp (production:production-system-pending-count worker)))
         (multiple-value-bind (slot kind) (next-stride-four-job ring source x y)
           (when slot
             (let ((request (make-stride-four-job ring state source slot kind)))
               (production:schedule-production-request worker request)
               (setf (stride-four-ring-pending ring) request)))))))))
