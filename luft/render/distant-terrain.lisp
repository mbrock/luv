(in-package #:luft.render)

;;; Distant terrain via concentric terraced voxel columns.
;;;
;;; Beyond the resident full-detail near chunks, four power-of-two LOD rings
;;; (strides 2, 4, 8, 16 cells; 32x32 sample tiles covering 64, 128, 256, 512
;;; cells) render analytical surface columns as packed 16-byte uvec4 quads in a
;;; single :DEVICE-LOCAL GPU-BUFFER-ARENA and one MULTI-DRAW-INDEXED-INDIRECT
;;; call per frame.
;;;
;;; Every inner ring's bounding box aligns exactly to the next outer ring's
;;; tile grid, and Level 0's 64-cell tiles align identically to Luft's near
;;; chunks, so skipping resident near chunks and inner-covered tiles requires
;;; zero partial-tile rebuilding when the camera moves or chunks stream in.

(defconstant +distant-tile-samples+ 32)
(defconstant +distant-level-count+ 4)
(defconstant +distant-coord-bias+ #x800000)
(defconstant +distant-z-bias+ 1024)
(defconstant +distant-quad-arena-initial-capacity+ 65536)

(defparameter *distant-terrain-p* t
  "Render concentric terraced voxel columns beyond near chunk residency.")

(defparameter *distant-terrain-frame-budget* 16
  "Maximum distant terrain tiles built and uploaded per interactive frame.")

(declaim (inline distant-level-stride distant-level-tile-size distant-tile-key))

(defun distant-tile-key (level tx ty)
  "Pack (LEVEL, TX, TY) into an EQL fixnum hash key without consing."
  (declare (type (integer 0 7) level)
           (type fixnum tx ty))
  (logior (ash (logand (+ tx #x100000) #x1FFFFF) 24)
          (ash (logand (+ ty #x100000) #x1FFFFF) 3)
          level))

(defun distant-level-stride (level)
  "Cell width of one column sample at LOD LEVEL (2, 4, 8, 16)."
  (declare (type (integer 0 7) level))
  (ash 2 level))

(defun distant-level-tile-size (level)
  "World-cell footprint of one tile at LOD LEVEL (64, 128, 256, 512)."
  (declare (type (integer 0 7) level))
  (* +distant-tile-samples+ (distant-level-stride level)))

(defun distant-ring-bounds (level camera-x camera-y)
  "Return (VALUES MIN-TX MAX-TX MIN-TY MAX-TY) inclusive in LEVEL's tile units.

Levels 0..2 snap to their parent level's tile grid (+/-3 parent tiles = 14x14
child tiles) so each ring's footprint is an exact integer block of the next
level's tiles. Level 3 spans +/-5 tiles (11x11, >=2560 cells in every
direction)."
  (declare (type (integer 0 3) level))
  (let ((cx (floor camera-x))
        (cy (floor camera-y)))
    (if (< level (1- +distant-level-count+))
        (let* ((parent-size (distant-level-tile-size (1+ level)))
               (px (floor cx parent-size))
               (py (floor cy parent-size)))
          (values (* (- px 3) 2) (1+ (* (+ px 3) 2))
                  (* (- py 3) 2) (1+ (* (+ py 3) 2))))
        (let* ((size (distant-level-tile-size level))
               (tx (floor cx size))
               (ty (floor cy size)))
          (values (- tx 5) (+ tx 5)
                  (- ty 5) (+ ty 5))))))

(defun distant-tile-covered-by-inner-ring-p (level tx ty inner-bounds)
  "Whether tile (TX, TY) at LEVEL (> 0) lies inside LEVEL-1's tile bounds."
  (declare (ignore level)
           (type fixnum tx ty))
  (when inner-bounds
    (let ((min-x (ash (aref inner-bounds 0) -1))
          (max-x (ash (aref inner-bounds 1) -1))
          (min-y (ash (aref inner-bounds 2) -1))
          (max-y (ash (aref inner-bounds 3) -1)))
      (and (<= min-x tx max-x)
           (<= min-y ty max-y)))))

(defun distant-level0-covered-by-near-p (tx ty mesh-slots)
  "Whether Level-0 64-cell tile (TX, TY) is currently resident in MESH-SLOTS."
  (declare (type fixnum tx ty))
  (and mesh-slots
       (<= 0 tx 65535)
       (<= 0 ty 65535)
       (nth-value 1 (gethash (luft:chunk-key-at (ash tx luft:+chunk-bits+)
                                                (ash ty luft:+chunk-bits+))
                             mesh-slots))))

(declaim (inline pack-distant-quad-words %push-distant-quad))

(defun pack-distant-quad-words
    (x y z0 z1 face width depth material-code top-code ao)
  "Pack one distant column quad into four 32-bit unsigned words (16 bytes).

Word 0: x + #x800000 (24 bits) | material-code (8 bits)
Word 1: y + #x800000 (24 bits) | top-code (8 bits)
Word 2: z0 + 1024 (12 bits) | z1 + 1024 (12 bits) | ao (8 bits)
Word 3: face (8 bits) | width (8 bits) | depth (8 bits) | reserved (8 bits)"
  (declare (type fixnum x y z0 z1 face width depth material-code top-code ao))
  (let ((xb (logand #xffffff (+ x +distant-coord-bias+)))
        (yb (logand #xffffff (+ y +distant-coord-bias+)))
        (z0b (logand #xfff (max 0 (min #xfff (+ z0 +distant-z-bias+)))))
        (z1b (logand #xfff (max 0 (min #xfff (+ z1 +distant-z-bias+))))))
    (values (logior xb (ash (logand #xff material-code) 24))
            (logior yb (ash (logand #xff top-code) 24))
            (logior z0b (ash z1b 12) (ash (logand #xff ao) 24))
            (logior (logand #xff face)
                    (ash (logand #xff width) 8)
                    (ash (logand #xff depth) 16)))))

(defun %push-distant-quad
    (out x y z0 z1 face width depth material-code top-code ao)
  (declare (type fixnum x y z0 z1 face width depth material-code top-code ao))
  (multiple-value-bind (w0 w1 w2 w3)
      (pack-distant-quad-words x y z0 z1 face width depth material-code top-code ao)
    (vector-push-extend w0 out)
    (vector-push-extend w1 out)
    (vector-push-extend w2 out)
    (vector-push-extend w3 out)))

(defun build-distant-tile-quads
    (source level tx ty &key heights top-codes side-codes out)
  "Sample SOURCE for tile (LEVEL, TX, TY) and emit packed terraced column quads.

Returns (VALUES WORDS QUAD-COUNT BOUNDS) where WORDS is a adjustable u32 vector
filled with 4*QUAD-COUNT words and BOUNDS is a 6-float AABB or NIL if empty."
  (declare (type (integer 0 3) level)
           (type fixnum tx ty))
  (let* ((count +distant-tile-samples+)
         (pitch (+ count 2))
         (grid-len (* pitch pitch))
         (stride (distant-level-stride level))
         (tile-size (distant-level-tile-size level))
         (origin-x (* tx tile-size))
         (origin-y (* ty tile-size))
         (skirt-drop (max 4 stride))
         (heights (or heights (make-array grid-len :element-type '(signed-byte 32)
                                                   :initial-element -1)))
         (top-codes (or top-codes (make-array grid-len :element-type '(unsigned-byte 8)
                                                       :initial-element 0)))
         (side-codes (or side-codes (make-array grid-len :element-type '(unsigned-byte 8)
                                                         :initial-element 0)))
         (out (or out (make-array 4096 :element-type '(unsigned-byte 32)
                                       :adjustable t :fill-pointer 0)))
         (min-z most-positive-fixnum)
         (max-z most-negative-fixnum))
    (declare (type (simple-array (signed-byte 32) (*)) heights)
             (type (simple-array (unsigned-byte 8) (*)) top-codes side-codes)
             (type fixnum count pitch stride tile-size origin-x origin-y skirt-drop
                   min-z max-z))
    (setf (fill-pointer out) 0)
    (sample-world-source-surface-grid
     source origin-x origin-y stride count heights top-codes side-codes)
    (dotimes (row count)
      (let ((wy (+ origin-y (* row stride)))
            (r1 (1+ row)))
        (declare (type fixnum wy r1))
        (dotimes (col count)
          (let* ((c1 (1+ col))
                 (idx (+ (* r1 pitch) c1))
                 (h (aref heights idx)))
            (declare (type fixnum c1 idx h))
            (when (>= h 0)
              (let* ((wx (+ origin-x (* col stride)))
                     (top-z (1+ h))
                     (tcode (aref top-codes idx))
                     (scode (aref side-codes idx))
                     (h-e (aref heights (1+ idx)))
                     (h-w (aref heights (1- idx)))
                     (h-n (aref heights (+ idx pitch)))
                     (h-s (aref heights (- idx pitch)))
                     (enclosure
                       (+ (if (> h-e h) (- h-e h) 0)
                          (if (> h-w h) (- h-w h) 0)
                          (if (> h-n h) (- h-n h) 0)
                          (if (> h-s h) (- h-s h) 0)))
                     (ao-drop (min 85 (floor (* enclosure 14) stride)))
                     (top-ao (- 255 ao-drop))
                     (side-ao (max 145 (- top-ao 12))))
                (declare (type fixnum wx top-z tcode scode h-e h-w h-n h-s
                               enclosure ao-drop top-ao side-ao))
                (when (< top-z min-z) (setf min-z top-z))
                (when (> top-z max-z) (setf max-z top-z))
                ;; Top (+Z) face.
                (%push-distant-quad
                 out wx wy top-z top-z 0 stride stride tcode tcode top-ao)
                ;; Four cardinal cliff skirts; outer tile edges extend slightly
                ;; downward so LOD-to-LOD and near-to-far seams never crack.
                (flet ((emit-side (face neighbor boundary-p)
                         (declare (type fixnum face neighbor))
                         (let* ((neighbor-top (if (< neighbor 0) 0 (1+ neighbor)))
                                (z0 (cond
                                      (boundary-p
                                       (max 0 (- (min top-z neighbor-top) skirt-drop)))
                                      ((< neighbor h) neighbor-top)
                                      (t top-z))))
                           (declare (type fixnum neighbor-top z0))
                           (when (< z0 top-z)
                             (when (< z0 min-z) (setf min-z z0))
                             (%push-distant-quad
                              out wx wy z0 top-z face stride stride
                              scode tcode side-ao)))))
                  (emit-side 1 h-e (= col (1- count)))
                  (emit-side 2 h-w (zerop col))
                  (emit-side 3 h-n (= row (1- count)))
                  (emit-side 4 h-s (zerop row)))))))))
    (let ((quad-count (ash (length out) -2)))
      (values out
              quad-count
              (when (plusp quad-count)
                (vector (coerce origin-x 'single-float)
                        (coerce origin-y 'single-float)
                        (coerce min-z 'single-float)
                        (coerce (+ origin-x tile-size) 'single-float)
                        (coerce (+ origin-y tile-size) 'single-float)
                        (coerce max-z 'single-float)))))))

;;; GPU arena and active ring management ------------------------------------

(defstruct (distant-tile (:copier nil))
  (level 0 :type (integer 0 3) :read-only t)
  (tx 0 :type fixnum :read-only t)
  (ty 0 :type fixnum :read-only t)
  (slice nil)
  (quad-count 0 :type (integer 0 *))
  (bounds nil))

(defclass distant-terrain-state (gpu-resource-owner)
  ((device :initarg :device :reader distant-terrain-device)
   (source :initform nil :accessor distant-terrain-source)
   (quad-arena :initform nil :accessor distant-terrain-quad-arena)
   (tiles :initform (make-hash-table :test #'eql) :reader distant-terrain-tiles)
   (ring-bounds :initform (make-array +distant-level-count+ :initial-element nil)
                :reader distant-terrain-ring-bounds)
   (horizon-distance :initform 0.0f0 :accessor distant-terrain-horizon-distance)
   (heights-scratch
    :initform (make-array (* (+ +distant-tile-samples+ 2) (+ +distant-tile-samples+ 2))
                          :element-type '(signed-byte 32) :initial-element -1)
    :reader distant-terrain-heights-scratch)
   (top-codes-scratch
    :initform (make-array (* (+ +distant-tile-samples+ 2) (+ +distant-tile-samples+ 2))
                          :element-type '(unsigned-byte 8) :initial-element 0)
    :reader distant-terrain-top-codes-scratch)
   (side-codes-scratch
    :initform (make-array (* (+ +distant-tile-samples+ 2) (+ +distant-tile-samples+ 2))
                          :element-type '(unsigned-byte 8) :initial-element 0)
    :reader distant-terrain-side-codes-scratch)
   (quad-scratch
    :initform (make-array 8192 :element-type '(unsigned-byte 32)
                               :adjustable t :fill-pointer 0)
    :reader distant-terrain-quad-scratch)))

(defun make-distant-terrain-state (device)
  "Create the renderer's distant terraced-column arena and tile cache."
  (let ((state (make-instance 'distant-terrain-state :device device)))
    (with-gpu-construction (state)
      (setf (distant-terrain-quad-arena state)
            (own-gpu-object
             state
             (make-gpu-buffer-arena
              device :label "luft distant terrain quads arena"
                     :element-size 16
                     :initial-capacity +distant-quad-arena-initial-capacity+
                     :usage '(:storage) :memory :device-local))))))

(defun distant-terrain-quad-buffer (state)
  (and state
       (distant-terrain-quad-arena state)
       (gpu-buffer-arena-buffer (distant-terrain-quad-arena state))))

(defun %distant-terrain-progress (state)
  (let ((reader (queue-progress-reader (device-queue (distant-terrain-device state)))))
    (if reader
        (funcall reader)
        (values 0 nil))))

(defun reclaim-distant-terrain-arena (state)
  (when (and state (distant-terrain-quad-arena state)
             (not (gpu-buffer-arena-destroyed-p (distant-terrain-quad-arena state))))
    (multiple-value-bind (submitted completed) (%distant-terrain-progress state)
      (declare (ignore submitted))
      (if completed
          (gpu-buffer-arena-reclaim (distant-terrain-quad-arena state) :completed completed)
          (gpu-buffer-arena-reclaim (distant-terrain-quad-arena state)))))
  state)

(defun flush-distant-terrain-arena (state &optional encoder)
  (when (and state (distant-terrain-quad-arena state)
             (not (gpu-buffer-arena-destroyed-p (distant-terrain-quad-arena state))))
    (gpu-buffer-arena-flush (distant-terrain-quad-arena state) encoder))
  state)

(defun clear-distant-terrain (state)
  "Evict every cached distant terrain tile and reset horizon distance."
  (when state
    (let ((arena (distant-terrain-quad-arena state))
          (tiles (distant-terrain-tiles state)))
      (multiple-value-bind (submitted completed) (%distant-terrain-progress state)
        (declare (ignore completed))
        (maphash (lambda (key tile)
                   (declare (ignore key))
                   (when (and arena (distant-tile-slice tile)
                              (not (gpu-buffer-arena-destroyed-p arena)))
                     (gpu-buffer-arena-free arena (distant-tile-slice tile)
                                            :ready-after submitted)))
                 tiles))
      (clrhash tiles)
      (dotimes (level +distant-level-count+)
        (setf (aref (distant-terrain-ring-bounds state) level) nil))
      (setf (distant-terrain-horizon-distance state) 0.0f0)))
  state)

(defun renderer-distant-terrain-horizon (renderer)
  "Active outer horizon fade distance in cells, or 0.0 when distant terrain is off."
  (let ((state (and renderer (renderer-distant-terrain renderer))))
    (if (and *distant-terrain-p* state (distant-terrain-source state)
             (plusp (hash-table-count (distant-terrain-tiles state))))
        (distant-terrain-horizon-distance state)
        0.0f0)))

(defun update-renderer-distant-terrain
    (renderer source camera-x camera-y &key (budget *distant-terrain-frame-budget*))
  "Maintain concentric distant-terrain rings around (CAMERA-X, CAMERA-Y).

Evicts tiles outside the outer hysteresis margin and builds up to BUDGET
missing tiles closest to the camera first.  Also ensures RENDERER's shared
material-descriptors buffer carries SOURCE's vocabulary."
  (let ((state (and renderer (renderer-distant-terrain renderer))))
    (unless (and state *distant-terrain-p* source
                 (world-source-distant-terrain-p source))
      (when (and state (distant-terrain-source state))
        (clear-distant-terrain state)
        (setf (distant-terrain-source state) nil))
      (return-from update-renderer-distant-terrain 0))
    (unless (eq source (distant-terrain-source state))
      (clear-distant-terrain state)
      (setf (distant-terrain-source state) source)
      ;; Make sure material-descriptors is populated even before near chunks land.
      (let* ((vocab (world-source-material-vocabulary source))
             (descriptors (compile-terrain-material-descriptors vocab)))
        (terrain-arenas-ensure-descriptors (renderer-terrain-arenas renderer) descriptors)))
    (reclaim-distant-terrain-arena state)
    (let ((ring-bounds (distant-terrain-ring-bounds state))
          (tiles (distant-terrain-tiles state))
          (arena (distant-terrain-quad-arena state))
          (mesh-slots (renderer-mesh-slots renderer)))
      ;; Compute the 4 nested ring boxes.
      (dotimes (level +distant-level-count+)
        (multiple-value-bind (min-x max-x min-y max-y)
            (distant-ring-bounds level camera-x camera-y)
          (setf (aref ring-bounds level) (vector min-x max-x min-y max-y))))
      ;; Evict tiles outside a 1-tile hysteresis margin around their ring.
      (let ((evicted nil))
        (maphash (lambda (key tile)
                   (let* ((level (distant-tile-level tile))
                          (tx (distant-tile-tx tile))
                          (ty (distant-tile-ty tile))
                          (b (aref ring-bounds level)))
                     (when (or (< tx (1- (aref b 0))) (> tx (1+ (aref b 1)))
                               (< ty (1- (aref b 2))) (> ty (1+ (aref b 3))))
                       (push (cons key tile) evicted))))
                 tiles)
        (when evicted
          (multiple-value-bind (submitted completed) (%distant-terrain-progress state)
            (declare (ignore completed))
            (dolist (entry evicted)
              (remhash (car entry) tiles)
              (let ((slice (distant-tile-slice (cdr entry))))
                (when slice
                  (gpu-buffer-arena-free arena slice :ready-after submitted)))))))
      ;; Gather missing tiles across all active rings, skipping tiles covered by
      ;; inner rings or currently resident near chunks.
      (let ((missing nil))
        (dotimes (level +distant-level-count+)
          (let* ((b (aref ring-bounds level))
                 (inner (and (> level 0) (aref ring-bounds (1- level))))
                 (tile-size (distant-level-tile-size level))
                 (half (* 0.5f0 tile-size)))
            (loop for ty from (aref b 2) to (aref b 3) do
              (loop for tx from (aref b 0) to (aref b 1)
                    for key = (distant-tile-key level tx ty)
                    unless (or (gethash key tiles)
                               (if (zerop level)
                                   (distant-level0-covered-by-near-p tx ty mesh-slots)
                                   (distant-tile-covered-by-inner-ring-p level tx ty inner)))
                      do (let* ((dx (- (+ (* tx tile-size) half) camera-x))
                                (dy (- (+ (* ty tile-size) half) camera-y))
                                (dist2 (+ (* dx dx) (* dy dy))))
                           (push (list dist2 level tx ty key) missing))))))
        ;; Build closest tiles first so near rings fill immediately.
        (setf missing (sort missing #'< :key #'first))
        (let ((built 0)
              (heights (distant-terrain-heights-scratch state))
              (top-codes (distant-terrain-top-codes-scratch state))
              (side-codes (distant-terrain-side-codes-scratch state))
              (scratch (distant-terrain-quad-scratch state)))
          (dolist (item missing)
            (when (>= built budget)
              (return))
            (destructuring-bind (dist2 level tx ty key) item
              (declare (ignore dist2))
              (multiple-value-bind (words quad-count bounds)
                  (build-distant-tile-quads
                   source level tx ty
                   :heights heights :top-codes top-codes :side-codes side-codes
                   :out scratch)
                (let ((slice (when (plusp quad-count)
                               (let ((s (gpu-buffer-arena-allocate arena quad-count)))
                                 (gpu-buffer-arena-write arena s words)
                                 s))))
                  (setf (gethash key tiles)
                        (make-distant-tile :level level :tx tx :ty ty
                                           :slice slice :quad-count quad-count
                                           :bounds bounds)))
                (incf built))))
          ;; Outer ring guarantees >= 2560 cells in every cardinal direction.
          (setf (distant-terrain-horizon-distance state) 2500.0f0)
          built)))))

(defun encode-renderer-distant-terrain (renderer frame pass &optional clip)
  "Draw all visible active distant terrain tiles in one indirect batch."
  (let* ((state (and *distant-terrain-p* (renderer-distant-terrain renderer)))
         (terrain (renderer-terrain renderer))
         (program (and (typep terrain 'terrain-drawing)
                       (terrain-distant-program terrain)))
         (indices (and (typep terrain 'terrain-drawing)
                       (terrain-distant-indices terrain))))
    (unless (and state program indices (distant-terrain-source state)
                 (plusp (hash-table-count (distant-terrain-tiles state))))
      (return-from encode-renderer-distant-terrain))
    (let* ((tiles (distant-terrain-tiles state))
           (ring-bounds (distant-terrain-ring-bounds state))
           (mesh-slots (renderer-mesh-slots renderer))
           (scratch (renderer-frame-state-indirect-draw-scratch frame)))
      (setf (fill-pointer scratch) 0)
      (dotimes (level +distant-level-count+)
        (let ((b (aref ring-bounds level))
              (inner (and (> level 0) (aref ring-bounds (1- level)))))
          (when b
            (loop for ty from (aref b 2) to (aref b 3) do
              (loop for tx from (aref b 0) to (aref b 1)
                    unless (if (zerop level)
                               (or (distant-level0-covered-by-near-p tx ty mesh-slots)
                                   (voxel-lod-tile-present-p renderer tx ty))
                               (distant-tile-covered-by-inner-ring-p level tx ty inner))
                      do (let ((tile (gethash (distant-tile-key level tx ty) tiles)))
                           (when (and tile (plusp (distant-tile-quad-count tile))
                                      (distant-tile-slice tile)
                                      (or (null clip)
                                          (null (distant-tile-bounds tile))
                                          (not (box-outside-clip-p
                                                (distant-tile-bounds tile) clip))))
                             (%push-indirect-draw
                              scratch 6 (distant-tile-quad-count tile) 0
                              (gpu-buffer-slice-offset (distant-tile-slice tile))))))))))
      (multiple-value-bind (indirect draw-count byte-offset)
          (stage-frame-indirect-draws renderer frame scratch)
        (when (plusp draw-count)
          (let ((binding (renderer-frame-distant-terrain-bind-group renderer frame)))
            (set-pipeline pass (program-pipeline program))
            (set-bind-group pass 0 binding)
            (multi-draw-indexed-indirect
             pass indices :uint32 indirect draw-count byte-offset)))))))

(defun distant-terrain-stats (renderer)
  "Return an inspectable plist of distant terrain residency and arena stats."
  (let ((state (and renderer (renderer-distant-terrain renderer))))
    (when state
      (let ((counts (make-array +distant-level-count+ :initial-element 0))
            (total-quads 0))
        (maphash (lambda (key tile)
                   (declare (ignore key))
                   (incf (aref counts (distant-tile-level tile)))
                   (incf total-quads (distant-tile-quad-count tile)))
                 (distant-terrain-tiles state))
        (list :enabled *distant-terrain-p*
              :source (and (distant-terrain-source state) t)
              :horizon-distance (distant-terrain-horizon-distance state)
              :tile-count (hash-table-count (distant-terrain-tiles state))
              :tiles-by-level (coerce counts 'list)
              :quad-count total-quads
              :arena (gpu-buffer-arena-stats (distant-terrain-quad-arena state)))))))
