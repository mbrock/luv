(in-package #:luft.terrain)

;;; Generic 3D coarse voxel tiles (#INDOL7 #CLIAOO #NDXUW8).
;;;
;;; A coarse voxel tile downsamples a 64x64x255 chunk (at stride 2, lod-shift 1)
;;; into a 32x32x128 3D grid of occupancy bits, exposed boundary-voted material
;;; codes, and optional RGB4 emissive blocks.  It compiles straight into the
;;; exact same 256-star GPU ABI as near chunks, carrying lod-shift and +2^20
;;; coordinate bias in site.z so steep mountains, caves, carved tunnels, and
;;; overhangs render with the same beveled geometry and sky irradiance.

(defconstant +coarse-tile-bias+ 1048576)

(defstruct (coarse-voxel-tile
             (:constructor %make-coarse-voxel-tile
                 (tx ty lod-shift size-xy size-z word-count words materials
                  &key emissions journal-head authored-air-cells)))
  "Downsampled 3D voxel occupancy and boundary materials for tile (TX, TY)."
  (tx 0 :type fixnum :read-only t)
  (ty 0 :type fixnum :read-only t)
  (lod-shift 1 :type (integer 1 4) :read-only t)
  (size-xy 32 :type fixnum :read-only t)
  (size-z 128 :type fixnum :read-only t)
  (word-count 2 :type fixnum :read-only t)
  (words #() :type (simple-array (unsigned-byte 64) (*)) :read-only t)
  (materials #() :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (emissions nil :type (or null hash-table))
  (journal-head 0 :type fixnum)
  (authored-air-cells 0 :type fixnum))

(declaim (inline %coarse-fiber-base %coarse-cell-index %coarse-occupied-p))

(defun %coarse-fiber-base (cx cy size-xy word-count)
  (declare (type fixnum cx cy size-xy word-count)
           (optimize (speed 3) (safety 1)))
  (the fixnum (* word-count (the fixnum (+ cy (the fixnum (* size-xy cx)))))))

(defun %coarse-cell-index (cx cy cz size-xy size-z)
  (declare (type fixnum cx cy cz size-xy size-z)
           (optimize (speed 3) (safety 1)))
  (the fixnum (+ cz (the fixnum (* size-z (the fixnum (+ cy (the fixnum (* size-xy cx)))))))))

(defun %coarse-occupied-p (words base cz)
  (declare (type (simple-array (unsigned-byte 64) (*)) words)
           (type fixnum base cz)
           (optimize (speed 3) (safety 1)))
  (logbitp (logand cz 63) (aref words (+ base (ash cz -6)))))

(defun %fine-cell-exposed-p (material-grid fx fy fz)
  "Whether fine cell (FX, FY, FZ) touches at least one in-chunk air cell (or sky)."
  (declare (type (simple-array (unsigned-byte 8) (*)) material-grid)
           (type fixnum fx fy fz)
           (optimize (speed 3) (safety 1)))
  (let ((idx (+ fz (ash (+ fx (ash fy 6)) 8))))
    (declare (type fixnum idx))
    (or (>= fz (1- luft:+top-z+))
        (zerop (aref material-grid (1+ idx)))
        (and (plusp fz) (zerop (aref material-grid (1- idx))))
        (and (plusp fx) (zerop (aref material-grid (- idx 256))))
        (and (< fx 63) (zerop (aref material-grid (+ idx 256))))
        (and (plusp fy) (zerop (aref material-grid (- idx 16384))))
        (and (< fy 63) (zerop (aref material-grid (+ idx 16384)))))))

(defun %vote-2x2x2-material (material-grid lx0 ly0 lz0)
  "Pick the dominant material code in [LX0..LX0+1]x[LY0..LY0+1]x[LZ0..LZ0+1],
strongly preferring exposed boundary cells over occluded interior rock (#NDXUW8)."
  (declare (type (simple-array (unsigned-byte 8) (*)) material-grid)
           (type fixnum lx0 ly0 lz0)
           (optimize (speed 3) (safety 1)))
  (let ((exposed-codes (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
        (exposed-counts (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
        (exposed-len 0)
        (all-codes (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
        (all-counts (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
        (all-len 0))
    (declare (type (simple-array (unsigned-byte 8) (8))
                   exposed-codes exposed-counts all-codes all-counts)
             (type fixnum exposed-len all-len))
    (dotimes (dz 2)
      (let ((fz (+ lz0 dz)))
        (declare (type fixnum fz))
        (when (< fz luft:+top-z+)
          (dotimes (dy 2)
            (let ((fy (+ ly0 dy)))
              (declare (type fixnum fy))
              (dotimes (dx 2)
                (let* ((fx (+ lx0 dx))
                       (code (aref material-grid (+ fz (ash (+ fx (ash fy 6)) 8)))))
                  (declare (type fixnum fx)
                           (type (unsigned-byte 8) code))
                  (when (plusp code)
                    ;; Record in all-cells histogram.
                    (let ((found nil))
                      (dotimes (i all-len)
                        (when (= (aref all-codes i) code)
                          (incf (aref all-counts i))
                          (setf found t)
                          (return)))
                      (unless found
                        (setf (aref all-codes all-len) code
                              (aref all-counts all-len) 1)
                        (incf all-len)))
                    ;; Record in exposed-boundary histogram when touching air.
                    (when (%fine-cell-exposed-p material-grid fx fy fz)
                      (let ((found nil))
                        (dotimes (i exposed-len)
                          (when (= (aref exposed-codes i) code)
                            (incf (aref exposed-counts i))
                            (setf found t)
                            (return)))
                        (unless found
                          (setf (aref exposed-codes exposed-len) code
                                (aref exposed-counts exposed-len) 1)
                          (incf exposed-len))))))))))))
    (flet ((pick-best (codes counts len)
             (let ((best-code 0)
                   (best-count 0))
               (declare (type (unsigned-byte 8) best-code best-count))
               ;; >= favors later entries (higher LZ / surface skin on tie).
               (dotimes (i len best-code)
                 (let ((c (aref counts i)))
                   (when (>= c best-count)
                     (setf best-count c
                           best-code (aref codes i))))))))
      (if (plusp exposed-len)
          (pick-best exposed-codes exposed-counts exposed-len)
          (pick-best all-codes all-counts all-len)))))

(defun %cull-sealed-coarse-pockets (words materials seeded-air &optional fallback-code)
  "Flood-fill exterior air from open sky (CZ=127) and any SEEDED-AIR indices
(e.g., authored carved-air edits), then mark unreachable enclosed pockets solid
for this coarse LOD so deep bedrock caves do not waste 75% of distant stars (#CLIAOO)."
  (declare (type (simple-array (unsigned-byte 64) (*)) words)
           (type (simple-array (unsigned-byte 8) (*)) materials)
           (type list seeded-air)
           (optimize (speed 3) (safety 1)))
  (let* ((total (* 32 32 128))
         (reachable (make-array total :element-type 'bit :initial-element 0))
         (queue (make-array total :element-type '(unsigned-byte 32)))
         (head 0)
         (tail 0)
         (culled 0)
         (default-code (or fallback-code 1)))
    (declare (type fixnum head tail culled)
             (type (unsigned-byte 8) default-code))
    (flet ((enqueue (idx)
             (declare (type fixnum idx))
             (when (and (zerop (aref materials idx))
                        (zerop (sbit reachable idx)))
               (setf (sbit reachable idx) 1
                     (aref queue tail) idx)
               (incf tail))))
      ;; Seed sky top plane CZ = 127.
      (dotimes (cx 32)
        (dotimes (cy 32)
          (enqueue (+ 127 (ash (+ cy (ash cx 5)) 7)))))
      ;; Seed any coarse air cells explicitly touched by authored air edits
      ;; (plus their column skyward? No, BFS handles everything connected to them).
      (dolist (idx seeded-air)
        (enqueue idx))
      ;; 6-connected BFS through open air.
      (loop while (< head tail) do
        (let* ((idx (aref queue head))
               (cz (logand idx 127))
               (col (ash idx -7))
               (cy (logand col 31))
               (cx (ash col -5)))
          (declare (type fixnum idx cz col cy cx))
          (incf head)
          (when (< cz 127) (enqueue (1+ idx)))
          (when (plusp cz) (enqueue (1- idx)))
          (when (> cx 0) (enqueue (- idx 4096)))
          (when (< cx 31) (enqueue (+ idx 4096)))
          (when (> cy 0) (enqueue (- idx 128)))
          (when (< cy 31) (enqueue (+ idx 128)))))
      ;; Plug unreachable pockets solid.
      (dotimes (cx 32)
        (dotimes (cy 32)
          (let ((base (* 2 (+ cy (* 32 cx))))
                (col-idx (ash (+ cy (ash cx 5)) 7)))
            (declare (type fixnum base col-idx))
            (dotimes (cz 128)
              (let ((idx (+ col-idx cz)))
                (when (and (zerop (aref materials idx))
                           (zerop (sbit reachable idx)))
                  (let ((fill (or (and (plusp cz)
                                       (let ((below (aref materials (1- idx))))
                                         (and (plusp below) below)))
                                  (and (< cz 127)
                                       (let ((above (aref materials (1+ idx))))
                                         (and (plusp above) above)))
                                  default-code)))
                    (setf (aref materials idx) fill)
                    (let ((w (+ base (ash cz -6)))
                          (bit (logand cz 63)))
                      (setf (aref words w)
                            (logior (aref words w) (ash 1 bit))))
                    (incf culled))))))))
      culled)))

(defun %fill-world-source-chunk-fine-grid (source tx ty grid fibers emission-grid emission-table edits head)
  "Evaluate SOURCE at 64x64 chunk (TX, TY) into flat GRID, FIBERS, EMISSION-GRID,
and return (VALUES SEEDED-AIR-COORDS JOURNAL-HEAD AUTHORED-AIR-COUNT).
Both generated terrain and sparse journal edits (including carved air) replay cleanly."
  (declare (type (simple-array (unsigned-byte 8) (*)) grid)
           (optimize (speed 3) (safety 1)))
  (fill grid 0)
  (fill (luft:chunk-fibers-words fibers) 0)
  (when emission-grid (fill emission-grid 0))
  (let* ((domain (world-source-domain source))
         (vocabulary (world-source-material-vocabulary source))
         (members (domains:identity-vocabulary-members vocabulary))
         (fixture-kinds
           (map 'simple-vector
                (lambda (placement)
                  (let ((kind (material-placement-kind placement)))
                    (and (material-kind-attachment-form kind) kind)))
                members))
         (x0 (* tx luft:+chunk-size+))
         (y0 (* ty luft:+chunk-size+))
         (x-end (+ x0 luft:+chunk-size+))
         (y-end (+ y0 luft:+chunk-size+))
         (key (luft:chunk-key-at x0 y0))
         (offsets (make-hash-table :test #'eq))
         (seeded-air nil)
         (authored-air-count 0)
         (fibers-words (luft:chunk-fibers-words fibers)))
    (declare (type fixnum x0 y0 x-end y-end head authored-air-count))
    (flet ((place-cell (x y z offset)
             (declare (type fixnum x y z offset))
             (let* ((lx (- x x0))
                    (ly (- y y0))
                    (col (material-column-index lx ly))
                    (idx (+ z (ash col 8)))
                    (code (1+ offset)))
               (setf (aref grid idx) code)
               (let ((fword (+ (luft:fiber-base lx ly) (ash z -6)))
                     (fbit (logand z 63)))
                 (setf (aref fibers-words fword)
                       (logior (aref fibers-words fword) (ash 1 fbit))))
               (when (and emission-grid emission-table)
                 (setf (aref emission-grid idx) (aref emission-table offset))))))
      (when (and (<= 0 x0) (< x0 (luft:world-domain-x-limit domain))
                 (<= 0 y0) (< y0 (luft:world-domain-y-limit domain)))
        (generate-world-source-chunk
         source key
         (lambda (x y z placement)
           (declare (type fixnum x y z))
           (when (and (<= x0 x) (< x x-end)
                      (<= y0 y) (< y y-end)
                      (<= 0 z) (< z luft:+top-z+))
             (let ((offset (or (gethash placement offsets)
                               (setf (gethash placement offsets)
                                     (domains:identity-vocabulary-offset
                                      vocabulary placement)))))
               (unless (aref fixture-kinds offset)
                 (place-cell x y z offset)))))))
      ;; Replay sparse edits in this chunk (including carved air = NIL).
      (when (and (<= 0 x0) (< x0 (luft:world-domain-x-limit domain))
                 (<= 0 y0) (< y0 (luft:world-domain-y-limit domain))
                 edits (plusp (hash-table-count edits)))
        (maphash
         (lambda (site value)
           (when (= key (luft:site-chunk-key site))
             (let* ((lx (logand (luft:site-x site) 63))
                    (ly (logand (luft:site-y site) 63))
                    (z (luft:site-z site))
                    (col (material-column-index lx ly))
                    (idx (+ z (ash col 8)))
                    (placement (world-value-placement value)))
               (if placement
                   (let ((offset (domains:identity-vocabulary-offset
                                  vocabulary placement)))
                     (if (aref fixture-kinds offset)
                         (when (and emission-grid emission-table)
                           ;; Keep fixture emission (lantern/torch) even when cell is non-cube.
                           (setf (aref emission-grid idx) (aref emission-table offset)))
                         (place-cell (luft:site-x site) (luft:site-y site) z offset)))
                   (progn
                     (setf (aref grid idx) 0)
                     (let ((fword (+ (luft:fiber-base lx ly) (ash z -6)))
                           (fbit (logand z 63)))
                       (setf (aref fibers-words fword)
                             (logandc2 (aref fibers-words fword) (ash 1 fbit))))
                     (when emission-grid (setf (aref emission-grid idx) 0))
                     (incf authored-air-count)
                     (push (+ (ash z -1)
                              (ash (+ (ash ly -1) (ash (ash lx -1) 5)) 7))
                           seeded-air))))))
         edits)))
    (values (delete-duplicates seeded-air) head authored-air-count)))

(defun %reduce-coarse-grid (fwords material-grid tx ty shift threshold
                            &key emission-grid (journal-head 0) (authored-air-cells 0))
  "Reduce 2×2×2 cells from a 64×64 grid with 256-cell column pitch."
  (declare (type (simple-array (unsigned-byte 64) (*)) fwords)
           (type (simple-array (unsigned-byte 8) (*)) material-grid)
           (type fixnum tx ty)
           (type (integer 1 2) shift)
           (type (integer 1 8) threshold)
           (optimize (speed 3) (safety 1)))
  (let* ((size-z (ash 256 (- shift)))
         (word-count (ceiling size-z 64))
         (cwords (make-array (* 32 32 word-count) :element-type '(unsigned-byte 64) :initial-element 0))
         (cmats (make-array (* 32 32 size-z) :element-type '(unsigned-byte 8) :initial-element 0))
         (emissions nil))
    (dotimes (cx 32)
      (let ((lx0 (* 2 cx)))
        (declare (type fixnum lx0))
        (dotimes (cy 32)
          (let* ((ly0 (* 2 cy))
                 (b00 (luft:fiber-base lx0 ly0))
                 (b10 (luft:fiber-base (1+ lx0) ly0))
                 (b01 (luft:fiber-base lx0 (1+ ly0)))
                 (b11 (luft:fiber-base (1+ lx0) (1+ ly0)))
                 (cbase (* word-count (+ cy (* 32 cx))))
                 (col-off (* (+ cy (ash cx 5)) size-z)))
            (declare (type fixnum ly0 b00 b10 b01 b11 cbase col-off))
            (dotimes (w (floor size-z 32))
              (let ((w00 (aref fwords (+ b00 w)))
                    (w10 (aref fwords (+ b10 w)))
                    (w01 (aref fwords (+ b01 w)))
                    (w11 (aref fwords (+ b11 w)))
                    (out 0))
                (declare (type (unsigned-byte 64) w00 w10 w01 w11)
                         (type (unsigned-byte 32) out))
                (unless (zerop (logior w00 w10 w01 w11))
                  (let ((lz-base (* w 64))
                        (cz-base (* w 32)))
                    (declare (type fixnum lz-base cz-base))
                    (dotimes (pair 32)
                      (let* ((b (* pair 2))
                             (b1 (1+ b))
                             (cnt (+ (ldb (byte 1 b) w00) (ldb (byte 1 b1) w00)
                                     (ldb (byte 1 b) w10) (ldb (byte 1 b1) w10)
                                     (ldb (byte 1 b) w01) (ldb (byte 1 b1) w01)
                                     (ldb (byte 1 b) w11) (ldb (byte 1 b1) w11)))
                             (cz (+ cz-base pair))
                             (cidx (+ col-off cz)))
                        (declare (type fixnum b b1 cnt cz cidx))
                        (when (>= cnt threshold)
                          (setf out (logior out (ash 1 pair)))
                          (let ((code (%vote-2x2x2-material
                                       material-grid lx0 ly0 (+ lz-base b))))
                            (setf (aref cmats cidx) (if (plusp code) code 1))))
                        ;; Also downsample any emissive light in this 2x2x2 block
                        ;; even when fixture sits just above air-adjacent support.
                        (when emission-grid
                          (let ((emit 0)
                                (lz (+ lz-base b)))
                            (declare (type (unsigned-byte 16) emit)
                                     (type fixnum lz))
                            (dotimes (dz 2)
                              (when (< (+ lz dz) luft:+top-z+)
                                (dotimes (dy 2)
                                  (dotimes (dx 2)
                                    (let ((e (aref emission-grid
                                                   (+ lz dz (ash (+ (+ lx0 dx) (ash (+ ly0 dy) 6)) 8)))))
                                      (unless (zerop e)
                                        (setf emit (luft:voxel-light-componentwise-max emit e))))))))
                            (unless (zerop emit)
                              (unless emissions
                                (setf emissions (make-hash-table :test #'eql)))
                              (setf (gethash cidx emissions) emit))))))))
                (unless (zerop out)
                  (let ((cw (ash w -1))
                        (shift (if (oddp w) 32 0)))
                    (setf (aref cwords (+ cbase cw))
                          (logior (aref cwords (+ cbase cw)) (ash out shift)))))))))))
    (%make-coarse-voxel-tile tx ty shift 32 size-z word-count cwords cmats
                             :emissions emissions
                             :journal-head (or journal-head 0)
                             :authored-air-cells authored-air-cells)))

(defun downsample-chunk-to-coarse-tile
    (fibers material-grid tx ty
     &key (threshold 4) (cull-interior-caves-p t)
          seeded-air journal-head (authored-air-cells 0)
          emission-grid fallback-code)
  "Downsample one 64x64x255 fine FIBERS + MATERIAL-GRID into a 32x32x128 COARSE-VOXEL-TILE.

THRESHOLD (1..8) sets how many solid subcells in each 2x2x2 block make the coarse
cell solid (4 = symmetric majority).  Boundary-cell material voting (#NDXUW8) avoids
underground stone drowning 1-cell grass/snow/masonry skins."
  (let ((tile (%reduce-coarse-grid (luft:chunk-fibers-words fibers) material-grid tx ty 1 threshold
                                   :emission-grid emission-grid :journal-head journal-head
                                   :authored-air-cells authored-air-cells)))
    (when cull-interior-caves-p
      (%cull-sealed-coarse-pockets (coarse-voxel-tile-words tile)
                                   (coarse-voxel-tile-materials tile) seeded-air fallback-code))
    tile))

(defun build-world-source-coarse-tile
    (source tx ty &key (threshold 4) (cull-interior-caves-p t)
                       grid-scratch fibers-scratch emission-scratch
                       (edits (world-source-edits source))
                       (head (world-journal-head (world-source-journal source))))
  "Generate chunk (TX, TY) from SOURCE and downsample it to a stride-2 COARSE-VOXEL-TILE."
  (let* ((domain (world-source-domain source))
         (vocab (world-source-material-vocabulary source))
         (members (domains:identity-vocabulary-members vocab))
         (emission-table
           (let ((tbl (make-array (length members) :element-type '(unsigned-byte 16)
                                                   :initial-element 0)))
             (loop for placement across members
                   for i from 0
                   do (setf (aref tbl i)
                            (material-kind-packed-light-emission
                             (material-placement-kind placement))))
             tbl))
         (fallback-code
           (or (loop for placement across members
                     for i from 0
                     when (string-equal (symbol-name (material-placement-name placement))
                                        "STONE")
                       return (1+ i))
               1))
         (grid (or grid-scratch
                   (make-array (* 64 64 256) :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
         (fibers (or fibers-scratch
                     (luft:make-chunk-fibers domain (luft:chunk-key-at 0 0))))
         (egrids (or emission-scratch
                     (make-array (* 64 64 256) :element-type '(unsigned-byte 16)
                                               :initial-element 0))))
    (multiple-value-bind (seeded-air head authored-air-count)
        (%fill-world-source-chunk-fine-grid source tx ty grid fibers egrids emission-table edits head)
      (downsample-chunk-to-coarse-tile
       fibers grid tx ty
       :threshold threshold
       :cull-interior-caves-p cull-interior-caves-p
       :seeded-air seeded-air
       :journal-head head
       :authored-air-cells authored-air-count
       :emission-grid egrids
       :fallback-code fallback-code))))

;;; Meshing a coarse voxel tile directly to the 256-star GPU ABI ------------

(defun %coarse-pack-halo (tile neighbor-fn
                           &key near-xm-p near-xp-p near-ym-p near-yp-p)
  "Assemble 34x34x128 occupancy words, materials, top-z, and merged emissions
for TILE plus its 1-cell XY border from same-level NEIGHBOR-FN.  Missing neighbors clamp
their edge column to TILE's own border; neighbors covered by resident near chunks
drop by 2 coarse cells to form a downward seam skirt."
  (let* ((hwords (make-array (* 34 34 2) :element-type '(unsigned-byte 64) :initial-element 0))
         (hmats (make-array (* 34 34 128) :element-type '(unsigned-byte 8) :initial-element 0))
         (top-z (make-array (* 34 34) :element-type '(signed-byte 16) :initial-element -1))
         (emissions (make-hash-table :test #'eql))
         (size-z (coarse-voxel-tile-size-z tile))
         (word-count (coarse-voxel-tile-word-count tile))
         (base-words (coarse-voxel-tile-words tile))
         (base-mats (coarse-voxel-tile-materials tile))
         (base-emit (coarse-voxel-tile-emissions tile)))
    (declare (type (simple-array (unsigned-byte 64) (*)) hwords base-words)
             (type (simple-array (unsigned-byte 8) (*)) hmats base-mats)
             (type (simple-array (signed-byte 16) (*)) top-z)
             (optimize (speed 3) (safety 1)))
    (flet ((copy-shifted-column (src-words src-mats src-emit scx scy px py shift)
             (declare (type (simple-array (unsigned-byte 64) (*)) src-words)
                      (type (simple-array (unsigned-byte 8) (*)) src-mats)
                      (type fixnum scx scy px py shift))
             (let* ((sbase (* word-count (+ scy (* 32 scx))))
                    (hbase (* 2 (+ py (* 34 px))))
                    (soff (* (+ scy (ash scx 5)) size-z))
                    (hoff (ash (+ py (* 34 px)) 7))
                    (src-w0 (aref src-words sbase))
                    (src-w1 (if (= word-count 2) (aref src-words (1+ sbase)) 0))
                    (carry-bits (max 0 (min 63 shift)))
                    (w0 (if (zerop shift)
                            src-w0
                            (logand #xffffffffffffffff
                                    (logior (ash src-w0 (- shift))
                                            (if (plusp carry-bits)
                                                (ash (ldb (byte carry-bits 0) src-w1)
                                                     (- 64 shift))
                                                0)))))
                    (w1 (if (zerop shift) src-w1 (ash src-w1 (- shift))))
                    (keep (- size-z shift)))
               (declare (type fixnum sbase hbase soff hoff carry-bits keep)
                        (type (unsigned-byte 64) src-w0 src-w1 w0 w1))
               (setf (aref hwords hbase) w0
                     (aref hwords (1+ hbase)) w1)
               (when (plusp keep)
                 (replace hmats src-mats :start1 hoff :end1 (+ hoff keep)
                                         :start2 (+ soff shift) :end2 (+ soff size-z)))
               (when (< keep 128)
                 (fill hmats 0 :start (+ hoff keep) :end (+ hoff 128)))
               (setf (aref top-z (+ py (* 34 px)))
                     (cond ((plusp w1) (+ 64 (1- (integer-length w1))))
                           ((plusp w0) (1- (integer-length w0)))
                           (t -1)))
               (when (and src-emit (plusp (hash-table-count src-emit)))
                 (maphash (lambda (cidx emit)
                            (let ((rel (- cidx soff)))
                              (when (<= shift rel (1- size-z))
                                (setf (gethash (+ hoff (- rel shift)) emissions) emit))))
                          src-emit)))))
      ;; Interior 32x32 from TILE.
      (dotimes (cx 32)
        (dotimes (cy 32)
          (copy-shifted-column base-words base-mats base-emit cx cy (1+ cx) (1+ cy) 0)))
      ;; Borders & corners from neighbors, near-seam downward skirts, or clamped edges.
      (let ((n-xm (funcall neighbor-fn -1 0))
            (n-xp (funcall neighbor-fn 1 0))
            (n-ym (funcall neighbor-fn 0 -1))
            (n-yp (funcall neighbor-fn 0 1))
            (n-mm (funcall neighbor-fn -1 -1))
            (n-pm (funcall neighbor-fn 1 -1))
            (n-mp (funcall neighbor-fn -1 1))
            (n-pp (funcall neighbor-fn 1 1)))
        (flet ((copy-border (nbr near-p sx sy px py fx fy)
                 (cond
                   ((and nbr (not near-p))
                    (unless (= size-z (coarse-voxel-tile-size-z nbr))
                      (error "A coarse tile apron needs neighbors at the same LOD."))
                    (copy-shifted-column (coarse-voxel-tile-words nbr)
                                         (coarse-voxel-tile-materials nbr)
                                         (coarse-voxel-tile-emissions nbr)
                                         sx sy px py 0))
                   (t
                    (copy-shifted-column base-words base-mats base-emit
                                         fx fy px py (if near-p 2 0))))))
          (dotimes (i 32)
            (copy-border n-xm near-xm-p 31 i 0 (1+ i) 0 i)
            (copy-border n-xp near-xp-p 0 i 33 (1+ i) 31 i)
            (copy-border n-ym near-ym-p i 31 (1+ i) 0 i 0)
            (copy-border n-yp near-yp-p i 0 (1+ i) 33 i 31))
          (copy-border n-mm (or near-xm-p near-ym-p) 31 31 0 0 0 0)
          (copy-border n-pm (or near-xp-p near-ym-p) 0 31 33 0 31 0)
          (copy-border n-mp (or near-xm-p near-yp-p) 31 0 0 33 0 31)
          (copy-border n-pp (or near-xp-p near-yp-p) 0 0 33 33 31 31))))
    (values hwords hmats top-z (when (plusp (hash-table-count emissions)) emissions))))

(defun mesh-coarse-voxel-tile (tile neighbor-fn descriptors
                               &key close-x-plus-p close-y-plus-p
                                    near-xm-p near-xp-p near-ym-p near-yp-p)
  "Build GPU-ready packed star arrays for coarse TILE.

Returns (VALUES SITE-WORDS APPEARANCE-CODES LIGHT-WORDS STAR-COUNT).
When CLOSE-X-PLUS-P, CLOSE-Y-PLUS-P, NEAR-XP-P, or NEAR-YP-P is true, emit
the exposed +X/+Y seam slice at CX=32 or CY=32.  NEAR-*-P flags drop the
corresponding adjacent border column down 2 coarse cells as a seam skirt."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-bind (hwords hmats top-z emissions)
      (%coarse-pack-halo tile neighbor-fn
                         :near-xm-p near-xm-p
                         :near-xp-p near-xp-p
                         :near-ym-p near-ym-p
                         :near-yp-p near-yp-p)
    (declare (type (simple-array (unsigned-byte 64) (*)) hwords)
             (type (simple-array (unsigned-byte 8) (*)) hmats)
             (type (simple-array (signed-byte 16) (*)) top-z))
    (let* ((tx (coarse-voxel-tile-tx tile))
           (ty (coarse-voxel-tile-ty tile))
           (lod-shift (coarse-voxel-tile-lod-shift tile))
           (scale (ash 1 lod-shift))
           (tiers (and descriptors (terrain-descriptor-bevel-tiers descriptors)))
           (site-words (make-array 4096 :element-type '(unsigned-byte 32)
                                        :adjustable t :fill-pointer 0))
           (app-codes (make-array 8192 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0))
           (light-words (make-array 4096 :element-type '(unsigned-byte 32)
                                         :adjustable t :fill-pointer 0))
           (max-cx (if (or close-x-plus-p near-xp-p) 32 31))
           (max-cy (if (or close-y-plus-p near-yp-p) 32 31))
           (star-count 0))
      (declare (type fixnum tx ty max-cx max-cy star-count))
      (labels ((cell-solid-p (px py cz)
                 (declare (type fixnum px py cz))
                 (and (<= 0 px 33) (<= 0 py 33) (<= 0 cz 127)
                      (let ((w (+ (* 2 (+ py (* 34 px))) (ash cz -6))))
                        (logbitp (logand cz 63) (aref hwords w)))))
               (cell-code (px py cz)
                 (declare (type fixnum px py cz))
                 (if (and (<= 0 px 33) (<= 0 py 33) (<= 0 cz 127))
                     (aref hmats (+ cz (ash (+ py (* 34 px)) 7)))
                     0))
               (corner-light-and-tier (cx cy cz)
                 (declare (type fixnum cx cy cz))
                 ;; Lattice corner at tile-local (cx, cy, cz): surrounded by cells
                 ;; padded (cx + 0..1, cy + 0..1, cz - 1..0).
                 (let ((open-sky-p nil)
                       (portal-step 0)
                       (rgb 0)
                       (best-tier 4))
                   (declare (type fixnum portal-step rgb best-tier))
                   (dotimes (dx 2)
                     (let ((px (min 33 (+ cx dx))))
                       (dotimes (dy 2)
                         (let* ((py (min 33 (+ cy dy)))
                                (tz (aref top-z (+ py (* 34 px)))))
                           (when (>= cz tz) (setf open-sky-p t))
                           (dotimes (dz 2)
                             (let ((pz (+ cz dz -1)))
                               (when (and (<= 0 pz 127))
                                 (let ((c (cell-code px py pz)))
                                   (when (plusp c)
                                     (when tiers
                                       (let ((tval (aref tiers (1- c))))
                                         (when (< tval best-tier)
                                           (setf best-tier tval))))
                                     (when emissions
                                       (let ((e (gethash (+ pz (ash (+ py (* 34 px)) 7)) emissions 0)))
                                         (unless (zerop e)
                                           (setf rgb (luft:voxel-light-componentwise-max rgb e))))))))))))))
                   (unless open-sky-p
                     ;; Check 1-cell then 2-cell ring around the 2x2 column for open sky (tunnel entrance / overhang lip).
                     (loop for ox fixnum from (max 0 (1- cx)) to (min 33 (+ cx 2))
                           until (= portal-step 1) do
                             (loop for oy fixnum from (max 0 (1- cy)) to (min 33 (+ cy 2)) do
                               (when (>= cz (aref top-z (+ oy (* 34 ox))))
                                 (setf portal-step 1)
                                 (return))))
                     (when (zerop portal-step)
                       (loop for ox fixnum from (max 0 (- cx 2)) to (min 33 (+ cx 3))
                             until (= portal-step 2) do
                               (loop for oy fixnum from (max 0 (- cy 2)) to (min 33 (+ cy 3)) do
                                 (when (>= cz (aref top-z (+ oy (* 34 ox))))
                                   (setf portal-step 2)
                                   (return))))))
                   (let* ((sky (cond
                                 (open-sky-p 15)
                                 ((= portal-step 1) 12)
                                 ((= portal-step 2) 8)
                                 (t 4)))
                          (packed-light (logior (logand rgb #xfff) (ash (logand sky 15) 12)))
                          (tier (if (= best-tier 4) 1 best-tier)))
                     (values packed-light tier))))
               (compute-edge-shaping (cx cy cz star)
                 (declare (type fixnum cx cy cz star))
                 ;; Same 12-edge rule as %STAR-EDGE-SHAPING on padded coords (1-cell shift).
                 (let ((edges 0))
                   (declare (type (unsigned-byte 32) edges))
                   (dotimes (axis 3)
                     (unless (eq (logbitp 7 star) (logbitp (- 7 (ash 1 axis)) star))
                       (let* ((solid-is-hi (logbitp 7 star))
                              ;; All-positive cell is padded (cx+1, cy+1, cz).
                              (s-px (+ cx 1 (if (and (not solid-is-hi) (= axis 0)) -1 0)))
                              (s-py (+ cy 1 (if (and (not solid-is-hi) (= axis 1)) -1 0)))
                              (s-pz (+ cz   (if (and (not solid-is-hi) (= axis 2)) -1 0)))
                              (a-px (+ cx 1 (if (and solid-is-hi (= axis 0)) -1 0)))
                              (a-py (+ cy 1 (if (and solid-is-hi (= axis 1)) -1 0)))
                              (a-pz (+ cz   (if (and solid-is-hi (= axis 2)) -1 0))))
                         (dotimes (edge 4)
                           (let* ((tangent (mod (+ axis 1 (floor edge 2)) 3))
                                  (step (if (evenp edge) -1 1))
                                  (dx (if (= tangent 0) step 0))
                                  (dy (if (= tangent 1) step 0))
                                  (dz (if (= tangent 2) step 0))
                                  (s-occ (cell-solid-p (+ s-px dx) (+ s-py dy) (+ s-pz dz)))
                                  (a-occ (cell-solid-p (+ a-px dx) (+ a-py dy) (+ a-pz dz)))
                                  (cls (cond ((not s-occ) 1) (a-occ 2) (t 0))))
                             (setf (ldb (byte 2 (+ (* axis 8) (* edge 2))) edges) cls))))))
                   edges)))
        (dotimes (cy (1+ max-cy))
          (dotimes (cx (1+ max-cx))
            ;; Four neighboring column fiber bases in 34x34 halo for lattice (cx, cy):
            ;; sample 0..3 correspond to (cx, cy), (cx+1, cy), (cx, cy+1), (cx+1, cy+1)
            (let* ((b00 (* 2 (+ cy (* 34 cx))))
                   (b10 (* 2 (+ cy (* 34 (1+ cx)))))
                   (b01 (* 2 (+ (1+ cy) (* 34 cx))))
                   (b11 (* 2 (+ (1+ cy) (* 34 (1+ cx)))))
                   (wx (+ (* (+ (* tx 32) cx) scale) +coarse-tile-bias+))
                   (wy (+ (* (+ (* ty 32) cy) scale) +coarse-tile-bias+)))
              (declare (type fixnum b00 b10 b01 b11 wx wy))
              (dotimes (w 2)
                (let* ((a00 (aref hwords (+ b00 w)))
                       (a10 (aref hwords (+ b10 w)))
                       (a01 (aref hwords (+ b01 w)))
                       (a11 (aref hwords (+ b11 w)))
                       ;; Treat cz = -1 (w = 0, bit 0 of p00..p11) as solid bedrock so
                       ;; buried ground does not emit invisible z = 0 underside stars.
                       (p00 (if (zerop w)
                                (logior 1 (logand #xffffffffffffffff (ash a00 1)))
                                (logior (logand #xffffffffffffffff (ash a00 1))
                                        (ldb (byte 1 63) (aref hwords b00)))))
                       (p10 (if (zerop w)
                                (logior 1 (logand #xffffffffffffffff (ash a10 1)))
                                (logior (logand #xffffffffffffffff (ash a10 1))
                                        (ldb (byte 1 63) (aref hwords b10)))))
                       (p01 (if (zerop w)
                                (logior 1 (logand #xffffffffffffffff (ash a01 1)))
                                (logior (logand #xffffffffffffffff (ash a01 1))
                                        (ldb (byte 1 63) (aref hwords b01)))))
                       (p11 (if (zerop w)
                                (logior 1 (logand #xffffffffffffffff (ash a11 1)))
                                (logior (logand #xffffffffffffffff (ash a11 1))
                                        (ldb (byte 1 63) (aref hwords b11)))))
                       (any (logior a00 a10 a01 a11 p00 p10 p01 p11))
                       (all (logand a00 a10 a01 a11 p00 p10 p01 p11))
                       (active (logand any (logandc1 all #xffffffffffffffff))))
                  (declare (type (unsigned-byte 64) a00 a10 a01 a11 p00 p10 p01 p11 any all active))
                  (loop while (plusp active) do
                    (let* ((lowest (logand active (- active)))
                           (bit (1- (integer-length lowest)))
                           (cz (+ (* 64 w) bit))
                           (star (logior (if (logbitp bit p00) #x01 0)
                                         (if (logbitp bit p10) #x02 0)
                                         (if (logbitp bit p01) #x04 0)
                                         (if (logbitp bit p11) #x08 0)
                                         (if (logbitp bit a00) #x10 0)
                                         (if (logbitp bit a10) #x20 0)
                                         (if (logbitp bit a01) #x40 0)
                                         (if (logbitp bit a11) #x80 0)))
                           (edges (compute-edge-shaping cx cy cz star))
                           (wz (logior (* cz scale) (ash lod-shift 16) (ash 1 19))))
                      (declare (type fixnum bit cz star)
                               (type (unsigned-byte 32) edges wz))
                      (setf active (logand active (1- active)))
                      (incf star-count)
                      (vector-push-extend wx site-words)
                      (vector-push-extend wy site-words)
                      (vector-push-extend wz site-words)
                      (vector-push-extend (logior star (ash edges 8)) site-words)
                      ;; 8 material appearance codes.
                      (dotimes (sample 8)
                        (let ((px (+ cx (if (logbitp 0 sample) 1 0)))
                              (py (+ cy (if (logbitp 1 sample) 1 0)))
                              (pz (+ cz (if (logbitp 2 sample) 0 -1))))
                          (vector-push-extend
                           (if (logbitp sample star)
                               (let ((c (cell-code px py pz)))
                                 (if (plusp c) c 1))
                               0)
                           app-codes)))
                      ;; 7 lattice-corner lights + bevel tiers.
                      (let ((lw0 0) (lw1 0) (lw2 0) (lw3 0)
                            (tier-word (if tiers #x4000 0)))
                        (declare (type (unsigned-byte 32) lw0 lw1 lw2 lw3)
                                 (type (unsigned-byte 16) tier-word))
                        (dotimes (pt 7)
                          (multiple-value-bind (pl ptier)
                              (corner-light-and-tier
                               (+ cx (ldb (byte 1 0) pt))
                               (+ cy (ldb (byte 1 1) pt))
                               (+ cz (ldb (byte 1 2) pt)))
                            (when tiers
                              (setf tier-word (logior tier-word (ash ptier (* 2 pt)))))
                            (let ((shift (if (oddp pt) 16 0)))
                              (case (ash pt -1)
                                (0 (setf lw0 (logior lw0 (ash pl shift))))
                                (1 (setf lw1 (logior lw1 (ash pl shift))))
                                (2 (setf lw2 (logior lw2 (ash pl shift))))
                                (3 (setf lw3 (logior lw3 (ash pl shift))))))))
                        (setf lw3 (logior lw3 (ash tier-word 16)))
                        (vector-push-extend lw0 light-words)
                        (vector-push-extend lw1 light-words)
                        (vector-push-extend lw2 light-words)
                        (vector-push-extend lw3 light-words))))))))))
      (let ((final-sites (make-array (length site-words) :element-type '(unsigned-byte 32)))
            (final-apps (make-array (length app-codes) :element-type '(unsigned-byte 8)))
            (final-lights (make-array (length light-words) :element-type '(unsigned-byte 32))))
        (replace final-sites site-words)
        (replace final-apps app-codes)
        (replace final-lights light-words)
        (values final-sites final-apps final-lights star-count)))))
