(in-package #:luft.render)

;;; Packing is a batch transform, not a sequence of sparse point queries.
;;; One demanded 8^3 lattice page reads a 9^3 cell halo from eight RGB/sky
;;; page pointers. Scratch cells temporarily expand RGB4/sky to byte lanes;
;;; PMAXUB then joins four corners at once. Storage and the GPU ABI stay u16.

#+x86-64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %join-four-light-corners
      ((simple-array (unsigned-byte 32) (*)) fixnum
       (simple-array (unsigned-byte 32) (*)) fixnum)
      (simple-array (unsigned-byte 32) (*))
      (sb-c:always-translatable) :overwrite-fndb-silently t)
  (sb-c:define-vop (%join-four-light-corners-vop)
    (:translate %join-four-light-corners)
    (:policy :fast-safe)
    (:args (input :scs (sb-vm::descriptor-reg))
           (index :scs (sb-vm::any-reg sb-vm::signed-reg sb-vm::unsigned-reg))
           (output :scs (sb-vm::descriptor-reg) :target result)
           (out-index :scs (sb-vm::any-reg sb-vm::signed-reg sb-vm::unsigned-reg)))
    (:arg-types sb-vm::simple-array-unsigned-byte-32 sb-vm::tagged-num
                sb-vm::simple-array-unsigned-byte-32 sb-vm::tagged-num)
    (:temporary (:sc sb-vm::int-sse-reg) maximum candidate)
    (:results (result :scs (sb-vm::descriptor-reg)))
    (:result-types sb-vm::simple-array-unsigned-byte-32)
    (:generator 8
      (flet ((address (array index offset)
               (sb-vm::ea (+ (* sb-vm::vector-data-offset sb-vm::n-word-bytes)
                             (- sb-vm::other-pointer-lowtag) (* 4 offset))
                          array index (sb-vm::index-scale 4 index))))
        (sb-vm::inst sb-x86-64-asm::movdqu maximum (address input index 0))
        (dolist (offset '(1 9 10 81 82 90 91))
          (sb-vm::inst sb-x86-64-asm::movdqu candidate (address input index offset))
          (sb-vm::inst sb-x86-64-asm::pmaxub maximum candidate))
        (sb-vm::inst sb-x86-64-asm::movdqu (address output out-index 0) maximum)
        (sb-vm::move result output)))))

#+x86-64
(defun %join-four-light-corners (input index output out-index)
  (%join-four-light-corners input index output out-index))

(declaim (inline %join-light-byte-lanes))
(defun %join-light-byte-lanes (a b)
  (declare (type (unsigned-byte 32) a b) (optimize (speed 3) (safety 1)))
  (logior (max (ldb (byte 8 0) a) (ldb (byte 8 0) b))
          (ash (max (ldb (byte 8 8) a) (ldb (byte 8 8) b)) 8)
          (ash (max (ldb (byte 8 16) a) (ldb (byte 8 16) b)) 16)
          (ash (max (ldb (byte 8 24) a) (ldb (byte 8 24) b)) 24)))

(defun %join-four-light-corners-scalar (input index output out-index)
  "Portable kernel and exact control for the SSE2 implementation."
  (declare (type (simple-array (unsigned-byte 32) (*)) input output)
           (type fixnum index out-index) (optimize (speed 3) (safety 1)))
  (dotimes (point 4 output)
    (let ((value 0))
      (dolist (offset '(0 1 9 10 81 82 90 91))
        (setf value (%join-light-byte-lanes value (aref input (+ index point offset)))))
      (setf (aref output (+ out-index point)) value))))

(defparameter +zero-star-light-page+
  (make-array 512 :element-type '(unsigned-byte 16) :initial-element 0)
  "Read-only substitute for missing input pages, never a write destination.")

(defstruct (star-light-scratch (:constructor make-star-light-scratch ()))
  (cells (make-array 729 :element-type '(unsigned-byte 32))
         :type (simple-array (unsigned-byte 32) (729)))
  (points (make-array 512 :element-type '(unsigned-byte 32))
          :type (simple-array (unsigned-byte 32) (512)))
  (rgb-pages (make-array 8) :type simple-vector)
  (sky-pages (make-array 8) :type simple-vector))

(declaim (inline %star-light-cache-index))
(defun %star-light-cache-index (x y z xmin ymin zmin height depth)
  "Z-contiguous, like the light pages and the corner kernel."
  (declare (type fixnum x y z xmin ymin zmin height depth)
           (optimize (speed 3) (safety 1)))
  (let* ((dx (- x xmin)) (dy (- y ymin)) (dz (- z zmin))
         (row (the fixnum (+ dy (the fixnum (* height dx))))))
    (declare (type fixnum dx dy dz))
    (the fixnum (+ dz (the fixnum (* depth row))))))

(defun %fill-light-corner-tile (field sky bx by bz scratch)
  "Load one cell halo, then join it into SCRATCH's lattice page in site order."
  (declare (type fixnum bx by bz)
           (optimize (speed 3) (safety 1)))
  (let* ((domain (luft:voxel-light-field-domain field))
         (xl (luft:world-domain-x-limit domain)) (yl (luft:world-domain-y-limit domain))
         (column-reader (and sky (luft::voxel-light-field-baseline-column sky)))
         (cells (star-light-scratch-cells scratch))
         (points (star-light-scratch-points scratch))
         (rgb (star-light-scratch-rgb-pages scratch))
         (skies (star-light-scratch-sky-pages scratch)))
    (labels ((pages (source directory)
               (dotimes (x 2)
                 (dotimes (y 2)
                   (dotimes (z 2)
                     (let ((cx (+ bx (* 8 (1- x))))
                           (cy (+ by (* 8 (1- y)))) (cz (+ bz (* 8 (1- z)))))
                       (setf (aref directory (+ z (* 2 y) (* 4 x)))
                             (or (and source (<= 0 cx) (< cx xl) (<= 0 cy) (< cy yl)
                                      (<= 0 cz) (< cz luft:+top-z+)
                                      (luft::%voxel-light-page-at
                                       (luft::voxel-light-field-pages source)
                                       (luft::voxel-light-field-direct-p source)
                                       (luft::voxel-light-field-x-pages source)
                                       (luft::voxel-light-field-y-pages source) cx cy cz))
                                 +zero-star-light-page+))))))))
      (pages field rgb) (pages sky skies))
    (dotimes (x 9)
      (dotimes (y 9)
        (let* ((cx (+ bx x -1)) (cy (+ by y -1))
               (column (if column-reader (funcall column-reader cx cy) nil))
               (base (+ (if (zerop x) 0 4) (if (zerop y) 0 2))))
          (dotimes (z 9)
            (let* ((cz (+ bz z -1))
                   (page-id (+ base (if (zerop z) 0 1)))
                   (index (luft::%voxel-light-page-index cx cy cz))
                   (light (aref (the luft::voxel-light-page (aref rgb page-id)) index))
                   (direct (if sky
                               (cond ((or (eq column :unknown) (< cx 0) (>= cx xl)
                                          (< cy 0) (>= cy yl) (< cz 0) (>= cz luft:+top-z+)) 0)
                                     ((null column) 15)
                                     (t (let ((column (the (simple-array (unsigned-byte 8) (*)) column)))
                                          (if (< cz (length column)) (aref column cz) 15))))
                               15))
                   (sky-light (max direct (aref (the luft::voxel-light-page (aref skies page-id)) index))))
              (setf (aref cells (+ z (* 9 y) (* 81 x)))
                    (logior (logand light 15) (ash (ldb (byte 4 4) light) 8)
                            (ash (ldb (byte 4 8) light) 16) (ash sky-light 24))))))))
    (dotimes (x 8)
      (dotimes (y 8)
        (loop for z fixnum from 0 below 8 by 4 do
          (#+x86-64 %join-four-light-corners
           #-x86-64 %join-four-light-corners-scalar
           cells (+ z (* 9 y) (* 81 x)) points (+ z (* 8 y) (* 64 x))))))
    points))

(defun %star-light-lattice-reader (field sky xmin ymin zmin width height depth cache)
  "Return a mesh-local demanded-page fill function, or NIL for general baselines.
A miss fills every lattice point of its page in CACHE, clipped to mesh bounds.
The outer seven-corner gather then reads the same compact cache directly."
  (declare (type fixnum xmin ymin zmin width height depth)
           (optimize (speed 3) (safety 1)))
  (when (or (luft::voxel-light-field-baseline field)
            (and sky (not (luft::voxel-light-field-baseline-column sky))))
    (return-from %star-light-lattice-reader nil))
  (let ((scratch (make-star-light-scratch)))
    (declare (type fixnum xmin ymin zmin width height depth)
             (type (simple-array (unsigned-byte 32) (*)) cache))
    (lambda (x y z)
      (declare (type fixnum x y z))
      (let ((bx (logandc2 x 7)) (by (logandc2 y 7)) (bz (logandc2 z 7)))
        (%fill-light-corner-tile field sky bx by bz scratch)
        (loop for cx fixnum from (max bx xmin) below (min (+ bx 8) (+ xmin width)) do
          (loop for cy fixnum from (max by ymin) below (min (+ by 8) (+ ymin height)) do
            (loop for cz fixnum from (max bz zmin) below (min (+ bz 8) (+ zmin depth)) do
              (let ((value (aref (star-light-scratch-points scratch)
                                 (luft::%voxel-light-page-index cx cy cz))))
                (setf (aref cache (%star-light-cache-index cx cy cz xmin ymin zmin height depth))
                      (logior (logand value 15) (ash (ldb (byte 4 8) value) 4)
                              (ash (ldb (byte 4 16) value) 8) (ash (ldb (byte 4 24) value) 12)))))))
        (aref cache (%star-light-cache-index x y z xmin ymin zmin height depth))))))
