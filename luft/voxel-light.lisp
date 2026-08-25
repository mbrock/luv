;;;; LUFT -- sparse colored voxel light
;;;;
;;;; Local light is an immutable derived field over cells.  Its semantic
;;;; boundary is RGB4, while its physical storage is a sparse set of 8^3 u16
;;;; pages.  A brightest-first packed frontier computes the componentwise-max
;;;; fixpoint without allocating coordinate objects in the hot loop.

(in-package #:luft)

(declaim (optimize (speed 3) (safety 2) (debug 1)))

(defconstant +maximum-voxel-light-level+ 15)
(defconstant +voxel-light-mesh-cell-size+ 8)
(defconstant +voxel-light-page-bits+ 3)
(defconstant +voxel-light-page-size+ (ash 1 +voxel-light-page-bits+))
(defconstant +voxel-light-page-cell-count+
  (* +voxel-light-page-size+
     +voxel-light-page-size+
     +voxel-light-page-size+))
(defconstant +voxel-light-value-bits+ 12)
(defconstant +voxel-light-value-mask+
  (1- (ash 1 +voxel-light-value-bits+)))
(defconstant +voxel-light-direct-page-limit+ 262144
  "Largest bounded page directory retained directly by a light field.")

(deftype voxel-light () '(unsigned-byte 12))
(deftype voxel-light-page ()
  '(simple-array (unsigned-byte 16) (512)))

(declaim (inline pack-voxel-light voxel-light-red voxel-light-green
                 voxel-light-blue %voxel-light-priority
                 voxel-light-componentwise-max))

(defun pack-voxel-light (red green blue)
  "Pack three local-light levels in the range 0..15 as RGB4."
  (check-type red (integer 0 15))
  (check-type green (integer 0 15))
  (check-type blue (integer 0 15))
  (logior red (ash green 4) (ash blue 8)))

(defun voxel-light-red (light)
  (check-type light voxel-light)
  (ldb (byte 4 0) light))

(defun voxel-light-green (light)
  (check-type light voxel-light)
  (ldb (byte 4 4) light))

(defun voxel-light-blue (light)
  (check-type light voxel-light)
  (ldb (byte 4 8) light))

(defun %voxel-light-priority (light)
  (max (voxel-light-red light)
       (voxel-light-green light)
       (voxel-light-blue light)))

(defun voxel-light-componentwise-max (a b)
  "Join two RGB4 values without allowing carry between color lanes."
  (check-type a voxel-light)
  (check-type b voxel-light)
  (pack-voxel-light (max (voxel-light-red a) (voxel-light-red b))
                    (max (voxel-light-green a) (voxel-light-green b))
                    (max (voxel-light-blue a) (voxel-light-blue b))))

(defstruct (voxel-light-field
             (:constructor %make-voxel-light-field
                 (domain pages direct-p x-pages y-pages page-count revision
                  visits pushes stale-pops))
             (:copier nil))
  (domain (make-world-domain) :type world-domain :read-only t)
  (pages nil :read-only t)
  (direct-p nil :type boolean :read-only t)
  (x-pages 0 :type (integer 0 *) :read-only t)
  (y-pages 0 :type (integer 0 *) :read-only t)
  (page-count 0 :type (integer 0 *) :read-only t)
  (revision 0 :type (integer 0 *) :read-only t)
  (visits 0 :type (integer 0 *) :read-only t)
  (pushes 0 :type (integer 0 *) :read-only t)
  (stale-pops 0 :type (integer 0 *) :read-only t))

(declaim (inline %voxel-light-page-key %voxel-light-page-index
                 %voxel-light-directory-index %voxel-light-page-at
                 %voxel-light-at-coordinates))

(defun %voxel-light-page-key (x y z)
  ;; X/Y have at most 17 bits and Z has 8.  After the 8-cell page shift,
  ;; 14 + 14 + 5 bits fit comfortably in a fixnum on supported hosts.
  (logior (ash x (- +voxel-light-page-bits+))
          (ash (ash y (- +voxel-light-page-bits+)) 14)
          (ash (ash z (- +voxel-light-page-bits+)) 28)))

(defun %voxel-light-page-index (x y z)
  (logior (logand z 7)
          (ash (logand y 7) 3)
          (ash (logand x 7) 6)))

(defun %voxel-light-directory-index (x-pages y-pages x y z)
  (declare (ignore x-pages))
  (+ (ash z -3)
     (* 32 (+ (ash y -3) (* y-pages (ash x -3))))))

(defun %voxel-light-page-at (pages direct-p x-pages y-pages x y z)
  (if direct-p
      (aref (the simple-vector pages)
            (%voxel-light-directory-index x-pages y-pages x y z))
      (gethash (%voxel-light-page-key x y z) (the hash-table pages))))

(defun %voxel-light-at-coordinates
    (pages direct-p x-pages y-pages x y z)
  (let ((page (%voxel-light-page-at
               pages direct-p x-pages y-pages x y z)))
    (if page
        (aref (the voxel-light-page page)
              (%voxel-light-page-index x y z))
        0)))

(defun %make-voxel-light-page-storage (domain)
  (let* ((x-pages
           (ceiling (world-domain-x-limit domain) +voxel-light-page-size+))
         (y-pages
           (ceiling (world-domain-y-limit domain) +voxel-light-page-size+))
         (directory-count (* x-pages y-pages 32))
         (direct-p (<= directory-count +voxel-light-direct-page-limit+)))
    (values (if direct-p
                (make-array directory-count :initial-element nil)
                (make-hash-table :test #'eql))
            direct-p x-pages y-pages)))

(defun voxel-light-at (field x y z)
  "Return FIELD's RGB4 value at an in-domain cell, or zero outside it."
  (check-type field voxel-light-field)
  (let ((domain (voxel-light-field-domain field)))
    (if (and (typep x '(integer 0 *))
             (typep y '(integer 0 *))
             (typep z '(integer 0 *))
             (< x (world-domain-x-limit domain))
             (< y (world-domain-y-limit domain))
             (< z +top-z+))
        (%voxel-light-at-coordinates
         (voxel-light-field-pages field)
         (voxel-light-field-direct-p field)
         (voxel-light-field-x-pages field)
         (voxel-light-field-y-pages field) x y z)
        0)))

(defun voxel-light-at-site (field cell)
  (check-type field voxel-light-field)
  (checked-site (voxel-light-field-domain field) cell)
  (unless (= (site-extent cell) +cell-extent+)
    (error "Voxel light is defined on cells, not site ~S." cell))
  (%voxel-light-at-coordinates
   (voxel-light-field-pages field)
   (voxel-light-field-direct-p field)
   (voxel-light-field-x-pages field)
   (voxel-light-field-y-pages field)
   (site-x cell) (site-y cell) (site-z cell)))

(defun voxel-light-at-lattice-point (field x y z)
  "Sample light at a cubical lattice point by joining its incident cells.

This canonical point-owned value is shared by every bevel primitive meeting
there, so independently emitted face, band, and fan instances remain lit
continuously."
  (check-type field voxel-light-field)
  (let ((answer 0))
    (declare (type voxel-light answer))
    (dolist (dx '(-1 0))
      (dolist (dy '(-1 0))
        (dolist (dz '(-1 0))
          (setf answer
                (voxel-light-componentwise-max
                 answer
                 (voxel-light-at field (+ x dx) (+ y dy) (+ z dz)))))))
    answer))

(defun %voxel-light-lattice-key (x y z)
  ;; Z lattice coordinates need nine bits and a maximum-size horizontal
  ;; domain's far boundary is the inclusive coordinate 2^17, so each
  ;; horizontal lane reserves eighteen bits.
  (logior z (ash y 9) (ash x 27)))

(defun voxel-light-at-mesh-point
    (field tick-x tick-y tick-z &optional lattice-cache)
  "Trilinearly sample FIELD at an exact LUFT mesh-tick position.

The result remains RGB4.  Sampling is a pure function of world position, so
separate face, band, fan, and attachment instances agree at shared vertices,
including the width-four medial limit where distinct site owners coincide.
When supplied, LATTICE-CACHE memoizes canonical cubical-corner readings across
many mesh points in one derived render population."
  (check-type field voxel-light-field)
  (labels ((axis (tick)
             (multiple-value-bind (low remainder)
                 (floor tick +voxel-light-mesh-cell-size+)
               (values low remainder
                       (- +voxel-light-mesh-cell-size+ remainder))))
           (lattice-light (x y z)
             (if lattice-cache
                 (let ((key (%voxel-light-lattice-key x y z)))
                   (multiple-value-bind (light present-p)
                       (gethash key lattice-cache)
                     (if present-p
                         light
                         (setf (gethash key lattice-cache)
                               (voxel-light-at-lattice-point field x y z)))))
                 (voxel-light-at-lattice-point field x y z))))
    (multiple-value-bind (x0 x1w x0w) (axis tick-x)
      (multiple-value-bind (y0 y1w y0w) (axis tick-y)
        (multiple-value-bind (z0 z1w z0w) (axis tick-z)
          (let ((red 0) (green 0) (blue 0))
            (dotimes (dx 2)
              (dotimes (dy 2)
                (dotimes (dz 2)
                  (let* ((weight (* (if (zerop dx) x0w x1w)
                                    (if (zerop dy) y0w y1w)
                                    (if (zerop dz) z0w z1w)))
                         (light
                           (lattice-light
                            (+ x0 dx) (+ y0 dy) (+ z0 dz))))
                    (incf red (* weight (voxel-light-red light)))
                    (incf green (* weight (voxel-light-green light)))
                    (incf blue (* weight (voxel-light-blue light)))))))
            (flet ((rounded-lane (sum)
                     (min +maximum-voxel-light-level+
                          (round sum (expt +voxel-light-mesh-cell-size+ 3)))))
              (pack-voxel-light (rounded-lane red)
                                (rounded-lane green)
                                (rounded-lane blue)))))))))

(defun make-voxel-light-source (cell light)
  "Pack an in-domain positive cell site and its RGB4 source value."
  (check-type cell site)
  (unless (and (= (site-extent cell) +cell-extent+)
               (site-positive-p cell))
    (error "A voxel-light source needs a positive cell site, got ~S." cell))
  (check-type light voxel-light)
  (logior cell (ash light 48)))

(declaim (inline %voxel-light-source-cell %voxel-light-source-value
                 %attenuate-voxel-light))

(defun %voxel-light-source-cell (source)
  (ldb (byte 48 0) source))

(defun %voxel-light-source-value (source)
  (ldb (byte +voxel-light-value-bits+ 48) source))

(defun %attenuate-voxel-light (light loss)
  (flet ((lane (level)
           (max 0 (- level loss))))
    (pack-voxel-light (lane (voxel-light-red light))
                      (lane (voxel-light-green light))
                      (lane (voxel-light-blue light)))))

(defun %material-opacity (material-cells opacity-table cell)
  (multiple-value-bind (material present-p) (gethash cell material-cells)
    (if present-p
        (if (< material (length opacity-table))
            (aref opacity-table material)
            (error "Material index ~D has no voxel-light opacity." material))
        0)))

(defun solve-voxel-light
    (domain material-cells opacity-table sources &key (revision 0))
  "Compute immutable colored local light with a packed brightest-first frontier.

MATERIAL-CELLS maps positive cell sites to dense material indices.
OPACITY-TABLE maps those indices to entered-cell loss in 0..15.  Every ordinary
step additionally costs one level.  SOURCES are values from
MAKE-VOXEL-LIGHT-SOURCE; a source illuminates its own cell regardless of that
cell's opacity.  Concurrent colors and sources meet by componentwise maximum."
  (check-type domain world-domain)
  (check-type material-cells hash-table)
  (check-type opacity-table (vector (unsigned-byte 8)))
  (check-type revision (integer 0 *))
  (multiple-value-bind (pages direct-p x-pages y-pages)
      (%make-voxel-light-page-storage domain)
    (let ((buckets (make-array 16))
          (visits 0)
          (pushes 0)
          (stale-pops 0)
          (page-count 0)
          (queued 0)
          (current-priority 0)
          (x-limit (world-domain-x-limit domain))
          (y-limit (world-domain-y-limit domain)))
      (dotimes (level 16)
        (setf (aref buckets level)
              (make-array 64 :element-type '(unsigned-byte 64)
                             :adjustable t :fill-pointer 0)))
      (labels ((light-at (x y z)
                 (%voxel-light-at-coordinates
                  pages direct-p x-pages y-pages x y z))
               (page-for-write (x y z)
                 (or (%voxel-light-page-at
                      pages direct-p x-pages y-pages x y z)
                     (let ((page
                             (make-array
                              +voxel-light-page-cell-count+
                              :element-type '(unsigned-byte 16)
                              :initial-element 0)))
                       (if direct-p
                           (setf (aref
                                  (the simple-vector pages)
                                  (%voxel-light-directory-index
                                   x-pages y-pages x y z))
                                 page)
                           (setf (gethash (%voxel-light-page-key x y z)
                                          (the hash-table pages))
                                 page))
                       (incf page-count)
                       page)))
               (admit (cell x y z proposed)
                 (let* ((old (light-at x y z))
                        (joined
                          (voxel-light-componentwise-max old proposed)))
                   (unless (= old joined)
                     (setf (aref (the voxel-light-page
                                      (page-for-write x y z))
                                 (%voxel-light-page-index x y z))
                           joined)
                     (let ((priority (%voxel-light-priority joined)))
                       (vector-push-extend
                        (logior cell (ash joined 48))
                        (aref buckets priority))
                       (setf current-priority
                             (max current-priority priority)))
                     (incf queued)
                     (incf pushes))))
               (drain-neighbor (x y z light dx dy dz)
                 (let ((target-x (+ x dx))
                       (target-y (+ y dy))
                       (target-z (+ z dz)))
                   (when (and (<= 0 target-x) (< target-x x-limit)
                              (<= 0 target-y) (< target-y y-limit)
                              (<= 0 target-z) (< target-z +top-z+))
                     (let* ((target
                              (make-site domain target-x target-y target-z
                                         +cell-extent+ 1))
                            (loss
                              (1+ (%material-opacity
                                   material-cells opacity-table target)))
                            (candidate
                              (%attenuate-voxel-light light loss)))
                       (unless (zerop candidate)
                         (admit target target-x target-y target-z
                                candidate)))))))
        (map nil
             (lambda (source)
               (let ((cell (%voxel-light-source-cell source))
                     (light (%voxel-light-source-value source)))
                 (checked-site domain cell)
                 (unless (= (site-extent cell) +cell-extent+)
                   (error "Voxel-light source ~S is not a cell." cell))
                 (admit cell (site-x cell) (site-y cell) (site-z cell)
                        light)))
             sources)
        ;; RGB joins can retain an old bright lane while improving a dim one.
        ;; That may re-admit a value to a bucket brighter than the source being
        ;; drained, so CURRENT-PRIORITY is dynamic rather than a one-way scan.
        (loop while (plusp queued) do
          (loop while (zerop (fill-pointer (aref buckets current-priority)))
                do (decf current-priority))
          (let* ((entry (vector-pop (aref buckets current-priority)))
                 (cell (%voxel-light-source-cell entry))
                 (light (%voxel-light-source-value entry))
                 (x (site-x cell))
                 (y (site-y cell))
                 (z (site-z cell)))
            (decf queued)
            (if (/= light (light-at x y z))
                (incf stale-pops)
                (progn
                  (incf visits)
                  (drain-neighbor x y z light -1 0 0)
                  (drain-neighbor x y z light 1 0 0)
                  (drain-neighbor x y z light 0 -1 0)
                  (drain-neighbor x y z light 0 1 0)
                  (drain-neighbor x y z light 0 0 -1)
                  (drain-neighbor x y z light 0 0 1)))))
        (%make-voxel-light-field
         domain pages direct-p x-pages y-pages page-count revision
         visits pushes stale-pops)))))
