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
  (logior red (ash green #.(third (light-point-byte 1)))
          (ash blue #.(third (light-point-byte 2)))))

(defun voxel-light-red (light)
  (check-type light voxel-light)
  (ldb #.(light-point-byte 0) light))

(defun voxel-light-green (light)
  (check-type light voxel-light)
  (ldb #.(light-point-byte 1) light))

(defun voxel-light-blue (light)
  (check-type light voxel-light)
  (ldb #.(light-point-byte 2) light))

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
  ;; One semantic owner per field, never one tag per page element.
  (definition (luv.fields:field-definition-for :luft-rgb4)
              :type luv.fields:voxel-field-definition)
  (pages nil :read-only t)
  (direct-p nil :type boolean :read-only t)
  (x-pages 0 :type (integer 0 *) :read-only t)
  (y-pages 0 :type (integer 0 *) :read-only t)
  (page-count 0 :type (integer 0 *) :read-only t)
  (revision 0 :type (integer 0 *) :read-only t)
  ;; Implicit direct-column light avoids materializing the open atmosphere.
  (baseline nil :type (or null function))
  ;; Optional exact sky witness: (X,Y) -> immutable u8 column, NIL (open),
  ;; or :UNKNOWN (closed). Equal witnesses imply equal baseline at every Z.
  (baseline-column nil :type (or null function))
  (skylight nil :type (or null voxel-light-field))
  (visits 0 :type (integer 0 *) :read-only t)
  (pushes 0 :type (integer 0 *) :read-only t)
  (stale-pops 0 :type (integer 0 *) :read-only t))

(defmethod luv.fields:materialized-field-definition
    ((field voxel-light-field) name)
  (let ((definition (voxel-light-field-definition field)))
    (when (eq name (luv.fields:voxel-field-definition-name definition))
      definition)))

(defmethod luv.fields:materialized-field-representation
    ((field voxel-light-field) name)
  (when (luv.fields:materialized-field-definition field name) field))

(defmethod luv.fields:field-representation-domain ((field voxel-light-field))
  (voxel-light-field-domain field))

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

(defun %make-voxel-light-page-storage (domain &optional hashed-p)
  "Page storage for DOMAIN: a direct directory when small, else a hash table.

HASHED-P forces the hash table, for sparse fields over a small part of a
domain whose directory alone would outweigh their light."
  (let* ((x-pages
           (ceiling (world-domain-x-limit domain) +voxel-light-page-size+))
         (y-pages
           (ceiling (world-domain-y-limit domain) +voxel-light-page-size+))
         (directory-count (* x-pages y-pages 32))
         (direct-p (and (not hashed-p)
                        (<= directory-count +voxel-light-direct-page-limit+))))
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
        (let ((stored (%voxel-light-at-coordinates
                       (voxel-light-field-pages field)
                       (voxel-light-field-direct-p field)
                       (voxel-light-field-x-pages field)
                       (voxel-light-field-y-pages field) x y z)))
          (if (voxel-light-field-baseline field)
              (voxel-light-componentwise-max
               stored (funcall (voxel-light-field-baseline field) x y z))
              stored))
        0)))

(defun voxel-light-at-site (field cell)
  (check-type field voxel-light-field)
  (checked-site (voxel-light-field-domain field) cell)
  (unless (= (site-extent cell) +cell-extent+)
    (error "Voxel light is defined on cells, not site ~S." cell))
  (voxel-light-at field (site-x cell) (site-y cell) (site-z cell)))

(defun voxel-light-at-lattice-point (field x y z)
  "Sample light at a cubical lattice point by joining its incident cells.

This canonical point-owned value is shared by every bevel primitive meeting
there, so independently emitted face, band, and fan instances remain lit
continuously."
  (check-type field voxel-light-field)
  (let* ((domain (voxel-light-field-domain field))
         (pages (voxel-light-field-pages field))
         (direct-p (voxel-light-field-direct-p field))
         (xp (voxel-light-field-x-pages field))
         (yp (voxel-light-field-y-pages field))
         (baseline (voxel-light-field-baseline field))
         (red 0) (green 0) (blue 0))
    (declare (type fixnum x y z red green blue))
    ;; Most lattice points lie inside one page. Look it up once rather than
    ;; hashing all eight incident cells independently.
    (when (and (null baseline)
               (< 0 x (world-domain-x-limit domain))
               (< 0 y (world-domain-y-limit domain)) (< 0 z +top-z+)
               (not (zerop (logand x 7)))
               (not (zerop (logand y 7)))
               (not (zerop (logand z 7))))
      (let ((page (%voxel-light-page-at pages direct-p xp yp x y z))
            (index (%voxel-light-page-index (1- x) (1- y) (1- z))))
        (when page
          (dolist (offset '(0 1 8 9 64 65 72 73))
            (let ((light (aref (the voxel-light-page page) (+ index offset))))
              (setf red (max red (ldb (byte 4 0) light))
                    green (max green (ldb (byte 4 4) light))
                    blue (max blue (ldb (byte 4 8) light))))))
        (return-from voxel-light-at-lattice-point (pack-voxel-light red green blue))))
    (loop for cx fixnum from (max 0 (1- x)) below (min (1+ x) (world-domain-x-limit domain)) do
      (loop for cy fixnum from (max 0 (1- y)) below (min (1+ y) (world-domain-y-limit domain)) do
        (loop for cz fixnum from (max 0 (1- z)) below (min (1+ z) +top-z+) do
          (let* ((stored (%voxel-light-at-coordinates pages direct-p xp yp cx cy cz))
                 (light (if baseline
                            (voxel-light-componentwise-max
                             stored (funcall baseline cx cy cz))
                            stored)))
            (setf red (max red (ldb (byte 4 0) light))
                  green (max green (ldb (byte 4 4) light))
                  blue (max blue (ldb (byte 4 8) light)))))))
    (pack-voxel-light red green blue)))

(defun voxel-sky-at-lattice-point (field x y z)
  "Scalar sky join, stopping as soon as an incident cell sees the open sky."
  (let* ((answer 0)
         (domain (voxel-light-field-domain field))
         (baseline (voxel-light-field-baseline field)))
    (declare (type fixnum x y z answer))
    (loop for cx fixnum from (max 0 (1- x)) below (min (1+ x) (world-domain-x-limit domain)) do
      (loop for cy fixnum from (max 0 (1- y)) below (min (1+ y) (world-domain-y-limit domain)) do
        (loop for cz fixnum from (min z (1- +top-z+)) downto (max 0 (1- z)) do
          (when baseline
            (setf answer (max answer (the (integer 0 15) (funcall baseline cx cy cz))))
            (when (= answer 15) (return-from voxel-sky-at-lattice-point 15)))
          (setf answer
                (max answer (%voxel-light-at-coordinates
                             (voxel-light-field-pages field)
                             (voxel-light-field-direct-p field)
                             (voxel-light-field-x-pages field)
                             (voxel-light-field-y-pages field) cx cy cz)))
          (when (= answer 15) (return-from voxel-sky-at-lattice-point 15)))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (luv.frontier:define-frontier-program light-channel-addition
    :family :monotone-max-fixpoint
    :frontier-layout (:buckets 15)
    :neighborhood :voxel-face-relations
    :fields ((level :relaxed t) loss)
    :transfer (max (luv.frontier:as-field-quantity level 0)
                   (luv.frontier:as-field-quantity level
                     (- (level source)
                        (luv.frontier:as-field-quantity level (loss target)))))))

(defun %attenuate-voxel-light (light loss)
  ;; Packed RGB realizes the checked scalar law independently in each lane.
  (luv.frontier:with-frontier-kernel
      (light-channel-addition
       :fields (list (luv.frontier:make-frontier-field-binding 'level
                       :declaration (luv.fields:field-definition-for :luft-block-lane))
                     (luv.frontier:make-frontier-field-binding 'loss))
       (lane :transfer (((level source) level) ((loss target) loss))))
    (flet ((attenuate (level) (lane level loss)))
      (pack-voxel-light (attenuate (voxel-light-red light))
                        (attenuate (voxel-light-green light))
                        (attenuate (voxel-light-blue light))))))

(defun %material-opacity (material-cells opacity-table cell)
  (multiple-value-bind (material present-p)
      (etypecase material-cells
        (hash-table (gethash cell material-cells))
        (function (funcall material-cells cell)))
    (if present-p
        (if (< material (length opacity-table))
            (aref opacity-table material)
            (error "Material index ~D has no voxel-light opacity." material))
        0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (luv.frontier:define-frontier-program rgb4-light-addition
    :family :monotone-max-fixpoint
    :frontier-layout (:buckets 15)
    :neighborhood :voxel-face-relations
    :fields ((level :relaxed t) opacity)
    :transfer (%attenuate-voxel-light (level source) (1+ (opacity target)))
    :join (voxel-light-componentwise-max current candidate)
    :priority (%voxel-light-priority value))
  (defparameter *rgb4-paged-field-binding*
    (luv.frontier:make-frontier-field-binding 'level
      :declaration (luv.fields:field-definition-for :luft-rgb4)
      :read '(light-at x y z)
      :write '(setf (aref (the voxel-light-page (page-for-write x y z))
                         (%voxel-light-page-index x y z)) value))))

(declaim (inline voxel-light-transfer))
(defun voxel-light-transfer (light opacity)
  "The light program's transfer for seed interfaces and virtual sources."
  (luv.frontier:with-frontier-kernel
      (rgb4-light-addition
       (transfer :transfer (((level source) value) ((opacity target) loss))))
    (transfer light opacity)))

(defun solve-voxel-light
    (domain material-cells opacity-table sources
     &key (revision 0) region hashed-p baseline (field-name :luft-rgb4))
  "Compute immutable colored local light with a packed brightest-first frontier.

MATERIAL-CELLS is a hash table or a lookup function returning a dense material
index and presence flag for a positive cell site.
OPACITY-TABLE maps those indices to entered-cell loss in 0..15.  Every ordinary
step additionally costs one level.  SOURCES are values from
MAKE-VOXEL-LIGHT-SOURCE; a source illuminates its own cell regardless of that
cell's opacity.  Concurrent colors and sources meet by componentwise maximum.

REGION, a list (X0 Y0 X1 Y1) of half-open cell bounds, asks only for the light
of the columns inside it.  Every step costs at least one level, so a value at
horizontal L1 distance D outside REGION can still reach it only while its
brightest lane exceeds D; the frontier admits nothing else.  The field is then
exact inside REGION and a partial witness outside it. HASHED-P forces sparse
hashed page storage (#BQMYNU). BASELINE, when supplied, reads immutable
implicit RGB4 at X,Y,Z. SOURCES must include every interface at which that
baseline can improve a neighbour; only improvements over it enter the queue."
  (check-type domain world-domain)
  (check-type material-cells (or hash-table function))
  (check-type opacity-table (vector (unsigned-byte 8)))
  (check-type revision (integer 0 *))
  (luv.frontier:with-frontier-kernel
      (rgb4-light-addition
       (transfer :transfer (((level source) light) ((opacity target) opacity)))
       (join :join ((current old) (candidate proposed)))
       (priority :priority ((value light))))
  (multiple-value-bind (pages direct-p x-pages y-pages)
      (%make-voxel-light-page-storage domain hashed-p)
    (let ((buckets (make-array 16))
          (region-x0 (if region (first region) 0))
          (region-y0 (if region (second region) 0))
          (region-x1 (if region (third region) most-positive-fixnum))
          (region-y1 (if region (fourth region) most-positive-fixnum))
          (visits 0)
          (pushes 0)
          (stale-pops 0)
          (page-count 0)
          (cached-key -1)
          (cached-page nil)
          (queued 0)
          (current-priority 0)
          (x-limit (world-domain-x-limit domain))
          (y-limit (world-domain-y-limit domain)))
      (dotimes (level 16)
        (setf (aref buckets level)
              (make-array 64 :element-type '(unsigned-byte 64)
                             :adjustable t :fill-pointer 0)))
      (labels ((page-at (x y z)
                 ;; Frontier neighbours overwhelmingly share a page. This
                 ;; solve-local cache includes misses and never escapes.
                 (if direct-p
                     (%voxel-light-page-at pages t x-pages y-pages x y z)
                     (let ((key (%voxel-light-page-key x y z)))
                       (unless (= key cached-key)
                         (setf cached-key key cached-page (gethash key pages)))
                       cached-page)))
               (light-at (x y z)
                 (let* ((page (page-at x y z))
                        (stored (if page
                                    (aref (the voxel-light-page page)
                                          (%voxel-light-page-index x y z))
                                    0)))
                   (if baseline
                       (voxel-light-componentwise-max stored (funcall baseline x y z))
                       stored)))
               (page-for-write (x y z)
                 (or (page-at x y z)
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
                                          (the hash-table pages)) page
                                 cached-key (%voxel-light-page-key x y z)
                                 cached-page page))
                       (incf page-count)
                       page)))
               (region-distance (x y)
                 (+ (max 0 (- region-x0 x) (- x (1- region-x1)))
                    (max 0 (- region-y0 y) (- y (1- region-y1)))))
               (admit (cell x y z proposed)
                 (let* ((old (luv.frontier:frontier-field-access
                             *rgb4-paged-field-binding* :read (x x) (y y) (z z)))
                        (joined
                          (join old proposed)))
                   (unless (or (= old joined)
                               (<= (priority proposed)
                                   (region-distance x y)))
                     (luv.frontier:frontier-field-access
                      *rgb4-paged-field-binding* :write
                      (x x) (y y) (z z) (value joined))
                     (let ((priority (priority joined)))
                       (vector-push-extend
                        (logior cell (ash joined 48))
                        (aref buckets priority))
                       (setf current-priority
                             (max current-priority priority)))
                     (incf queued)
                     (incf pushes))))
               (drain-neighbor (cell x y z light dx dy dz)
                 (declare (type site cell) (type fixnum x y z)
                          (type voxel-light light) (type (integer -1 1) dx dy dz))
                 (let ((target-x (+ x dx))
                       (target-y (+ y dy))
                       (target-z (+ z dz)))
                   (when (and (<= 0 target-x) (< target-x x-limit)
                              (<= 0 target-y) (< target-y y-limit)
                              (<= 0 target-z) (< target-z +top-z+))
                     (let* ((target
                              ;; Only a horizontal chunk crossing needs a
                              ;; new Morton key. Everywhere else the packed
                              ;; local lanes can advance without revalidation.
                              (if (or (and (= dx -1) (zerop (logand x 63)))
                                      (and (= dx 1) (= (logand x 63) 63))
                                      (and (= dy -1) (zerop (logand y 63)))
                                      (and (= dy 1) (= (logand y 63) 63)))
                                  (make-site domain target-x target-y target-z +cell-extent+ 1)
                                  (+ cell (ash dx +x-local-shift+)
                                     (ash dy +y-local-shift+) (ash dz +z-shift+))))
                            (candidate
                              (transfer light (%material-opacity
                                               material-cells opacity-table target))))
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
                  (drain-neighbor cell x y z light -1 0 0)
                  (drain-neighbor cell x y z light 1 0 0)
                  (drain-neighbor cell x y z light 0 -1 0)
                  (drain-neighbor cell x y z light 0 1 0)
                  (drain-neighbor cell x y z light 0 0 -1)
                  (drain-neighbor cell x y z light 0 0 1)))))
        (let ((field (%make-voxel-light-field
                      domain pages direct-p x-pages y-pages page-count revision
                      visits pushes stale-pops)))
          (setf (voxel-light-field-baseline field) baseline
                (voxel-light-field-definition field)
                (luv.fields:field-definition-for field-name))
          field))))))

;;; ---------------------------------------------------------------------------
;;; Regional fields
;;;
;;; A streamed world solves its light one chunk column at a time (#BQMYNU).
;;; Each such tile owns only the pages over its own columns; a cohort's field
;;; joins the tiles it needs by sharing their immutable pages.

(defun map-voxel-light-pages (function field)
  "Call FUNCTION with the X, Y, and Z origin of each page of FIELD and the page."
  (check-type field voxel-light-field)
  (let ((pages (voxel-light-field-pages field))
        (y-pages (voxel-light-field-y-pages field)))
    (if (voxel-light-field-direct-p field)
        (loop for index from 0 below (length (the simple-vector pages))
              for page = (aref (the simple-vector pages) index)
              when page
                do (multiple-value-bind (column page-z) (floor index 32)
                     (multiple-value-bind (page-x page-y) (floor column y-pages)
                       (funcall function
                                (* page-x +voxel-light-page-size+)
                                (* page-y +voxel-light-page-size+)
                                (* page-z +voxel-light-page-size+)
                                page))))
        (maphash (lambda (key page)
                   (funcall function
                            (* (ldb (byte 14 0) key) +voxel-light-page-size+)
                            (* (ldb (byte 14 14) key) +voxel-light-page-size+)
                            (* (ash key -28) +voxel-light-page-size+)
                            page))
                 (the hash-table pages)))))

(defun voxel-light-page-present-p (field x y z)
  "Whether FIELD stores a page over in-domain cell X/Y/Z."
  (check-type field voxel-light-field)
  (not (null (%voxel-light-page-at
              (voxel-light-field-pages field)
              (voxel-light-field-direct-p field)
              (voxel-light-field-x-pages field)
              (voxel-light-field-y-pages field) x y z))))

(defun join-voxel-light-fields (domain fields &key (revision 0) region)
  "Return the componentwise join of FIELDS as one sparse field over DOMAIN.

Pages present in only one field are shared, never copied; overlapping pages
are joined into a fresh page.  REGION, a list (X0 Y0 X1 Y1), keeps only the
pages whose columns lie inside it, so a regional solve can keep exactly the
part of its field it is exact over."
  (check-type domain world-domain)
  (multiple-value-bind (pages direct-p x-pages y-pages)
      (%make-voxel-light-page-storage domain t)
    (declare (ignore direct-p))
    (let ((page-count 0)
          (visits 0)
          (pushes 0)
          (stale-pops 0))
      (dolist (field fields)
        (unless (equalp (voxel-light-field-domain field) domain)
          (error "Voxel-light field ~S belongs to another domain." field))
        (luv.arithmetic:ensure-declarations-compatible
         (voxel-light-field-definition field)
         (voxel-light-field-definition (first fields)))
        (incf visits (voxel-light-field-visits field))
        (incf pushes (voxel-light-field-pushes field))
        (incf stale-pops (voxel-light-field-stale-pops field))
        (map-voxel-light-pages
         (lambda (x y z page)
           (when (or (null region)
                     (and (<= (first region) x) (< x (third region))
                          (<= (second region) y) (< y (fourth region))))
             (let* ((key (%voxel-light-page-key x y z))
                    (old (gethash key pages)))
               (cond
                 ((null old)
                  (incf page-count)
                  (setf (gethash key pages) page))
                 ((not (eq old page))
                  (let ((joined (make-array +voxel-light-page-cell-count+
                                            :element-type '(unsigned-byte 16))))
                    (dotimes (index +voxel-light-page-cell-count+)
                      (setf (aref joined index)
                            (voxel-light-componentwise-max
                             (aref (the voxel-light-page old) index)
                             (aref (the voxel-light-page page) index))))
                    (setf (gethash key pages) joined)))))))
         field))
      (let ((result (%make-voxel-light-field
                     domain pages nil x-pages y-pages page-count revision
                     visits pushes stale-pops))
            (baselines (remove nil (mapcar #'voxel-light-field-baseline fields))))
        (when fields
          (setf (voxel-light-field-definition result)
                (voxel-light-field-definition (first fields))))
        (when baselines
          (setf (voxel-light-field-baseline result)
                (lambda (x y z)
                  (if (and region
                           (not (and (<= (first region) x) (< x (third region))
                                     (<= (second region) y) (< y (fourth region)))))
                      0
                      (reduce #'voxel-light-componentwise-max baselines
                              :key (lambda (baseline) (funcall baseline x y z))
                              :initial-value 0)))))
        result))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (luv.frontier:define-frontier-program bounded-light-invalidation
    :family :invalidation
    :frontier-layout :lifo-stack
    :neighborhood :page-columns
    :fields ((level :invalidated t))
    :constants (x0 y0 x1 y1)
    :admission (and (<= x0 x) (< x x1) (<= y0 y) (< y y1))))

(defun replace-voxel-light-region (domain old replacement region)
  "Invalidate page-aligned REGION in OLD and install its re-added REPLACEMENT.

Unchanged pages remain shared with the immutable old generation. Removed
pages are not joined back: zero in REPLACEMENT really removes old light.
The caller proves REGION contains the complete influence of its changes.
REPLACEMENT's implicit baseline must describe the whole retained tile."
  (when (and old replacement)
    (luv.arithmetic:ensure-declarations-compatible
     (voxel-light-field-definition old)
     (voxel-light-field-definition replacement)))
  (destructuring-bind (x0 y0 x1 y1) region
    (unless (every (lambda (x) (zerop (mod x 8))) region)
      (error "Light invalidation bounds must be page aligned: ~S." region))
    (luv.frontier:with-frontier-kernel
        (bounded-light-invalidation
         (invalid-p :admission ((x px) (y py) (x0 low-x) (y0 low-y) (x1 high-x) (y1 high-y))))
      (multiple-value-bind (pages direct-p xp yp)
          (%make-voxel-light-page-storage domain t)
        (declare (ignore direct-p))
        (when old
          (map-voxel-light-pages
           (lambda (x y z page)
             (unless (invalid-p x y x0 y0 x1 y1)
               (setf (gethash (%voxel-light-page-key x y z) pages) page))) old))
        (when replacement
          (map-voxel-light-pages
           (lambda (x y z page)
             (when (invalid-p x y x0 y0 x1 y1)
               (setf (gethash (%voxel-light-page-key x y z) pages) page))) replacement))
        (let ((field (%make-voxel-light-field
                      domain pages nil xp yp (hash-table-count pages) 0
                      (if replacement (voxel-light-field-visits replacement) 0)
                      (if replacement (voxel-light-field-pushes replacement) 0)
                      (if replacement (voxel-light-field-stale-pops replacement) 0))))
          (when (or replacement old)
            (setf (voxel-light-field-definition field)
                  (voxel-light-field-definition (or replacement old))))
          (when replacement
            (setf (voxel-light-field-baseline field)
                  (voxel-light-field-baseline replacement)
                  (voxel-light-field-baseline-column field)
                  (voxel-light-field-baseline-column replacement)))
          field)))))
