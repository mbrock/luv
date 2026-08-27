(in-package #:luft)

;;; Focused executable claims for the retained topology and the replacement
;;; manifold-sheet mesher.

(defvar *luft-test-count* 0)
(defvar *luft-test-section* nil)

(defmacro %check (form &optional note)
  `(progn
     (incf *luft-test-count*)
     (unless ,form
       (error "LUFT test failed in ~A~@[ (~A)~]: ~S"
              *luft-test-section* ,note ',form))))

(defmacro %with-test-section ((name) &body body)
  `(let ((*luft-test-section* ,name)) ,@body))

(defun %signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun %chain-from-sites (domain sites)
  (let ((builder (make-chain-builder domain :initial-capacity (length sites))))
    (dolist (site sites) (chain-builder-add-site builder site))
    (finish-chain-builder builder)))

(defun %boundary-sites (domain site)
  (let ((parts '()))
    (map-site-boundary
     (lambda (part axis side)
       (declare (ignore axis side))
       (push part parts))
     domain site)
    (nreverse parts)))

(defun %test-sites-and-chains ()
  (%with-test-section ("packed sites, chains, and boundary")
    (let* ((domain (make-world-domain :x-bits 3 :y-bits 3))
           (cell (make-site domain 2 3 7 +cell-extent+ 1))
           (neighbor (make-site domain 3 3 7 +cell-extent+ 1))
           (solid (%chain-from-sites domain (list cell neighbor)))
           (surface (surface-chain solid)))
      (%check (= 2 (chain-count solid)))
      (%check (= 10 (chain-count surface)))
      (%check (= 6 (length (%boundary-sites domain cell))))
      (%check (site-valid-p domain cell))
      (%check (= cell (opposite-site (opposite-site cell))))
      (%check (= 1 (chain-cell-occupancy-bit solid 2 3 7)))
      (%check (= 0 (chain-cell-occupancy-bit solid 2 3 6)))
      (%check (null (site-backward domain
                                   (make-site domain 2 3 0) :z)))
      (%check (%signals-error-p
               (lambda ()
                 (make-site domain 2 3 +top-z+ +z-edge-extent+))))
      ;; Boundary squared remains the chain-level topological invariant.
      (%check (chain-empty-p (boundary-chain (boundary-chain solid))))
      ;; The value is immutable through its public vector view.
      (let ((copy (chain-sites solid)))
        (setf (aref copy 0) 0)
        (%check (= 2 (chain-count solid)))))))

(defun %edge-set= (left right)
  (equal (sort (copy-list left) #'<)
         (sort (copy-list right) #'<)))

(defun %test-sheet-decomposition ()
  (%with-test-section ("manifold sheet decomposition")
    (let ((singular-count 0))
      (dotimes (mask 256)
        (when (star-singular-p mask) (incf singular-count))
        (let ((cycles (%star-sheet-cycles mask)))
          (%check (= (length (%star-boundary-edges mask))
                     (loop for cycle in cycles sum (length cycle))))
          (%check (%edge-set= (%star-boundary-edges mask)
                              (loop for cycle in cycles append cycle)))))
      (%check (= 128 singular-count)))
    ;; Representatives of the two singular mechanisms and the maximally
    ;; crossed checkerboard resolve into the intended ordinary sheets.
    (%check (equal '(#x02 #x04)
                   (sort (decompose-star-mask #x06) #'<)))
    (%check (equal '(#x08 #x10)
                   (sort (decompose-star-mask #x18) #'<)))
    (%check (equal '(#x01 #x08 #x20 #x40)
                   (sort (decompose-star-mask #x69) #'<)))
    ;; This is the deliberate spike boundary: its occupied-side cycle needs a
    ;; covered junction and cannot be disguised as an ordinary eight-bit star.
    (%check (%signals-error-p (lambda () (decompose-star-mask #x6f))))))

(defun %test-voxel-light ()
  (%with-test-section ("packed RGB voxel-light frontier and face torches")
    (%check (= #xc3f (pack-voxel-light 15 3 12)))
    (%check (= (pack-voxel-light 11 9 13)
               (voxel-light-componentwise-max
                (pack-voxel-light 11 2 13)
                (pack-voxel-light 4 9 7))))
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (materials (make-hash-table :test #'eql))
           (opacity
             (make-array 1 :element-type '(unsigned-byte 8)
                           :initial-contents '(15)))
           (source-cell (make-site domain 8 8 8 +cell-extent+ 1))
           (wall-cell (make-site domain 9 8 8 +cell-extent+ 1)))
      ;; Placement offset zero is real opaque matter; absent cells are air.
      (setf (gethash wall-cell materials) 0)
      (let ((field
              (solve-voxel-light
               domain materials opacity
               (list (make-voxel-light-source
                      source-cell (pack-voxel-light 15 0 0))))))
        (%check (voxel-light-field-direct-p field))
        (%check (= 15 (voxel-light-red (voxel-light-at-site field source-cell))))
        (%check (= 14 (voxel-light-red (voxel-light-at field 7 8 8))))
        (%check (zerop (voxel-light-at-site field wall-cell)))
        (%check (plusp (voxel-light-field-visits field)))
        (%check (plusp (voxel-light-field-page-count field)))))
    ;; A source owns its emission before its own material is considered;
    ;; attenuation is paid only when the frontier enters the next cell.
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (materials (make-hash-table :test #'eql))
           (opacity
             (make-array 2 :element-type '(unsigned-byte 8)
                           :initial-contents '(15 1)))
           (source (make-site domain 8 8 8 +cell-extent+ 1))
           (transmissive (make-site domain 9 8 8 +cell-extent+ 1))
           (air (make-site domain 10 8 8 +cell-extent+ 1))
           (wall (make-site domain 11 8 8 +cell-extent+ 1)))
      (setf (gethash source materials) 0
            (gethash transmissive materials) 1
            (gethash wall materials) 0)
      (let ((field
              (solve-voxel-light
               domain materials opacity
               (list (make-voxel-light-source
                      source (pack-voxel-light 15 10 4))))))
        (%check (= (pack-voxel-light 15 10 4)
                   (voxel-light-at-site field source)))
        (%check (= (pack-voxel-light 13 8 2)
                   (voxel-light-at-site field transmissive)))
        (%check (= (pack-voxel-light 12 7 1)
                   (voxel-light-at-site field air)))
        (%check (zerop (voxel-light-at-site field wall)))))
    ;; A weighted detour is an independent max-fixpoint oracle: entering the
    ;; lossy direct cell is dimmer than walking four all-air edges around it.
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (materials (make-hash-table :test #'eql))
           (opacity
             (make-array 1 :element-type '(unsigned-byte 8)
                           :initial-element 4))
           (source (make-site domain 4 4 4 +cell-extent+ 1))
           (lossy (make-site domain 5 4 4 +cell-extent+ 1)))
      (setf (gethash lossy materials) 0)
      (let ((field
              (solve-voxel-light
               domain materials opacity
               (list (make-voxel-light-source
                      source (pack-voxel-light 15 0 0))))))
        (%check (= 10 (voxel-light-red (voxel-light-at-site field lossy))))
        (%check (= 11 (voxel-light-red (voxel-light-at field 6 4 4))))))
    ;; Improving red on a site that already retains blue level 15 re-enters
    ;; bucket 15.  The red lane must then continue through the site.
    (flet ((mixed-field (domain)
             (let ((opacity
                     (make-array 1 :element-type '(unsigned-byte 8)
                                   :initial-element 15)))
               (solve-voxel-light
                domain (make-hash-table :test #'eql) opacity
                (list
                 (make-voxel-light-source
                  (make-site domain 6 6 6 +cell-extent+ 1)
                  (pack-voxel-light 0 0 15))
                 (make-voxel-light-source
                  (make-site domain 5 6 6 +cell-extent+ 1)
                  (pack-voxel-light 10 0 0)))))))
      (let* ((direct (mixed-field (make-world-domain :horizontal-bits 4)))
             (sparse (mixed-field (make-world-domain :horizontal-bits 17)))
             (joined (voxel-light-at direct 6 6 6))
             (continued (voxel-light-at direct 7 6 6)))
        (%check (= 9 (voxel-light-red joined)))
        (%check (= 15 (voxel-light-blue joined)))
        (%check (= 8 (voxel-light-red continued)))
        (%check (= 14 (voxel-light-blue continued)))
        (%check (not (voxel-light-field-direct-p sparse)))
        (%check (= continued (voxel-light-at sparse 7 6 6)))
        ;; Mesh sampling is spatial, not reverse-mapped to an ambiguous site.
        (%check (= (pack-voxel-light 10 0 15)
                   (voxel-light-at-mesh-point direct 52 52 52)))
        (%check (= (voxel-light-at-mesh-point direct 52 52 52)
                   (voxel-light-at-mesh-point sparse 52 52 52)))
        ;; Inclusive far-boundary lattice coordinates do not alias the next
        ;; horizontal lane in the population sampler's memoization key.
        (%check (/= (%voxel-light-lattice-key 0 (ash 1 17) 0)
                    (%voxel-light-lattice-key 1 0 0)))))
    ;; Sparse attachments are resolved from the finished surface rather than
    ;; compiled as cubical, width-independent companion geometry.
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (cell (make-site domain 7 7 7 +cell-extent+ 1))
           (solid (%chain-from-sites domain (list cell)))
           (faces
             (loop for axis in '(:x :y :z)
                   append
                   (list (site-boundary-low domain cell axis)
                         (site-boundary-high domain cell axis)))))
      (labels ((dot (left right)
                 (loop for l across left for r across right sum (* l r)))
               (unit-p (vector)
                 (< (abs (- (sqrt (dot vector vector)) 1.0)) 1.0e-5)))
        (dolist (width '(1 2 3 4))
          (let ((mesh (make-surface-mesh solid :bevel-width width)))
            (dolist (face faces)
              (let* ((frame (resolve-surface-attachment-frame mesh face))
                     (normal (surface-attachment-frame-normal frame))
                     (tangent (surface-attachment-frame-tangent frame)))
                (multiple-value-bind (nx ny nz) (face-oriented-normal face)
                  (%check (> (+ (* nx (aref normal 0))
                                (* ny (aref normal 1))
                                (* nz (aref normal 2)))
                             0.9999)))
                (%check (unit-p normal))
                (%check (unit-p tangent))
                (%check (< (abs (dot normal tangent)) 1.0e-5))))))
        ;; One stable chart point is flat at width one and lies on the actual
        ;; diagonal bevel at widths two and four.  Both origin and frame must
        ;; therefore change with the realized geometry.
        (let* ((face (site-boundary-high domain cell :z))
               (narrow
                 (resolve-surface-attachment-frame
                  (make-surface-mesh solid :bevel-width 1) face :u 0.6d0))
               (medium
                 (resolve-surface-attachment-frame
                  (make-surface-mesh solid :bevel-width 2) face :u 0.6d0))
               (medial
                 (resolve-surface-attachment-frame
                  (make-surface-mesh solid :bevel-width 4) face :u 0.6d0))
               (narrow-normal (surface-attachment-frame-normal narrow))
               (medium-normal (surface-attachment-frame-normal medium))
               (medium-origin (surface-attachment-frame-origin medium))
               (medial-origin (surface-attachment-frame-origin medial)))
          (%check (equal '(:face)
                         (surface-attachment-frame-primitive-kinds narrow)))
          (%check (equal '(:band)
                         (surface-attachment-frame-primitive-kinds medium)))
          (%check (equal '(:junction)
                         (surface-attachment-frame-primitive-kinds medial)))
          (%check (> (aref narrow-normal 2) 0.9999))
          (%check (> (aref medium-normal 0) 0.7))
          (%check (> (aref medium-normal 2) 0.7))
          (%check (< (aref medium-origin 2) 8.0))
          (%check (< (aref medial-origin 2) (aref medium-origin 2))))
        ;; A separate parallel surface farther along the authored ray is not
        ;; part of the support cell's bevel slab and cannot steal the mount.
        (let* ((distant-cell
                 (make-site domain 7 7 9 +cell-extent+ 1))
               (two-surfaces
                 (%chain-from-sites domain (list cell distant-cell)))
               (face (site-boundary-high domain cell :z))
               (frame
                 (resolve-surface-attachment-frame
                  (make-surface-mesh two-surfaces :bevel-width 2) face)))
          (%check (< (abs (- 8.0
                            (aref (surface-attachment-frame-origin frame) 2)))
                     1.0e-6)))))))

(defun %solid-for-star (mask &key (centre '(8 8 8)))
  (let* ((domain (make-world-domain :horizontal-bits 5))
         (builder (make-chain-builder domain :initial-capacity 8)))
    (dotimes (sample 8)
      (when (logbitp sample mask)
        (let ((coordinates
                (loop for axis-number below 3
                      collect (+ (nth axis-number centre)
                                 (if (logbitp axis-number sample) 0 -1)))))
          (chain-builder-add-site
           builder
           (make-site domain
                      (first coordinates) (second coordinates)
                      (third coordinates) +cell-extent+ 1)))))
    (finish-chain-builder builder)))

(defun %asymmetric-site-bevel-width (x y z stocks)
  "Exercise every supported width without hiding axis-order mistakes."
  (declare (ignore stocks))
  (1+ (mod (+ x (* 2 y) (* 3 z)) 4)))

(defun %witness-site-stock-table (witness)
  "Materialize the callback contract independently through the list oracle."
  (let ((stocks-by-site (make-hash-table :test #'eql)))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (declare (ignore kind ambient mask normal))
       (dolist (point (list a b c))
         (multiple-value-bind (site direction)
             (%unit-bevel-point-site point)
           (declare (ignore direction))
           (pushnew stock
                    (gethash (%lattice-key (first site)
                                           (second site)
                                           (third site))
                             stocks-by-site)
                    :test #'=))))
     witness)
    (maphash (lambda (site stocks)
               (setf (gethash site stocks-by-site) (sort stocks #'<)))
             stocks-by-site)
    stocks-by-site))

(defun %same-surface-mesh-representation-p (left right)
  "Compare every retained field of two independently compiled meshes."
  (and (world-domain= (surface-mesh-domain left)
                      (surface-mesh-domain right))
       (= (surface-mesh-bevel-width left)
          (surface-mesh-bevel-width right))
       (= (surface-mesh-singular-star-count left)
          (surface-mesh-singular-star-count right))
       (= (surface-mesh-face-triangle-count left)
          (surface-mesh-face-triangle-count right))
       (= (surface-mesh-band-triangle-count left)
          (surface-mesh-band-triangle-count right))
       (= (surface-mesh-fan-triangle-count left)
          (surface-mesh-fan-triangle-count right))
       (equalp (surface-mesh-template-vertex-words left)
               (surface-mesh-template-vertex-words right))
       (equalp (surface-mesh-template-ranges left)
               (surface-mesh-template-ranges right))
       (equalp (surface-mesh-face-instance-words left)
               (surface-mesh-face-instance-words right))
       (equal (surface-mesh-face-draws left)
              (surface-mesh-face-draws right))
       (equalp (surface-mesh-band-instance-words left)
               (surface-mesh-band-instance-words right))
       (equal (surface-mesh-band-draws left)
              (surface-mesh-band-draws right))
       (equalp (surface-mesh-fan-instance-words left)
               (surface-mesh-fan-instance-words right))
       (equal (surface-mesh-fan-draws left)
              (surface-mesh-fan-draws right))))

(defun %test-source-stock-provenance ()
  (%with-test-section ("occupied source provenance for face stocks")
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (cell (make-site domain 7 6 5 +cell-extent+ 1))
           (solid (%chain-from-sites domain (list cell)))
           (stocks-by-face (make-hash-table :test #'eql))
           (expected-calls '())
           (observed-calls '())
           (next-stock 0))
      ;; This table is an independent face oracle for the legacy callback.
      ;; The provenance callback below never consults SOLID or reverse-probes
      ;; either incidence of FACE to recover the occupied cell.
      (dolist (axis '(:x :y :z))
        (dolist (side '(:forward :backward))
          (let ((face (if (eq side :forward)
                          (site-boundary-low domain cell axis)
                          (site-boundary-high domain cell axis))))
            (setf (gethash face stocks-by-face) next-stock)
            (push (list face cell axis side next-stock) expected-calls)
            (incf next-stock))))
      (labels ((face-stock (face)
                 (multiple-value-bind (stock present-p)
                     (gethash face stocks-by-face)
                   (unless present-p
                     (error "Unexpected oriented face ~S in stock oracle."
                            face))
                   stock))
               (source-stock (source-cell axis side)
                 ;; The provenance protocol no longer passes the oriented
                 ;; face; CELL, AXIS, and SIDE must reconstruct it exactly.
                 (let ((face
                         (if (eq side :forward)
                             (site-boundary-low domain source-cell axis)
                             (site-boundary-high domain source-cell axis))))
                   (unless (and (= source-cell cell)
                                (member axis '(:x :y :z))
                                (member side '(:forward :backward)))
                     (error "Bad source-stock provenance ~S."
                            (list source-cell axis side)))
                   (let ((stock (face-stock face)))
                     (pushnew (list face source-cell axis side stock)
                              observed-calls :test #'equal)
                     stock))))
        (let ((legacy
                (make-surface-mesh solid :stock-function #'face-stock))
              (provenance
                (make-surface-mesh
                 solid
                 :stock-function
                 (lambda (face)
                   (declare (ignore face))
                   (error "Legacy stock callback was not superseded."))
                 :source-stock-function #'source-stock)))
          (%check (= 6 (length observed-calls)))
          (%check (null (set-exclusive-or expected-calls observed-calls
                                          :test #'equal)))
          (%check (%same-surface-mesh-representation-p legacy provenance)))))))

(defun %surface-attachment-ray-oracle
    (meshes face &key (u 0.0d0) (v 0.0d0))
  "Return the pre-local-chart ray result, or NIL when that ray misses."
  (unless (listp meshes) (setf meshes (list meshes)))
  (multiple-value-bind (u-name v-name) (face-tangent-axes face)
    (multiple-value-bind (normal-x normal-y normal-z)
        (face-oriented-normal face)
      (let* ((authored-normal
               (mapcar #'coerce (list normal-x normal-y normal-z)
                       (make-list 3 :initial-element 'double-float)))
             (u-axis (%surface-frame-axis-vector u-name))
             (v-axis (%surface-frame-axis-vector v-name))
             (v-axis
               (if (minusp
                    (%surface-frame-dot
                     (%surface-frame-cross u-axis v-axis) authored-normal))
                   (%surface-frame-scale -1.0d0 v-axis)
                   v-axis))
             (center
               (loop for coordinate in
                     (list (site-x face) (site-y face) (site-z face))
                     for axis-number below 3
                     collect
                     (coerce
                      (+ (* +mesh-cell-size+ coordinate)
                         (if (logbitp axis-number (site-extent face))
                             (/ +mesh-cell-size+ 2)
                             0))
                      'double-float)))
             (chart-u (coerce u 'double-float))
             (chart-v (coerce v 'double-float))
             (chart-scale (max 1.0d0 (+ (abs chart-u) (abs chart-v))))
             (chart
               (%surface-frame+
                center
                (%surface-frame-scale
                 (* 0.5d0 +mesh-cell-size+ (/ chart-u chart-scale)) u-axis)
                (%surface-frame-scale
                 (* 0.5d0 +mesh-cell-size+ (/ chart-v chart-scale)) v-axis)))
             (maximum-inset
               (reduce #'max meshes :key #'surface-mesh-bevel-width))
             (tie-epsilon (* 1.0d-7 (max 1 maximum-inset)))
             (best-displacement nil)
             (hits nil))
        (dolist (mesh meshes)
          (%map-surface-mesh-triangle-records
           (lambda (kind stock ambient mask primitive-normal a b c)
             (declare (ignore ambient mask))
             (let* ((primitive-normal
                      (mapcar (lambda (value) (coerce value 'double-float))
                              primitive-normal))
                    (denominator
                      (%surface-frame-dot primitive-normal authored-normal)))
               (when (> denominator 1.0d-10)
                 (let* ((displacement
                          (/ (- (%surface-frame-dot primitive-normal a)
                                (%surface-frame-dot primitive-normal chart))
                             denominator))
                        (point
                          (%surface-frame+
                           chart
                           (%surface-frame-scale displacement
                                                 authored-normal))))
                   (when (and
                          (<= (- (+ maximum-inset tie-epsilon))
                              displacement tie-epsilon)
                          (%surface-frame-point-in-projected-triangle-p
                           point a b c u-axis v-axis))
                     (cond
                       ((or (null best-displacement)
                            (> displacement
                               (+ best-displacement tie-epsilon)))
                        (setf best-displacement displacement
                              hits
                              (list
                               (list kind stock primitive-normal point))))
                       ((<= (abs (- displacement best-displacement))
                            tie-epsilon)
                        (push (list kind stock primitive-normal point)
                              hits))))))))
           mesh))
        (when hits
          (let* ((unit-normals
                   (remove-duplicates
                    (mapcar (lambda (hit)
                              (%surface-frame-unit (third hit)))
                            hits)
                    :test (lambda (left right)
                            (> (%surface-frame-dot left right)
                               (- 1.0d0 1.0d-10)))))
                 (normal
                   (%surface-frame-unit
                    (reduce #'%surface-frame+ unit-normals)))
                 (projected-u
                   (%surface-frame+
                    u-axis
                    (%surface-frame-scale
                     (- (%surface-frame-dot u-axis normal)) normal)))
                 (tangent
                   (if (> (%surface-frame-dot projected-u projected-u)
                          1.0d-12)
                       (%surface-frame-unit projected-u)
                       (%surface-frame-unit
                        (%surface-frame+
                         v-axis
                         (%surface-frame-scale
                          (- (%surface-frame-dot v-axis normal)) normal)))))
                 (point (fourth (first hits))))
            (values point normal tangent
                    (sort (remove-duplicates (mapcar #'first hits))
                          #'< :key
                          (lambda (kind)
                            (ecase kind
                              (:face 0) (:band 1) (:junction 2))))
                    (sort (remove-duplicates (mapcar #'second hits)) #'<))))))))

(defun %check-surface-attachment-ray-preservation
    (frame meshes face u v)
  "Assert exact old-ray preservation when the independent oracle has a hit."
  (multiple-value-bind (point normal tangent primitive-kinds stocks)
      (%surface-attachment-ray-oracle meshes face :u u :v v)
    (when point
      (let ((expected-origin
              (map '(simple-array single-float (3))
                   (lambda (coordinate)
                     (coerce (/ coordinate +mesh-cell-size+) 'single-float))
                   point))
            (expected-normal
              (map '(simple-array single-float (3))
                   (lambda (component) (coerce component 'single-float))
                   normal))
            (expected-tangent
              (map '(simple-array single-float (3))
                   (lambda (component) (coerce component 'single-float))
                   tangent)))
        (%check (equalp expected-origin
                        (surface-attachment-frame-origin frame)))
        (%check (equalp expected-normal
                        (surface-attachment-frame-normal frame)))
        (%check (equalp expected-tangent
                        (surface-attachment-frame-tangent frame)))
        (%check (equal primitive-kinds
                       (surface-attachment-frame-primitive-kinds frame)))
        (%check (equal stocks (surface-attachment-frame-stocks frame)))))))

(defun %test-surface-attachment-square-chart ()
  (%with-test-section ("surface attachment square-chart realization")
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (cell (make-site domain 7 7 7 +cell-extent+ 1))
           (solid (%chain-from-sites domain (list cell)))
           (faces
             (loop for axis in '(:x :y :z)
                   append (list (site-boundary-low domain cell axis)
                                (site-boundary-high domain cell axis))))
           (samples
             '((0.0d0 0.0d0)
               (-1.0d0 0.0d0) (1.0d0 0.0d0)
               (0.0d0 -1.0d0) (0.0d0 1.0d0)
               (-1.0d0 -1.0d0) (-1.0d0 1.0d0)
               (1.0d0 -1.0d0) (1.0d0 1.0d0)))
           (epsilon 1.0d-4))
      (labels ((dot (left right)
                 (loop for axis below 3
                       sum (* (aref left axis) (aref right axis))))
               (cross (left right)
                 (vector (- (* (aref left 1) (aref right 2))
                            (* (aref left 2) (aref right 1)))
                         (- (* (aref left 2) (aref right 0))
                            (* (aref left 0) (aref right 2)))
                         (- (* (aref left 0) (aref right 1))
                            (* (aref left 1) (aref right 0)))))
               (unit-p (vector)
                 (< (abs (- (sqrt (dot vector vector)) 1.0d0)) 1.0d-5))
               (distance (left right)
                 (sqrt
                  (loop for axis below 3
                        for delta = (- (aref left axis) (aref right axis))
                        sum (* delta delta))))
               (check-frame (frame face)
                 (let* ((normal (surface-attachment-frame-normal frame))
                        (tangent (surface-attachment-frame-tangent frame))
                        (bitangent (cross normal tangent))
                        (recovered-normal (cross tangent bitangent)))
                   (multiple-value-bind (nx ny nz)
                       (face-oriented-normal face)
                     (%check (> (+ (* nx (aref normal 0))
                                   (* ny (aref normal 1))
                                   (* nz (aref normal 2)))
                                0.0)))
                   (%check (unit-p normal))
                   (%check (unit-p tangent))
                   (%check (unit-p bitangent))
                   (%check (< (abs (dot normal tangent)) 1.0d-5))
                   (%check (> (dot recovered-normal normal) 0.9999d0)))))
        (dolist (width '(1 2 3 4))
          (let ((mesh (make-surface-mesh solid :bevel-width width)))
            (dolist (face faces)
              (dolist (sample samples)
                (destructuring-bind (u v) sample
                  (let ((frame
                          (resolve-surface-attachment-frame
                           mesh face :u u :v v)))
                    (check-frame frame face)
                    (%check-surface-attachment-ray-preservation
                     frame mesh face u v))))
              ;; Each exact logical corner must also be the continuous limit
              ;; of both incident square-chart edges.  Only position is
              ;; compared: the exact point may intentionally carry a normal-
              ;; cone bisector while either neighboring point is smooth.
              (dolist (corner '((-1.0d0 -1.0d0) (-1.0d0 1.0d0)
                                (1.0d0 -1.0d0) (1.0d0 1.0d0)))
                (destructuring-bind (u v) corner
                  (let* ((exact
                           (resolve-surface-attachment-frame
                            mesh face :u u :v v))
                         (near-u
                           (resolve-surface-attachment-frame
                            mesh face :u (* u (- 1.0d0 epsilon)) :v v))
                         (near-v
                           (resolve-surface-attachment-frame
                            mesh face :u u :v (* v (- 1.0d0 epsilon))))
                         (origin
                           (surface-attachment-frame-origin exact)))
                    (%check
                     (< (distance origin
                                  (surface-attachment-frame-origin near-u))
                        1.0d-3))
                    (%check
                     (< (distance origin
                                  (surface-attachment-frame-origin near-v))
                        1.0d-3))))))))))))

(defun %surface-attachment-test-chart (face u v)
  "Return the independent mesh-tick chart geometry used by attachment tests."
  (multiple-value-bind (u-name v-name) (face-tangent-axes face)
    (multiple-value-bind (normal-x normal-y normal-z)
        (face-oriented-normal face)
      (let* ((normal
               (mapcar #'coerce (list normal-x normal-y normal-z)
                       (make-list 3 :initial-element 'double-float)))
             (u-axis (%surface-frame-axis-vector u-name))
             (v-axis (%surface-frame-axis-vector v-name))
             (v-axis
               (if (minusp
                    (%surface-frame-dot
                     (%surface-frame-cross u-axis v-axis) normal))
                   (%surface-frame-scale -1.0d0 v-axis)
                   v-axis))
             (center
               (loop for coordinate in
                     (list (site-x face) (site-y face) (site-z face))
                     for axis-number below 3
                     collect
                     (coerce
                      (+ (* +mesh-cell-size+ coordinate)
                         (if (logbitp axis-number (site-extent face))
                             (/ +mesh-cell-size+ 2)
                             0))
                      'double-float)))
             (u (coerce u 'double-float))
             (v (coerce v 'double-float))
             (scale (max 1.0d0 (+ (abs u) (abs v))))
             (chart
               (%surface-frame+
                center
                (%surface-frame-scale
                 (* 0.5d0 +mesh-cell-size+ (/ u scale)) u-axis)
                (%surface-frame-scale
                 (* 0.5d0 +mesh-cell-size+ (/ v scale)) v-axis))))
        (values center chart u-axis v-axis normal)))))

(defun %surface-attachment-test-candidate-normals
    (meshes face u v frame)
  "Return distinct eligible normals incident to FRAME's selected point."
  (unless (listp meshes) (setf meshes (list meshes)))
  (multiple-value-bind (center chart u-axis v-axis authored-normal)
      (%surface-attachment-test-chart face u v)
    (let* ((maximum-inset
             (reduce #'max meshes :key #'surface-mesh-bevel-width))
           (selection-epsilon (* 1.0d-7 (max 1 maximum-inset)))
           (point-epsilon 1.0d-5)
           (point-squared-epsilon (* point-epsilon point-epsilon))
           (maximum-radius-squared
             (* (+ maximum-inset selection-epsilon)
                (+ maximum-inset selection-epsilon)))
           (selected-point
             (loop for coordinate across
                   (surface-attachment-frame-origin frame)
                   collect (* +mesh-cell-size+
                              (coerce coordinate 'double-float))))
           (normals nil))
      (dolist (mesh meshes)
        (%map-surface-mesh-triangle-records
         (lambda (kind stock ambient mask primitive-normal a b c)
           (declare (ignore kind stock ambient mask))
           (let* ((primitive-normal
                    (mapcar (lambda (value) (coerce value 'double-float))
                            primitive-normal))
                  (denominator
                    (%surface-frame-dot primitive-normal authored-normal)))
             (when (> denominator 1.0d-10)
               (multiple-value-bind (point radius-squared)
                   (%surface-frame-nearest-projected-triangle-point
                    chart a b c u-axis v-axis authored-normal
                    primitive-normal denominator)
                 (let ((displacement
                         (%surface-frame-dot
                          (%surface-frame+
                           point (%surface-frame-scale -1.0d0 chart))
                          authored-normal)))
                   (when (and
                          (<= (- (+ maximum-inset selection-epsilon))
                              displacement selection-epsilon)
                          (<= radius-squared maximum-radius-squared)
                          (%surface-frame-point-in-support-footprint-p
                           point center u-axis v-axis selection-epsilon)
                          (<= (%surface-frame-point-distance-squared
                               point selected-point)
                              point-squared-epsilon))
                     (push (%surface-frame-unit primitive-normal)
                           normals)))))))
         mesh))
      (remove-duplicates
       normals
       :test (lambda (left right)
               (> (%surface-frame-dot left right)
                  (- 1.0d0 1.0d-10)))))))

(defun %check-local-surface-attachment-frame
    (meshes face u v width frame)
  "Check local ownership, actual incidence, and an orthonormal frame."
  (multiple-value-bind (center chart u-axis v-axis authored-normal)
      (%surface-attachment-test-chart face u v)
    (let* ((point
             (loop for coordinate across
                   (surface-attachment-frame-origin frame)
                   collect (* +mesh-cell-size+
                              (coerce coordinate 'double-float))))
           (normal
             (loop for component across
                   (surface-attachment-frame-normal frame)
                   collect (coerce component 'double-float)))
           (tangent
             (loop for component across
                   (surface-attachment-frame-tangent frame)
                   collect (coerce component 'double-float)))
           (offset (%surface-frame+
                    point (%surface-frame-scale -1.0d0 chart)))
           (displacement (%surface-frame-dot offset authored-normal))
           (radius-squared
             (max 0.0d0
                  (- (%surface-frame-dot offset offset)
                     (* displacement displacement))))
           (center-offset
             (%surface-frame+
              point (%surface-frame-scale -1.0d0 center)))
           (bitangent (%surface-frame-cross normal tangent))
           (incident-normals
             (%surface-attachment-test-candidate-normals
              meshes face u v frame))
           (cone-normal
             (and incident-normals
                  (%surface-frame-unit
                   (reduce #'%surface-frame+ incident-normals))))
           (epsilon (* 1.0d-5 (max 1 width))))
      (%check incident-normals)
      (%check (> (%surface-frame-dot normal authored-normal) 0.0d0))
      (%check (< (abs (- (%surface-frame-dot normal normal) 1.0d0))
                 epsilon))
      (%check (< (abs (- (%surface-frame-dot tangent tangent) 1.0d0))
                 epsilon))
      (%check (< (abs (- (%surface-frame-dot bitangent bitangent) 1.0d0))
                 epsilon))
      (%check (< (abs (%surface-frame-dot normal tangent)) epsilon))
      (%check (> (%surface-frame-dot
                  (%surface-frame-cross tangent bitangent) normal)
                 (- 1.0d0 epsilon)))
      (%check (> (%surface-frame-dot cone-normal normal)
                 (- 1.0d0 epsilon)))
      (%check (<= (- (+ width epsilon)) displacement epsilon))
      (%check (<= radius-squared
                  (* (+ width epsilon) (+ width epsilon))))
      (%check (<= (abs (%surface-frame-dot center-offset u-axis))
                  (+ (* 0.5d0 +mesh-cell-size+) epsilon)))
      (%check (<= (abs (%surface-frame-dot center-offset v-axis))
                  (+ (* 0.5d0 +mesh-cell-size+) epsilon)))
      (%check (surface-attachment-frame-primitive-kinds frame))
      (values (sqrt radius-squared) point incident-normals))))

(defun %surface-attachment-test-offset-coordinates
    (coordinates axis amount)
  (let ((result (copy-list coordinates)))
    (incf (nth (axis-index axis) result) amount)
    result))

(defun %surface-attachment-test-site (domain coordinates)
  (make-site domain
             (first coordinates) (second coordinates) (third coordinates)
             +cell-extent+ 1))

(defun %surface-attachment-test-boundary (domain cell axis side)
  (ecase side
    (:low (site-boundary-low domain cell axis))
    (:high (site-boundary-high domain cell axis))))

(defun %test-surface-attachment-off-ray-shared-edge-ties ()
  (%with-test-section ("off-ray attachment crease cone under symmetry")
    (let* ((domain (make-world-domain :horizontal-bits 4))
           (cell (make-site domain 8 8 8 +cell-extent+ 1))
           (faces
             (loop for axis in '(:x :y :z)
                   append (list (site-boundary-low domain cell axis)
                                (site-boundary-high domain cell axis)))))
      (labels ((oriented-triangle (a b c normal)
                 (if (plusp
                      (%surface-frame-dot (%point-cross a b c) normal))
                     (list a b c)
                     (list a c b))))
        (dolist (face faces)
          (multiple-value-bind (center chart u-axis v-axis authored-normal)
              (%surface-attachment-test-chart face 0.0d0 0.0d0)
            (declare (ignore center chart))
            (dolist (reflection '(-1.0d0 1.0d0))
              (dolist (width '(1 2 3 4))
                (let* ((width (coerce width 'double-float))
                       (side-axis
                         (%surface-frame-scale reflection v-axis))
                       (a '(0.0d0 0.0d0 0.0d0))
                       (b (%surface-frame-scale width u-axis))
                       (c-flat (%surface-frame-scale width side-axis))
                       (c-fold
                         (%surface-frame+
                          c-flat
                          (%surface-frame-scale
                           (* 0.5d0 width) authored-normal)))
                       (flat
                         (oriented-triangle a b c-flat authored-normal))
                       (fold
                         (oriented-triangle a b c-fold authored-normal))
                       (query
                         (%surface-frame+
                          (%surface-frame-scale 0.5d0 b)
                          (%surface-frame-scale
                           (* -0.25d0 width) side-axis)))
                       (flat-normal
                         (apply #'%point-cross flat))
                       (fold-normal
                         (apply #'%point-cross fold))
                       (flat-denominator
                         (%surface-frame-dot flat-normal authored-normal))
                       (fold-denominator
                         (%surface-frame-dot fold-normal authored-normal))
                       (epsilon (* 1.0d-7 width))
                       (point-squared-epsilon (* epsilon epsilon))
                       (radius-squared-epsilon
                         (+ (* 2.0d0 width epsilon)
                            point-squared-epsilon)))
                  (%check
                   (not (%surface-frame-point-in-projected-triangle-p
                         query (first flat) (second flat) (third flat)
                         u-axis v-axis)))
                  (%check
                   (not (%surface-frame-point-in-projected-triangle-p
                         query (first fold) (second fold) (third fold)
                         u-axis v-axis)))
                  (multiple-value-bind (flat-point flat-radius-squared)
                      (%surface-frame-nearest-projected-triangle-point
                       query (first flat) (second flat) (third flat)
                       u-axis v-axis authored-normal flat-normal
                       flat-denominator)
                    (multiple-value-bind (fold-point fold-radius-squared)
                        (%surface-frame-nearest-projected-triangle-point
                         query (first fold) (second fold) (third fold)
                         u-axis v-axis authored-normal fold-normal
                         fold-denominator)
                      (%check
                       (<= (%surface-frame-point-distance-squared
                            flat-point fold-point)
                           point-squared-epsilon))
                      (%check (> flat-radius-squared 0.0d0))
                      (%check
                       (eq :tie
                           (%surface-frame-candidate-relation
                            fold-radius-squared 0.0d0 fold-point
                            flat-radius-squared 0.0d0 flat-point
                            epsilon radius-squared-epsilon
                            point-squared-epsilon)))
                      (let* ((flat-unit (%surface-frame-unit flat-normal))
                             (fold-unit (%surface-frame-unit fold-normal))
                             (cone
                               (%surface-frame-unit
                                (%surface-frame+ flat-unit fold-unit))))
                        (%check
                         (< (%surface-frame-dot flat-unit fold-unit)
                            (- 1.0d0 1.0d-5)))
                        (%check (> (%surface-frame-dot cone authored-normal)
                                   0.0d0))
                        (%check
                         (> (%surface-frame-dot cone flat-unit)
                            (%surface-frame-dot fold-unit flat-unit)))
                        (%check
                         (> (%surface-frame-dot cone fold-unit)
                            (%surface-frame-dot flat-unit fold-unit)))))))))))))))

(defun %test-surface-attachment-local-support-chart ()
  (%with-test-section ("local support-face attachment realization")
    (let ((domain (make-world-domain :horizontal-bits 5))
          (support-coordinates '(8 8 8))
          (epsilon 1.0d-5))
      ;; Every oriented face, both chart axes, and both reflections realize
      ;; the ordinary reentrant step which the old normal-only ray missed.
      (dolist (normal-axis '(:x :y :z))
        (dolist (side '(:low :high))
          (let* ((support
                   (%surface-attachment-test-site
                    domain support-coordinates))
                 (face
                   (%surface-attachment-test-boundary
                    domain support normal-axis side)))
            (multiple-value-bind
                  (center chart u-axis v-axis authored-normal)
                (%surface-attachment-test-chart face 0.0d0 0.0d0)
              (declare (ignore center chart))
              (multiple-value-bind (u-name v-name)
                  (face-tangent-axes face)
                (dolist (chart-axis '(:u :v))
                  (let ((tangent-name
                          (ecase chart-axis (:u u-name) (:v v-name)))
                        (tangent-vector
                          (ecase chart-axis (:u u-axis) (:v v-axis))))
                    (dolist (reflection '(-1 1))
                      (let* ((normal-step
                               (round
                                (nth (axis-index normal-axis)
                                     authored-normal)))
                             (tangent-step
                               (* reflection
                                  (round
                                   (nth (axis-index tangent-name)
                                        tangent-vector))))
                             (adjacent-coordinates
                               (%surface-attachment-test-offset-coordinates
                                support-coordinates tangent-name tangent-step))
                             (raised-coordinates
                               (%surface-attachment-test-offset-coordinates
                                adjacent-coordinates normal-axis normal-step))
                             (solid
                               (%chain-from-sites
                                domain
                                (list
                                 support
                                 (%surface-attachment-test-site
                                  domain adjacent-coordinates)
                                 (%surface-attachment-test-site
                                  domain raised-coordinates))))
                             (u
                               (if (eq chart-axis :u)
                                   (* 0.6d0 reflection)
                                   0.0d0))
                             (v
                               (if (eq chart-axis :v)
                                   (* 0.6d0 reflection)
                                   0.0d0)))
                        (dolist (width '(1 2 3 4))
                          (let* ((mesh
                                   (make-surface-mesh
                                    solid :bevel-width width))
                                 (frame
                                   (resolve-surface-attachment-frame
                                    mesh face :u u :v v)))
                            (multiple-value-bind
                                  (radius point incident-normals)
                                (%check-local-surface-attachment-frame
                                 mesh face u v width frame)
                              (declare (ignore point incident-normals))
                              (if (= width 1)
                                  (%check (< radius epsilon))
                                  (%check (> radius epsilon))))
                            (%check-surface-attachment-ray-preservation
                             frame mesh face u v)))))))))
            ;; A disconnected parallel wall two cells outward remains outside
            ;; the strict inward slab under every width and face orientation.
            (multiple-value-bind
                  (center chart u-axis v-axis authored-normal)
                (%surface-attachment-test-chart face 0.0d0 0.0d0)
              (declare (ignore chart u-axis v-axis))
              (let* ((normal-step
                       (round
                        (nth (axis-index normal-axis) authored-normal)))
                     (distant-coordinates
                       (%surface-attachment-test-offset-coordinates
                        support-coordinates normal-axis (* 2 normal-step)))
                     (solid
                       (%chain-from-sites
                        domain
                        (list support
                              (%surface-attachment-test-site
                               domain distant-coordinates)))))
                (dolist (width '(1 2 3 4))
                  (let* ((mesh
                           (make-surface-mesh solid :bevel-width width))
                         (frame
                           (resolve-surface-attachment-frame mesh face)))
                    (multiple-value-bind (radius point incident-normals)
                        (%check-local-surface-attachment-frame
                         mesh face 0.0d0 0.0d0 width frame)
                      (declare (ignore incident-normals))
                      (%check (< radius epsilon))
                      (%check
                       (< (sqrt
                           (%surface-frame-point-distance-squared point center))
                          epsilon)))
                    (%check-surface-attachment-ray-preservation
                     frame mesh face 0.0d0 0.0d0))))))))
      ;; Deterministic continuity sweep through the canonical former width-two
      ;; miss.  Every selected origin remains incident to the local closed
      ;; patch and adjacent chart samples cannot jump across another surface.
      (let* ((support-coordinates '(6 7 4))
             (support
               (%surface-attachment-test-site domain support-coordinates))
             (adjacent
               (%surface-attachment-test-site domain '(7 7 4)))
             (raised
               (%surface-attachment-test-site domain '(7 7 5)))
             (solid (%chain-from-sites domain (list support adjacent raised)))
             (face (site-boundary-high domain support :z))
             (mesh (make-surface-mesh solid :bevel-width 2))
             (previous-point nil)
             (last-radius nil))
        (dolist (u '(0.45d0 0.50d0 0.55d0 0.60d0
                     0.65d0 0.70d0 0.75d0 0.80d0))
          (let ((frame
                  (resolve-surface-attachment-frame
                   mesh face :u u :v 0.0d0)))
            (multiple-value-bind (radius point incident-normals)
                (%check-local-surface-attachment-frame
                 mesh face u 0.0d0 2 frame)
              (declare (ignore incident-normals))
              (when previous-point
                (%check
                 (< (sqrt
                     (%surface-frame-point-distance-squared
                      point previous-point))
                    0.5d0)))
              (setf previous-point point
                    last-radius radius))
            (%check-surface-attachment-ray-preservation
             frame mesh face u 0.0d0)))
        (%check (> last-radius epsilon))))))

(defun %vary-by-stock-mask-oracle
    (witness stock-masks site-widths contract-t-junctions-p)
  (flet ((width (x y z stocks)
           (declare (ignore x y z))
           (let ((site-mask 0))
             (dolist (stock stocks)
               (setf site-mask
                     (logior site-mask (aref stock-masks stock))))
             (aref site-widths site-mask))))
    (multiple-value-bind (generic generic-census generic-diagnostics)
        (funcall
         (if contract-t-junctions-p
             #'vary-surface-mesh-bevel-widths
             #'vary-uncontracted-surface-mesh-bevel-widths-diagnostic)
         witness #'width)
      (multiple-value-bind (compiled compiled-census compiled-diagnostics)
          (funcall
           (if contract-t-junctions-p
               #'vary-surface-mesh-bevel-widths-from-stock-masks
               #'vary-uncontracted-surface-mesh-bevel-widths-from-stock-masks-diagnostic)
           witness stock-masks site-widths)
        (values (%same-surface-mesh-representation-p generic compiled)
                (equalp generic-census compiled-census)
                (equal generic-diagnostics compiled-diagnostics))))))

(defun %map-mesh-triangles (function mesh)
  (let ((templates (surface-mesh-template-vertex-words mesh))
        (ranges (surface-mesh-template-ranges mesh)))
    (labels ((point (base vertex)
               (loop for axis below 3
                     collect (+ (* +mesh-cell-size+ (nth axis base))
                                (- (aref templates
                                         (+ (* vertex
                                               +mesh-template-vertex-word-count+)
                                            axis))
                                   +mesh-template-coordinate-bias+))))
             (visit (words kind)
               (loop for offset from 0 below (length words) by 4
                     for base = (list (aref words offset)
                                      (aref words (+ offset 1))
                                      (aref words (+ offset 2)))
                     for template-id = (ldb (byte 16 0)
                                            (aref words (+ offset 3)))
                     for start = (aref ranges (* 2 template-id))
                     for count = (aref ranges (1+ (* 2 template-id)))
                     do (loop for vertex from start below (+ start count) by 3
                              do (funcall function kind
                                          (point base vertex)
                                          (point base (1+ vertex))
                                          (point base (+ vertex 2)))))))
      (visit (surface-mesh-face-instance-words mesh) :face)
      (visit (surface-mesh-band-instance-words mesh) :band)
      (visit (surface-mesh-fan-instance-words mesh) :fan))))

(defun %ordered-edge (left right)
  (if (or (< (first left) (first right))
          (and (= (first left) (first right))
               (or (< (second left) (second right))
                   (and (= (second left) (second right))
                        (< (third left) (third right))))))
      (list left right)
      (list right left)))

(defun %mesh-geometric-edge-records (mesh)
  "Return undirected edge keys mapped to (incidence . orientation balance)."
  (let ((records (make-hash-table :test #'equal)))
    (%map-mesh-triangles
     (lambda (kind a b c)
       (declare (ignore kind))
       (let ((points (vector a b c)))
         (dotimes (index 3)
           (let* ((left (aref points index))
                  (right (aref points (mod (1+ index) 3)))
                  (edge (%ordered-edge left right))
                  (record (or (gethash edge records)
                              (setf (gethash edge records) (cons 0 0)))))
             (incf (car record))
             (incf (cdr record) (if (equal left (first edge)) 1 -1))))))
     mesh)
    records))

(defun %meshes-closed-p (meshes)
  "Return true when MESHES form one closed, consistently oriented cohort."
  (let ((records (make-hash-table :test #'equal)))
    (dolist (mesh meshes)
      (maphash
       (lambda (edge record)
         (let ((combined (or (gethash edge records)
                             (setf (gethash edge records) (cons 0 0)))))
           (incf (car combined) (car record))
           (incf (cdr combined) (cdr record))))
       (%mesh-geometric-edge-records mesh)))
    (loop for record being the hash-values of records
          always (and (= (car record) 2) (zerop (cdr record))))))

(defun %mesh-closed-p (mesh)
  (%meshes-closed-p (list mesh)))

(defun %mesh-nondegenerate-p (mesh)
  (let ((nondegenerate-p t))
    (%map-mesh-triangles
     (lambda (kind a b c)
       (declare (ignore kind))
       (when (every #'zerop
                    (%cross (%point- b a) (%point- c a)))
         (setf nondegenerate-p nil)))
     mesh)
    nondegenerate-p))

(defun %mesh-unique-points (mesh)
  (let ((points (make-hash-table :test #'equal)))
    (%map-mesh-triangles
     (lambda (kind a b c)
       (declare (ignore kind))
       (dolist (point (list a b c))
         (setf (gethash point points) t)))
     mesh)
    (loop for point being the hash-keys of points collect point)))

(defun %mesh-oriented-plane-areas (mesh)
  "Exact per-plane doubled-area signature, including render attributes."
  (let ((areas (make-hash-table :test #'equal)))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (declare (ignore mask))
       (let* ((cross (%point-cross a b c))
              (key (list kind stock ambient normal (%point-dot normal a)))
              (area (/ (%point-dot cross normal)
                       (%point-dot normal normal))))
         (incf (gethash key areas 0) area)))
     mesh)
    areas))

(defun %same-plane-areas-p (left right)
  (and (= (hash-table-count left) (hash-table-count right))
       (loop for key being the hash-keys of left using (hash-value area)
             always (= area (gethash key right -1)))))

(defun %stream-template-coordinates-within-p (mesh instance-words low high)
  (let ((templates (surface-mesh-template-vertex-words mesh))
        (ranges (surface-mesh-template-ranges mesh)))
    (loop for offset from 0 below (length instance-words) by 4
          for template-id = (ldb (byte 16 0)
                                 (aref instance-words (+ offset 3)))
          for start = (aref ranges (* 2 template-id))
          for count = (aref ranges (1+ (* 2 template-id)))
          always
          (loop for vertex from start below (+ start count)
                always
                (loop for axis below 3
                      for coordinate =
                        (- (aref templates
                                 (+ (* vertex
                                       +mesh-template-vertex-word-count+)
                                    axis))
                           +mesh-template-coordinate-bias+)
                      always (<= low coordinate high))))))

(defun %fan-templates-are-triangles-p (mesh)
  (let ((ranges (surface-mesh-template-ranges mesh))
        (instances (surface-mesh-fan-instance-words mesh)))
    (loop for offset from 0 below (length instances) by 4
          for template-id = (ldb (byte 16 0) (aref instances (+ offset 3)))
          for count = (aref ranges (1+ (* 2 template-id)))
          always (= 3 count))))

(defun %fan-site-used-as-vertex-p (mesh site)
  (let ((templates (surface-mesh-template-vertex-words mesh))
        (ranges (surface-mesh-template-ranges mesh))
        (instances (surface-mesh-fan-instance-words mesh)))
    (loop for offset from 0 below (length instances) by 4
          thereis
          (and (loop for axis below 3
                     always (= (nth axis site)
                               (aref instances (+ offset axis))))
               (let* ((template-id
                        (ldb (byte 16 0) (aref instances (+ offset 3))))
                      (start (aref ranges (* 2 template-id)))
                      (count (aref ranges (1+ (* 2 template-id)))))
                 (loop for vertex from start below (+ start count)
                       thereis
                       (loop for axis below 3
                             always
                             (= +mesh-template-coordinate-bias+
                                (aref templates
                                      (+ (* vertex
                                            +mesh-template-vertex-word-count+)
                                         axis))))))))))

(defun %every-fan-triangle-at-site-uses-site-p (mesh site)
  (let ((templates (surface-mesh-template-vertex-words mesh))
        (ranges (surface-mesh-template-ranges mesh))
        (instances (surface-mesh-fan-instance-words mesh))
        (found nil))
    (loop for offset from 0 below (length instances) by 4
          do (when (loop for axis below 3
                         always (= (nth axis site)
                                   (aref instances (+ offset axis))))
               (setf found t)
               (let* ((template-id
                        (ldb (byte 16 0) (aref instances (+ offset 3))))
                      (start (aref ranges (* 2 template-id)))
                      (count (aref ranges (1+ (* 2 template-id)))))
                 (unless
                     (loop for vertex from start below (+ start count)
                           thereis
                           (loop for axis below 3
                                 always
                                 (= +mesh-template-coordinate-bias+
                                    (aref templates
                                          (+ (* vertex
                                                +mesh-template-vertex-word-count+)
                                             axis)))))
                   (return nil))))
          finally (return found))))

(defun %test-surface-mesh ()
  (%with-test-section ("integer site streams")
    ;; This strength reduction must preserve Common Lisp ROUND exactly,
    ;; including negative values and ties to even.
    (loop for sum from -128 to 128
          do (%check (= (%nearest-edge-site-coordinate sum 0)
                        (round sum (* 2 +mesh-cell-size+)))))
    ;; Packed quad-grain observation must retain the hash oracle's directed
    ;; open perimeter while omitting construction diagonals and reducing the
    ;; shared perimeter of adjacent quads.
    (labels ((hash-open-records (table)
               (sort
                (loop for key being the hash-keys of table
                        using (hash-value value)
                      when (= 1 (ash value
                                     (- +boundary-observation-count-shift+)))
                        collect (cons key value))
                #'< :key #'car))
             (packed-open-records (records)
               (loop for observation across
                     (%reduce-packed-boundary-observations records)
                     for key = (%boundary-edge-observation-key observation)
                     collect
                     (cons
                      key
                      (logior
                       (ash 1 +boundary-observation-count-shift+)
                       (ash (%boundary-edge-observation-left observation)
                            +fan-record-left-shift+)
                       (ash (%boundary-edge-observation-right observation)
                            +fan-record-right-shift+)
                       (%boundary-edge-observation-stock observation)))))
             (check-quad (normal-z)
               (let* ((domain (make-world-domain :horizontal-bits 5))
                      (hash-builder (%make-surface-mesh-builder domain 2))
                      (packed-builder (%make-surface-mesh-builder domain 2))
                      (packing (%make-spatial-edge-packing-for-box 0 2 0 1))
                      (replayed (make-hash-table :test #'eql))
                      (p0 #(0 0 8)) (p1 #(8 0 8))
                      (p2 #(8 8 8)) (p3 #(0 8 8))
                      (p4 #(16 0 8)) (p5 #(16 8 8)))
                 (let ((*boundary-observation-strategy* :hash))
                   (%enable-boundary-observations
                    (list hash-builder) packing 2))
                 (let ((*boundary-observation-strategy* :packed))
                   (%enable-boundary-observations
                    (list packed-builder) packing 2))
                 (flet ((emit (builder)
                          (%emit-quad builder :face 0 0 1 p0 p1 p2 p3
                                      0 0 normal-z 7 0)
                          (%emit-quad builder :face 1 0 1 p1 p4 p5 p2
                                      0 0 normal-z 9 0)))
                   (emit hash-builder)
                   (emit packed-builder))
                 (%scan-stream-boundary-edges
                  (surface-mesh-builder-face-stream hash-builder)
                  (surface-mesh-builder-templates hash-builder)
                  packing replayed)
                 (let* ((direct
                          (surface-mesh-builder-boundary-observations
                           hash-builder))
                        (packed
                          (surface-mesh-builder-boundary-edge-records
                           packed-builder))
                        (expected (hash-open-records replayed)))
                   (%check (= 7 (hash-table-count direct)))
                   (%check (= 9 (hash-table-count replayed)))
                   (%check (= 6 (length
                                 (%reduce-packed-boundary-observations
                                  packed))))
                   (%check (equal (hash-open-records direct) expected))
                   (%check (equal (packed-open-records packed) expected))))))
      (check-quad 1)
      (with-surface-mesh-workspace ()
        (check-quad -1)))
    (%check (equal '(0 -1 0) (%normal-direction-code '(0 -2 0))))
    (dolist (bevel-width '(1 2 3))
      (flet ((mesh-for-star (mask)
               (make-surface-mesh (%solid-for-star mask)
                                  :bevel-width bevel-width)))
        (let ((one (mesh-for-star #x01)))
          (%check (= bevel-width (surface-mesh-bevel-width one)))
          (%check (= 6 (surface-mesh-face-instance-count one)))
          (%check (= 12 (surface-mesh-band-instance-count one)))
          (%check (= 8 (surface-mesh-fan-instance-count one)))
          (%check (= 12 (surface-mesh-face-triangle-count one)))
          (%check (= 24 (surface-mesh-band-triangle-count one)))
          (%check (= 8 (surface-mesh-fan-triangle-count one)))
          (%check (= 44 (surface-mesh-triangle-count one)))
          (%check (%stream-template-coordinates-within-p
                   one (surface-mesh-fan-instance-words one)
                   (- bevel-width) bevel-width))
          (%check (%fan-templates-are-triangles-p one))
          (%check (%mesh-closed-p one)))
        (let ((pair (mesh-for-star #x03)))
          (%check (zerop (surface-mesh-singular-star-count pair)))
          (%check (%mesh-closed-p pair))
          (%check (plusp (surface-mesh-band-instance-count pair)))
          (%check (plusp (surface-mesh-fan-instance-count pair)))
          (%check (%stream-template-coordinates-within-p
                   pair (surface-mesh-fan-instance-words pair)
                   (- bevel-width) bevel-width))
          (%check (%fan-templates-are-triangles-p pair)))
        (let ((convex-trapezoid (mesh-for-star #x70))
              (concave-corner (mesh-for-star #x8f))
              (concave-run (mesh-for-star #xcf)))
          (%check (%fan-site-used-as-vertex-p
                   convex-trapezoid '(8 8 8)))
          (%check (%every-fan-triangle-at-site-uses-site-p
                   convex-trapezoid '(8 8 8)))
          (%check (%fan-site-used-as-vertex-p concave-corner '(8 8 8)))
          (%check (not (%fan-site-used-as-vertex-p concave-run '(8 8 8)))))
        (dolist (mask '(#x06 #x18 #x69))
          (let ((mesh (mesh-for-star mask)))
            (%check (plusp (surface-mesh-singular-star-count mesh))
                    (format nil "width ~D mask ~2,'0X" bevel-width mask))
            (%check (%mesh-closed-p mesh)
                    (format nil "width ~D mask ~2,'0X"
                            bevel-width mask))))))
    (let* ((medial
             (make-surface-mesh (%solid-for-star #x01) :bevel-width 4))
           (points (%mesh-unique-points medial)))
      (%check (= 4 (surface-mesh-bevel-width medial)))
      (%check (zerop (surface-mesh-face-instance-count medial)))
      (%check (zerop (surface-mesh-band-instance-count medial)))
      (%check (= 8 (surface-mesh-fan-instance-count medial)))
      (%check (= 8 (surface-mesh-triangle-count medial)))
      (%check (= 6 (length points)))
      (%check
       (every (lambda (point)
                (= 2 (count 4 point :key (lambda (coordinate)
                                          (mod coordinate 8)))))
              points))
      (%check (%mesh-closed-p medial))
      (%check (%mesh-nondegenerate-p medial)))
    (dotimes (mask 256)
      (let* ((mesh
               (make-surface-mesh (%solid-for-star mask) :bevel-width 4))
             (merged (%coplanar-merged-surface-mesh mesh)))
        (%check (%mesh-closed-p mesh)
                (format nil "medial mask ~2,'0X" mask))
        (%check (%mesh-nondegenerate-p mesh)
                (format nil "medial mask ~2,'0X" mask))
        (%check (%mesh-closed-p merged)
                (format nil "merged medial mask ~2,'0X" mask))
        (%check (%mesh-nondegenerate-p merged)
                (format nil "merged medial mask ~2,'0X" mask))
        (%check (<= (surface-mesh-triangle-count merged)
                    (surface-mesh-triangle-count mesh))
                (format nil "merged count mask ~2,'0X" mask))
        (%check (%same-plane-areas-p
                 (%mesh-oriented-plane-areas mesh)
                 (%mesh-oriented-plane-areas merged))
                (format nil "merged plane areas mask ~2,'0X" mask))))
    (dolist (width '(1 2 3 4))
      (dolist (mask '(#x01 #x70 #x8f #x69))
        (let* ((solid (%solid-for-star mask))
               (witness (make-surface-mesh solid :bevel-width 1))
               (oracle (make-surface-mesh solid :bevel-width width))
               (varied
                 (vary-surface-mesh-bevel-widths
                  witness
                  (lambda (x y z stocks)
                    (declare (ignore x y z stocks))
                    width))))
          (%check (%mesh-closed-p varied))
          (%check (%mesh-nondegenerate-p varied))
          (%check (%same-plane-areas-p
                   (%mesh-oriented-plane-areas oracle)
                   (%mesh-oriented-plane-areas varied))
                  (format nil "uniform affine width ~D mask ~2,'0X"
                          width mask)))))
    (dotimes (mask 256)
      (let* ((witness
               (make-surface-mesh (%solid-for-star mask) :bevel-width 1))
             (varied
               (vary-surface-mesh-bevel-widths
                witness
                (lambda (x y z stocks)
                  (declare (ignore stocks))
                  (if (oddp (+ x y z)) 4 1)))))
        (%check (%mesh-closed-p varied)
                (format nil "mixed affine closure mask ~2,'0X" mask))
        (%check (%mesh-nondegenerate-p varied)
                (format nil "mixed affine triangles mask ~2,'0X" mask))))
    ;; The parity policy above happens not to collapse any three-distinct-point
    ;; triangle.  This asymmetric field makes all four widths meet and forces
    ;; the contraction path in every nonempty occupancy star.
    (dotimes (mask 256)
      (let ((witness
              (make-surface-mesh (%solid-for-star mask) :bevel-width 1)))
        (multiple-value-bind (varied census diagnostics)
            (vary-surface-mesh-bevel-widths
             witness #'%asymmetric-site-bevel-width)
          (%check (%mesh-closed-p varied)
                  (format nil "mixed repair closure mask ~2,'0X" mask))
          (%check (%mesh-nondegenerate-p varied)
                  (format nil "mixed repair triangles mask ~2,'0X" mask))
          (%check (zerop (getf diagnostics :residual-edge-count))
                  (format nil "mixed repair residual mask ~2,'0X" mask))
          (unless (zerop mask)
            (%check (plusp (getf diagnostics :repaired-edge-count))
                    (format nil "mixed repair exercised mask ~2,'0X" mask)))
          (when (= mask #x01)
            (%check (equalp #(0 2 2 2 2) census))
            (%check (= 4 (getf diagnostics :collapsed-triangle-count)))
            (%check (= 12 (getf diagnostics :unmatched-edge-count)))
            (%check (= 4 (getf diagnostics :repaired-edge-count)))
            (%check (= 4 (length (getf diagnostics :candidate-splits))))
            (%check (= 44 (surface-mesh-triangle-count varied)))))))
    ;; A three-distinct-point collapse is only a geometric split candidate.
    ;; Its synthetic queried edges may all be absent from the realized mesh;
    ;; those zero counts are not unmatched boundary edges.
    (let ((witness
            (make-surface-mesh (%solid-for-star #x01) :bevel-width 1)))
      (multiple-value-bind (varied census diagnostics)
          (vary-uncontracted-surface-mesh-bevel-widths-diagnostic
           witness
           (lambda (x y z stocks)
             (declare (ignore stocks))
             (1+ (mod (+ x y z) 4))))
        (%check (equalp #(0 1 1 3 3) census))
        (%check (= 6 (getf diagnostics :collapsed-triangle-count)))
        (%check (= 3 (length (getf diagnostics :candidate-splits))))
        (%check (zerop (getf diagnostics :unmatched-edge-count)))
        (%check (zerop (getf diagnostics :repaired-edge-count)))
        (%check (zerop (getf diagnostics :residual-edge-count)))
        (%check (= 38 (surface-mesh-triangle-count varied)))
        (%check (%mesh-closed-p varied))
        (%check (%mesh-nondegenerate-p varied))))
    ;; The semantic callback is deliberately outside the dense loop: each
    ;; canonical site is presented exactly once with its complete, sorted,
    ;; duplicate-free set of incident stocks.
    (let* ((witness
             (make-surface-mesh (%solid-for-star #x69) :bevel-width 1
                                :stock-function
                                (lambda (face)
                                  (mod (+ (site-x face)
                                          (* 2 (site-y face))
                                          (* 3 (site-z face)))
                                       7))))
           (expected (%witness-site-stock-table witness))
           (seen (make-hash-table :test #'eql)))
      (vary-surface-mesh-bevel-widths
       witness
       (lambda (x y z stocks)
         (let ((site (%lattice-key x y z)))
           (%check (not (gethash site seen)))
           (%check (equal stocks (gethash site expected)))
           (setf (gethash site seen) t)
           (%asymmetric-site-bevel-width x y z stocks))))
      (%check (= (hash-table-count expected) (hash-table-count seen))))
    ;; The production stock-mask fold is only a denser policy compiler.  Its
    ;; complete retained representation must match the generic callback that
    ;; computes the same commutative, idempotent OR at every site.
    (let ((stock-masks
            (make-array 8 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 4 1 2 4 3 5)))
          (site-widths
            (make-array 8 :element-type '(unsigned-byte 8)
                          :initial-contents '(0 1 2 3 4 1 2 4))))
      (dotimes (mask 256)
        (let* ((solid (%solid-for-star mask))
               (witness
                 (make-surface-mesh
                  solid :bevel-width 1
                  :stock-function
                  (lambda (face)
                    (mod (+ (site-x face) (* 2 (site-y face))
                            (* 3 (site-z face)) (site-extent face))
                         8))
                  :chamfer-stock-function
                  (lambda (stocks) (mod (reduce #'+ stocks) 8)))))
          (multiple-value-bind (same-mesh same-census same-diagnostics)
              (%vary-by-stock-mask-oracle
               witness stock-masks site-widths t)
            (%check same-mesh
                    (format nil "compiled policy mesh mask ~2,'0X" mask))
            (%check same-census
                    (format nil "compiled policy census mask ~2,'0X" mask))
            (%check same-diagnostics
                    (format nil "compiled policy diagnostics mask ~2,'0X"
                            mask)))))
      (let ((witness
              (make-surface-mesh (%solid-for-star #x01) :bevel-width 1)))
        ;; Keep the odd interior width explicit in the packed production-policy
        ;; equality oracle; width three is neither the narrow witness nor the
        ;; medial-collapse endpoint exercised by the diagnostic case below.
        (multiple-value-bind (same-mesh same-census same-diagnostics)
            (%vary-by-stock-mask-oracle
             witness #(1) #(0 3) t)
          (%check same-mesh "compiled width-three policy mesh")
          (%check same-census "compiled width-three policy census")
          (%check same-diagnostics "compiled width-three policy diagnostics"))
        (multiple-value-bind (same-mesh same-census same-diagnostics)
            (%vary-by-stock-mask-oracle
             witness #(1) #(0 4) nil)
          (%check same-mesh)
          (%check same-census)
          (%check same-diagnostics))))
    ;; A sparse pair whose tight bounding volume is much larger than its site
    ;; count exercises the compiled field's EQL fallback rather than the dense
    ;; page result used by the exhaustive corpus above.
    (let* ((domain (make-world-domain :horizontal-bits 7))
           (solid
             (%chain-from-sites
              domain
              (list (make-site domain 1 1 1 +cell-extent+ 1)
                    (make-site domain 65 65 1 +cell-extent+ 1))))
           (witness (make-surface-mesh solid :bevel-width 1)))
      (multiple-value-bind (varied census diagnostics)
          (vary-surface-mesh-bevel-widths-from-stock-masks
           witness
           (make-array 1 :element-type '(unsigned-byte 8)
                         :initial-contents '(1))
           (make-array 2 :element-type '(unsigned-byte 8)
                         :initial-contents '(0 2)))
        (%check (equalp #(0 0 16 0 0) census))
        (%check (%mesh-closed-p varied))
        (%check (%mesh-nondegenerate-p varied))
        (%check (zerop (getf diagnostics :residual-edge-count)))))
    ;; Packed global points and bounded spatial edge keys must work at both
    ;; horizontal extremes and on the highest legal cell layer.  The two
    ;; asymmetric far-edge cases catch accidental X/Y field interchange.
    (let* ((domain (make-world-domain :horizontal-bits 17))
           (limit (world-domain-x-limit domain)))
      (dolist (cell `((0 0 0)
                      (,(1- limit) 13 254)
                      (13 ,(1- limit) 254)
                      (,(1- limit) ,(1- limit) 254)))
        (destructuring-bind (x y z) cell
          (let* ((solid
                   (%chain-from-sites
                    domain (list (make-site domain x y z +cell-extent+ 1))))
                 (witness (make-surface-mesh solid :bevel-width 1)))
            (multiple-value-bind (varied census diagnostics)
                (vary-surface-mesh-bevel-widths
                 witness #'%asymmetric-site-bevel-width)
              (declare (ignore census))
              (%check (%mesh-closed-p varied)
                      (format nil "mixed domain boundary ~S" cell))
              (%check (%mesh-nondegenerate-p varied)
                      (format nil "mixed domain triangles ~S" cell))
              (%check (zerop (getf diagnostics :residual-edge-count))
                      (format nil "mixed domain residual ~S" cell)))))))
    (dolist (width '(0 5 1/2))
      (%check (%signals-error-p
               (lambda ()
                 (make-surface-mesh (%solid-for-star #x01)
                                    :bevel-width width))))
      (%check (%signals-error-p
               (lambda ()
                 (vary-surface-mesh-bevel-widths
                  (make-surface-mesh (%solid-for-star #x01) :bevel-width 1)
                  (lambda (x y z stocks)
                    (declare (ignore x y z stocks))
                    width))))))
    (%check (%signals-error-p
             (lambda ()
               (vary-surface-mesh-bevel-widths
                (make-surface-mesh (%solid-for-star #x01) :bevel-width 2)
                #'%asymmetric-site-bevel-width))))
    (let ((witness
            (make-surface-mesh (%solid-for-star #x01) :bevel-width 1)))
      (%check (%signals-error-p
               (lambda ()
                 (vary-surface-mesh-bevel-widths-from-stock-masks
                  witness #() #(0 1)))))
      (%check (%signals-error-p
               (lambda ()
                 (vary-surface-mesh-bevel-widths-from-stock-masks
                  witness #(0) #(0 1)))))
      (%check (%signals-error-p
               (lambda ()
                 (vary-surface-mesh-bevel-widths-from-stock-masks
                  witness #(1) #(0 5))))))))

(defun %chunk-test-world ()
  "A four-chunk world with solids straddling every seam and the world box."
  (let* ((domain (make-world-domain :horizontal-bits 7))
         (builder (make-chain-builder domain :initial-capacity 16384)))
    (flet ((patch (x0 x1 y0 y1)
             (loop for x from x0 below x1 do
               (loop for y from y0 below y1 do
                 (let ((height
                         (max 1 (+ 4 (floor (* 2.5 (+ (sin (* x 0.37))
                                                      (cos (* y 0.29)))))))))
                   (dotimes (z height)
                     (chain-builder-add-site
                      builder
                      (make-site domain x y z +cell-extent+ 1))))))))
      ;; A cross over both interior seams, and both world-box corners.
      (patch 48 80 48 80)
      (patch 0 8 0 8)
      (patch 120 128 120 128))
    (finish-chain-builder builder)))

(defun %canonical-triangle-counts (meshes)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (mesh meshes table)
      (%map-mesh-triangles
       (lambda (kind a b c)
         (declare (ignore kind))
         (let* ((rotations
                  (list (append a b c) (append b c a) (append c a b)))
                (best (first rotations)))
           (dolist (rotation (rest rotations))
             (when (loop for l in rotation for r in best
                         when (/= l r) return (< l r)
                         finally (return nil))
               (setf best rotation)))
           (incf (gethash best table 0))))
       mesh))))

(defun %canonical-triangle-record-counts (meshes)
  "Count oriented triangles including every retained non-normal attribute."
  (labels ((number-list< (left right)
             (loop for l in left for r in right
                   when (/= l r) return (< l r)
                   finally (return nil)))
           (rotate-mask (mask)
             ;; Boundary bits name the edge opposite A, B, and C.  Rotating
             ;; (A B C) to (B C A) rotates those three bits in the same order.
             (logior (if (logbitp 1 mask) #b001 0)
                     (if (logbitp 2 mask) #b010 0)
                     (if (logbitp 0 mask) #b100 0)))
           (canonical-record (kind stock ambient mask a b c)
             (let* ((mask-1 (rotate-mask mask))
                    (mask-2 (rotate-mask mask-1))
                    (rotations
                      (list (list mask a b c)
                            (list mask-1 b c a)
                            (list mask-2 c a b)))
                    (best (first rotations)))
               (dolist (rotation (rest rotations))
                 (when (number-list<
                        (append (second rotation) (third rotation)
                                (fourth rotation) (list (first rotation)))
                        (append (second best) (third best)
                                (fourth best) (list (first best))))
                   (setf best rotation)))
               (list kind stock ambient best))))
    (let ((table (make-hash-table :test #'equal)))
      (dolist (mesh meshes table)
        (%map-surface-mesh-triangle-records
         (lambda (kind stock ambient mask normal a b c)
           (declare (ignore normal))
           (incf (gethash
                  (canonical-record kind stock ambient mask a b c)
                  table 0)))
         mesh)))))

(defun %triangle-counts= (left right)
  (and (= (hash-table-count left) (hash-table-count right))
       (loop for key being the hash-keys of left using (hash-value count)
             always (= count (gethash key right 0)))))

(defun %width-one-test-face-stock (cell axis side)
  "Four singleton stocks whose LOGIOR is the test material-summary algebra."
  (svref #(1 2 4 8)
         (mod (+ (site-x cell) (* 3 (site-y cell)) (* 5 (site-z cell))
                 (axis-index axis) (if (eq side :forward) 1 0))
              4)))

(defun %width-one-test-chamfer-stock (stocks)
  (reduce #'logior stocks))

(defun %make-width-one-test-chamfer-algebra ()
  (let ((masks (make-array 9 :element-type '(unsigned-byte 16)
                             :initial-element #xffff))
        (stocks (make-array 16 :element-type '(unsigned-byte 16))))
    (setf (aref masks 1) 1
          (aref masks 2) 2
          (aref masks 4) 4
          (aref masks 8) 8)
    (dotimes (mask 16)
      (setf (aref stocks mask) mask))
    (make-compiled-chamfer-algebra masks stocks 15)))

(defun %mesh-test-chunk-with (function chain key store arguments)
  (handler-bind
      ((missing-chunk
         (lambda (condition)
           (multiple-value-bind (neighbor present-p)
               (gethash (missing-chunk-key condition) store)
             (if present-p
                 (invoke-restart 'use-chunk neighbor)
                 (invoke-restart 'treat-as-air)))))
       (outside-domain
         (lambda (condition)
           (declare (ignore condition))
           (invoke-restart 'treat-as-air))))
    (apply function chain key arguments)))

(defun %mesh-test-chunk (chain key store &rest arguments)
  (%mesh-test-chunk-with #'mesh-chunk chain key store arguments))

(defun %mesh-test-chunk-reference (chain key store &rest arguments)
  (%mesh-test-chunk-with #'%mesh-chunk-reference chain key store arguments))

(defun %test-width-one-z-fibers ()
  (%with-test-section ("width-one padded Z fibers and SIMD classification")
    ;; The native kernel is an optimization of this scalar word algebra.  Use
    ;; deterministic full-width values so every lane and every input position
    ;; participates without making the test host-random.
    (let ((low (make-array 16 :element-type '(unsigned-byte 64)))
          (high (make-array 16 :element-type '(unsigned-byte 64)))
          (scalar (make-array +occupancy-fiber-word-count+
                              :element-type '(unsigned-byte 64)))
          (native (make-array +occupancy-fiber-word-count+
                              :element-type '(unsigned-byte 64)))
          (state #x6a09e667f3bcc909)
          (kernel (%width-one-active-word-kernel)))
      (labels ((next-word ()
                 (setf state
                       (ldb (byte 64 0)
                            (+ (* state #x5851f42d4c957f2d)
                               #x14057b7ef767814f)))))
        (dotimes (trial 64)
          (dotimes (index 16)
            (setf (aref low index) (next-word)
                  (aref high index) (next-word)))
          (%width-one-active-words-scalar
           low high 0 4 8 12 scalar)
          (funcall kernel low high 0 4 8 12 native)
          (%check (equalp scalar native)
                  (format nil "SIMD trial ~D" trial)))))
    ;; Exercise the sentinel/carry boundaries by comparing every reconstructed
    ;; star at selected XY positions against the original scalar occupancy
    ;; oracle.  The authored cells straddle all four u64 words and both legal
    ;; ends of the 0..254 cell interval.
    (let* ((domain (make-world-domain :horizontal-bits 7))
           (z-values '(0 1 62 63 64 65 126 127 128 129
                       190 191 192 193 253 254))
           (solid
             (%chain-from-sites
              domain
              (loop for z in z-values
                    append
                    (list (make-site domain 0 0 z +cell-extent+ 1)
                          (make-site domain 1 63 z +cell-extent+ 1)
                          (make-site domain 63 1 z +cell-extent+ 1)))))
           (field (%materialize-occupancy solid 0 64 0 64)))
      (handler-bind
          ((missing-chunk
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'treat-as-air)))
           (outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'treat-as-air))))
        (let* ((window
                 (%materialize-width-one-star-window
                  field domain 0 64 0 64))
               (air-window
                 (%materialize-width-one-star-window
                  field domain 0 64 0 64 :outside-domain-policy :air))
               (low (width-one-star-window-low-words window))
               (high (width-one-star-window-high-words window))
               (window-x0 (width-one-star-window-x0 window))
               (window-y0 (width-one-star-window-y0 window))
               (y-span (width-one-star-window-y-span window)))
          (%check
           (equalp high (width-one-star-window-high-words air-window))
           "constant AIR high fibers")
          (%check
           (equalp low (width-one-star-window-low-words air-window))
           "constant AIR shifted fibers")
          (dolist (x '(0 1 2 63 64))
            (dolist (y '(0 1 2 63 64))
              (let ((low-low
                      (%width-one-window-fiber-base
                       (1- x) (1- y) window-x0 window-y0 y-span))
                    (high-low
                      (%width-one-window-fiber-base
                       x (1- y) window-x0 window-y0 y-span))
                    (low-high
                      (%width-one-window-fiber-base
                       (1- x) y window-x0 window-y0 y-span))
                    (high-high
                      (%width-one-window-fiber-base
                       x y window-x0 window-y0 y-span)))
                (dotimes (z (1+ +top-z+))
                  (%check
                   (= (%star-mask-at field domain x y z)
                      (%width-one-star-mask-from-fibers
                       low high low-low high-low low-high high-high
                       (ash z -6) (logand z 63)))
                   (format nil "star (~D ~D ~D)" x y z)))))))))))

(defun %test-width-one-query-dimension ()
  (%with-test-section ("width-one query-native topology dimension")
    (let* ((vocabulary *width-one-query-vocabulary*)
           (dimension
             (width-one-query-vocabulary-dimension vocabulary))
           (contributor-counts
             (width-one-query-dimension-contributor-counts dimension))
           (face-starts (width-one-query-dimension-face-starts dimension))
           (face-templates
             (width-one-query-dimension-face-templates dimension))
           (face-material-slots
             (width-one-query-dimension-face-material-slots dimension))
           (face-source-offsets
             (width-one-query-dimension-face-source-offsets dimension))
           (band-starts (width-one-query-dimension-band-starts dimension))
           (band-templates
             (width-one-query-dimension-band-templates dimension))
           (band-contributor-masks
             (width-one-query-dimension-band-contributor-masks dimension))
           (band-source-witnesses
             (width-one-query-dimension-band-source-witnesses dimension))
           (band-ambient-stars
             (width-one-query-dimension-band-ambient-stars dimension))
           (fan-starts (width-one-query-dimension-fan-starts dimension))
           (fan-templates
             (width-one-query-dimension-fan-templates dimension))
           (fan-contributor-masks
             (width-one-query-dimension-fan-contributor-masks dimension))
           (ranges (width-one-query-vocabulary-ranges vocabulary))
           (words
             (aref
              (width-one-query-vocabulary-vertex-words-by-width vocabulary) 1))
           (face-vertices (make-array 6)))
      (dotimes (axis-number 3)
        (dotimes (source-bit 2)
          (setf (aref face-vertices (+ (* 2 axis-number) source-bit))
                (%width-one-query-face-vertices axis-number source-bit))))
      (labels ((template-vertices (template)
                 (let* ((start (aref ranges (* 2 template)))
                        (count (aref ranges (1+ (* 2 template))))
                        (vertices (make-array count :element-type 'fixnum)))
                   (dotimes (index count vertices)
                     (let ((word
                             (* +mesh-template-vertex-word-count+
                                (+ start index))))
                       (setf (aref vertices index)
                             (%pack-template-vertex
                              (- (aref words word)
                                 +mesh-template-coordinate-bias+)
                              (- (aref words (+ word 1))
                                 +mesh-template-coordinate-bias+)
                              (- (aref words (+ word 2))
                                 +mesh-template-coordinate-bias+)
                              (aref words (+ word 3))))))))
               (compact-contributors (pattern cube-mask)
                 (let ((compact 0))
                   (dotimes (index 12 compact)
                     (when (logbitp index cube-mask)
                       (setf compact
                             (logior
                              compact
                              (ash 1 (%width-one-contributor-slot
                                      pattern index))))))))
               (edge-cube-contributors
                   (transition-mask contributor-indices)
                 (let ((cube-mask 0))
                   (dotimes (transition 4 cube-mask)
                     (when (logbitp transition transition-mask)
                       (setf cube-mask
                             (logior cube-mask
                                     (ash 1
                                          (aref contributor-indices
                                                transition))))))))
               (source-offset (pattern contributor-index)
                 (let* ((contributor
                          (aref
                           (width-one-vertex-pattern-contributors pattern)
                           contributor-index))
                        (sample (ldb (byte 3 0) contributor)))
                   (logior (if (logbitp 0 sample) 0 1)
                           (if (logbitp 1 sample) 0 2))))
               (source-witness
                   (pattern contributor-indices transition-mask)
                 (let ((witness 0))
                   (dotimes (transition 4 witness)
                     (when (logbitp transition transition-mask)
                       (setf witness
                             (logior
                              witness
                              (ash 1
                                   (source-offset
                                    pattern
                                    (aref contributor-indices
                                          transition))))))))))
        ;; Recompute every row through the retained descriptor tables.  This
        ;; pins the compiled dimension's order and query-native translations
        ;; independently of the production planner which now only copies it.
        (dotimes (mask 256)
          (let* ((pattern (svref *width-one-vertex-pattern-table* mask))
                 (face-row (aref face-starts mask))
                 (face-end (aref face-starts (1+ mask)))
                 (band-row (aref band-starts mask))
                 (band-end (aref band-starts (1+ mask)))
                 (fan-row (aref fan-starts mask))
                 (fan-end (aref fan-starts (1+ mask))))
            (%check
             (= (aref contributor-counts mask)
                (width-one-vertex-pattern-contributor-count pattern))
             (format nil "contributor count ~2,'0X" mask))
            (dotimes (axis-number 3)
              (let* ((u (svref +axis-u+ axis-number))
                     (v (svref +axis-v+ axis-number))
                     (low (logior (ash 1 u) (ash 1 v)))
                     (high (logior low (ash 1 axis-number)))
                     (state
                       (logior (if (logbitp low mask) 1 0)
                               (if (logbitp high mask) 2 0)))
                     (source-bit
                       (svref *width-one-face-source-table* state)))
                (unless (minusp source-bit)
                  (let ((source-sample
                          (if (zerop source-bit) low high)))
                    (%check (< face-row face-end)
                            (format nil "face row ~2,'0X/~D"
                                    mask axis-number))
                    (%check
                     (equalp
                      (template-vertices (aref face-templates face-row))
                      (aref face-vertices
                            (+ (* 2 axis-number) source-bit)))
                     (format nil "face template ~2,'0X/~D"
                             mask axis-number))
                    (%check
                     (= (aref face-material-slots face-row)
                        (%width-one-contributor-slot
                         pattern
                         (aref *width-one-face-contributor-indices*
                               axis-number)))
                     (format nil "face material ~2,'0X/~D"
                             mask axis-number))
                    (%check
                     (= (aref face-source-offsets face-row)
                        (logior (if (logbitp 0 source-sample) 0 1)
                                (if (logbitp 1 source-sample) 0 2)))
                     (format nil "face source ~2,'0X/~D"
                             mask axis-number))
                    (incf face-row))))
              (let* ((edge-state (%width-one-edge-state mask axis-number))
                     (edge-pattern
                       (svref *width-one-edge-pattern-table* edge-state))
                     (indices
                       (svref
                        (width-one-edge-pattern-contributor-indices
                         edge-pattern)
                        axis-number)))
                (loop for descriptor across
                      (svref (width-one-edge-pattern-descriptors edge-pattern)
                             axis-number)
                      for transition-mask =
                        (width-one-template-descriptor-contributor-mask
                         descriptor)
                      do (%check (< band-row band-end)
                                 (format nil "band row ~2,'0X/~D"
                                         mask axis-number))
                         (%check
                          (equalp
                           (template-vertices
                            (aref band-templates band-row))
                           (width-one-template-descriptor-vertices descriptor))
                          (format nil "band template ~2,'0X/~D"
                                  mask axis-number))
                         (%check
                          (= (aref band-contributor-masks band-row)
                             (compact-contributors
                              pattern
                              (edge-cube-contributors
                               transition-mask indices)))
                          (format nil "band contributors ~2,'0X/~D"
                                  mask axis-number))
                         (%check
                          (= (aref band-source-witnesses band-row)
                             (source-witness
                              pattern indices transition-mask))
                          (format nil "band source ~2,'0X/~D"
                                  mask axis-number))
                         (%check
                          (= (aref band-ambient-stars band-row)
                             (if
                              (width-one-template-descriptor-ambient-star-p
                               descriptor)
                              1 0))
                          (format nil "band ambient ~2,'0X/~D"
                                  mask axis-number))
                         (incf band-row))))
            (%check (= face-row face-end)
                    (format nil "face extent ~2,'0X" mask))
            (%check (= band-row band-end)
                    (format nil "band extent ~2,'0X" mask))
            (loop for descriptor across
                  (width-one-vertex-pattern-descriptors pattern)
                  do (%check (< fan-row fan-end)
                             (format nil "fan row ~2,'0X" mask))
                     (%check
                      (equalp
                       (template-vertices (aref fan-templates fan-row))
                       (width-one-template-descriptor-vertices descriptor))
                      (format nil "fan template ~2,'0X" mask))
                     (%check
                      (= (aref fan-contributor-masks fan-row)
                         (compact-contributors
                          pattern
                          (width-one-template-descriptor-contributor-mask
                           descriptor)))
                      (format nil "fan contributors ~2,'0X" mask))
                     (incf fan-row))
            (%check (= fan-row fan-end)
                    (format nil "fan extent ~2,'0X" mask))))
        (%check (= (aref face-starts 256) (length face-templates)))
        (%check (= (aref band-starts 256) (length band-templates)))
        (%check (= (aref fan-starts 256) (length fan-templates)))))))

(defun %test-width-one-local-kernel ()
  (%with-test-section ("width-one finite-neighborhood production kernel")
    (let ((algebra (%make-width-one-test-chamfer-algebra))
          (empty-store (make-hash-table :test #'eql)))
      ;; The material batch is indexed entirely by compiled cube-edge slots.
      ;; Check that every descriptor mapping used by a face or edge state names
      ;; one of the star's dense contributor lanes; emission must never recover
      ;; this relation from a radial transition at run time.
      (dotimes (mask 256)
        (let* ((vertex-pattern
                 (svref *width-one-vertex-pattern-table* mask))
               (contributors
                 (width-one-vertex-pattern-contributors vertex-pattern))
               (slots
                 (width-one-vertex-pattern-contributor-slots vertex-pattern))
               (expected-slot 0))
          (dotimes (contributor-index 12)
            (if (minusp (aref contributors contributor-index))
                (%check (= -1 (aref slots contributor-index))
                        (format nil "absent contributor ~2,'0X/~D"
                                mask contributor-index))
                (progn
                  (%check (= expected-slot (aref slots contributor-index))
                          (format nil "dense contributor ~2,'0X/~D"
                                  mask contributor-index))
                  (incf expected-slot))))
          (%check
           (= expected-slot
              (width-one-vertex-pattern-contributor-count vertex-pattern))
           (format nil "contributor count ~2,'0X" mask))
          (dotimes (axis-number 3)
            (let* ((edge-state (%width-one-edge-state mask axis-number))
                   (edge-pattern
                     (svref *width-one-edge-pattern-table* edge-state))
                   (indices
                     (svref
                      (width-one-edge-pattern-contributor-indices edge-pattern)
                      axis-number)))
              (dotimes (transition 4)
                (let ((contributor-index (aref indices transition)))
                  (unless (minusp contributor-index)
                    (%check
                     (not (minusp (aref contributors contributor-index)))
                     (format nil "edge contributor ~2,'0X/~D/~D"
                             mask axis-number transition))))))
            (let* ((u (svref +axis-u+ axis-number))
                   (v (svref +axis-v+ axis-number))
                   (low (logior (ash 1 u) (ash 1 v)))
                   (high (logior low (ash 1 axis-number))))
              (when (not (eql (logbitp low mask) (logbitp high mask)))
                (%check
                 (not
                  (minusp
                   (aref
                    contributors
                    (aref *width-one-face-contributor-indices* axis-number))))
                 (format nil "face contributor ~2,'0X/~D"
                         mask axis-number)))))))
      ;; The complete 256-star corpus checks all face states, all sixteen edge
      ;; patterns in every orientation, every singular decomposition, material
      ;; contributor union, AO, construction-edge masks, triangle winding, and
      ;; every non-medial uniform template vocabulary.
      (dolist (width '(1 2 3))
        (dotimes (mask 256)
          (let* ((solid (%solid-for-star mask))
                 (fast
                   (%mesh-test-chunk
                    solid 0 empty-store
                    :source-stock-function #'%width-one-test-face-stock
                    :chamfer-stock-function #'%width-one-test-chamfer-stock
                    :chamfer-algebra algebra :bevel-width width))
                 (packed-reference
                   (%mesh-test-chunk-reference
                    solid 0 empty-store
                    :source-stock-function #'%width-one-test-face-stock
                    :chamfer-stock-function #'%width-one-test-chamfer-stock
                    :chamfer-algebra algebra :bevel-width width))
                 (oracle
                   (%mesh-test-chunk
                    solid 0 empty-store
                    :source-stock-function #'%width-one-test-face-stock
                    :chamfer-stock-function #'%width-one-test-chamfer-stock
                    :bevel-width width))
                 (fast-counts
                   (%canonical-triangle-record-counts (list fast)))
                 (reference-counts
                   (%canonical-triangle-record-counts (list packed-reference)))
                 (oracle-counts
                   (%canonical-triangle-record-counts (list oracle))))
            (%check
             (%triangle-counts= fast-counts oracle-counts)
             (format nil "width ~D star ~2,'0X" width mask))
            (%check
             (%triangle-counts= fast-counts reference-counts)
             (format nil "packed reference width ~D star ~2,'0X" width mask))
            (%check (= (surface-mesh-singular-star-count fast)
                       (surface-mesh-singular-star-count oracle))
                    (format nil "singular width ~D star ~2,'0X"
                            width mask)))))
      ;; Repeat the differential at real chunk seams, including every low-side
      ;; halo resolution used by the direct owned-star scan.
      (let* ((world (%chunk-test-world))
             (store (make-hash-table :test #'eql)))
        (map-chain-chunks
         (lambda (key chain) (setf (gethash key store) chain))
         world)
        (dolist (width '(1 2 3))
          (loop for key being the hash-keys of store using (hash-value chain)
                do (let ((fast
                           (%mesh-test-chunk
                            chain key store
                            :source-stock-function #'%width-one-test-face-stock
                            :chamfer-stock-function #'%width-one-test-chamfer-stock
                            :chamfer-algebra algebra :bevel-width width))
                         (oracle
                           (%mesh-test-chunk
                            chain key store
                            :source-stock-function #'%width-one-test-face-stock
                            :chamfer-stock-function #'%width-one-test-chamfer-stock
                            :bevel-width width))
                         (packed-reference
                           (%mesh-test-chunk-reference
                            chain key store
                            :source-stock-function #'%width-one-test-face-stock
                            :chamfer-stock-function #'%width-one-test-chamfer-stock
                            :chamfer-algebra algebra :bevel-width width)))
                     (%check
                      (%triangle-counts=
                       (%canonical-triangle-record-counts (list fast))
                       (%canonical-triangle-record-counts (list oracle)))
                      (format nil "width ~D chunk ~D" width key))
                     (%check
                      (%triangle-counts=
                       (%canonical-triangle-record-counts (list fast))
                       (%canonical-triangle-record-counts
                        (list packed-reference)))
                      (format nil "packed reference width ~D chunk ~D"
                              width key)))))))))

(defun %test-width-one-material-lanes ()
  (%with-test-section ("width-one compiled material lanes")
    ;; Authored side: every occupied cell names one of six placements; the
    ;; first two carry the architecture flag and the middle two the earth
    ;; flag, so architecture-on-earth columns exercise the foundation face.
    ;; The stock function resolves the same rule through hashes; the lane
    ;; path must reproduce it exactly through compiled chain-rank entries.
    (let* ((world (%chunk-test-world))
           (domain (chain-domain world))
           (algebra (%make-width-one-test-chamfer-algebra))
           (store (make-hash-table :test #'eql))
           (placements (make-hash-table :test #'eql))
           (placement-count 6)
           (architecture-flag 1)
           (earth-flag 2)
           (placement-flags
             (make-array placement-count :element-type '(unsigned-byte 8)))
           (face-stocks
             (make-array (* placement-count 7)
                         :element-type '(unsigned-byte 16))))
      (map-chain-chunks
       (lambda (key chain) (setf (gethash key store) chain))
       world)
      (dotimes (offset placement-count)
        (setf (aref placement-flags offset)
              (cond ((< offset 2) architecture-flag)
                    ((< offset 4) earth-flag)
                    (t 0)))
        (dotimes (face 7)
          (setf (aref face-stocks (+ (* offset 7) face))
                (svref #(1 2 4 8) (mod (+ offset face) 4)))))
      (loop for chain being the hash-values of store do
        (map-chain-facts-cells-ranked
         (lambda (rank x y z below-occupied-p)
           (declare (ignore rank below-occupied-p))
           (setf (gethash (make-site domain x y z +cell-extent+ 1)
                          placements)
                 (mod (+ x (* 3 y) (* 5 z)) placement-count)))
         (%chain-chunk-facts chain)))
      (labels ((placement-at (cell)
                 (multiple-value-bind (offset present-p)
                     (gethash cell placements)
                   (unless present-p
                     (error "Cell ~S has no test placement." cell))
                   offset))
               (foundation-p (cell offset)
                 (let ((z (site-z cell)))
                   (and (plusp z)
                        (logtest architecture-flag
                                 (aref placement-flags offset))
                        (let* ((below
                                 (make-site domain (site-x cell)
                                            (site-y cell) (1- z)
                                            +cell-extent+ 1))
                               (below-offset (gethash below placements)))
                          (and below-offset
                               (logtest earth-flag
                                        (aref placement-flags
                                              below-offset))
                               t)))))
               (stock-at (cell axis side)
                 (let* ((offset (placement-at cell))
                        (face (if (foundation-p cell offset)
                                  6
                                  (+ (* 2 (axis-index axis))
                                     (if (eq side :forward) 1 0)))))
                   (aref face-stocks (+ (* offset 7) face))))
               (fill-lane (facts entries)
                 (let ((previous-flags 0))
                   (map-chain-facts-cells-ranked
                    (lambda (rank x y z below-occupied-p)
                      (let* ((offset
                               (placement-at
                                (make-site domain x y z +cell-extent+ 1)))
                             (flags (aref placement-flags offset)))
                        (setf (aref entries rank)
                              (logior
                               (ash offset 1)
                               (if (and below-occupied-p
                                        (logtest architecture-flag flags)
                                        (logtest earth-flag previous-flags))
                                   1
                                   0))
                              previous-flags flags)))
                    facts)))
               (make-source (snapshot-key)
                 (make-width-one-material-source
                  :snapshot-key snapshot-key
                  :face-stocks face-stocks
                  :face-stride 7
                  :foundation-face-index 6
                  :fill-function #'fill-lane))
               (mesh-all (source)
                 (loop for key being the hash-keys of store
                         using (hash-value chain)
                       collect
                       (cons key
                             (%canonical-triangle-record-counts
                              (list
                               (apply #'%mesh-test-chunk
                                      chain key store
                                      :source-stock-function #'stock-at
                                      :chamfer-algebra algebra
                                      :bevel-width 1
                                      (when source
                                        (list :material-source source)))))))))
        (%reset-material-lanes :clear-cache t)
        (let* ((source (make-source (list :materials)))
               (lane-counts (mesh-all source))
               (hash-counts (mesh-all nil))
               (cold-builds *material-lane-build-count*))
          (loop for (key . counts) in lane-counts
                do (%check
                    (%triangle-counts=
                     counts (cdr (assoc key hash-counts)))
                    (format nil "lane materials chunk ~D" key)))
          (%check (= cold-builds (hash-table-count store))
                  "cold pass builds one lane per chain")
          ;; Warm reuse: unchanged chains under the same snapshot fill no
          ;; lanes; a fresh snapshot key refills every lane once and still
          ;; reproduces the same triangles over the surviving chain facts.
          (mesh-all source)
          (%check (= cold-builds *material-lane-build-count*)
                  "warm pass builds no lanes")
          (let ((edited-counts (mesh-all (make-source (list :materials)))))
            (%check (= (* 2 cold-builds) *material-lane-build-count*)
                    "material edit refills each lane once")
            (loop for (key . counts) in edited-counts
                  do (%check
                      (%triangle-counts=
                       counts (cdr (assoc key lane-counts)))
                      (format nil "post-edit chunk ~D" key)))))))))

(defun %test-chunked-meshing ()
  (%with-test-section ("chunked meshing equals whole-world meshing")
    (let* ((world (%chunk-test-world))
           (store (make-hash-table :test #'eql))
           (whole (make-surface-mesh world))
           (chunk-meshes '()))
      (map-chain-chunks
       (lambda (key chain) (setf (gethash key store) chain))
       world)
      (%check (= 4 (hash-table-count store)))
      (loop for key being the hash-keys of store using (hash-value chain)
            do (push
                (handler-bind
                    ((missing-chunk
                       (lambda (condition)
                         (let ((neighbor (gethash (missing-chunk-key condition)
                                                  store)))
                           (if neighbor
                               (invoke-restart 'use-chunk neighbor)
                               (invoke-restart 'treat-as-air)))))
                     (outside-domain
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'treat-as-air))))
                  (mesh-chunk chain key))
                chunk-meshes))
      (%check (= (surface-mesh-triangle-count whole)
                 (reduce #'+ chunk-meshes
                         :key #'surface-mesh-triangle-count)))
      (%check (= (surface-mesh-singular-star-count whole)
                 (reduce #'+ chunk-meshes
                         :key #'surface-mesh-singular-star-count)))
      (%check (%triangle-counts=
               (%canonical-triangle-counts (list whole))
               (%canonical-triangle-counts chunk-meshes))
              "chunked triangles differ from the whole-world mesh"))
    ;; A sparse cell on a high chunk seam leaves its edge and vertex
    ;; primitives canonically owned by the adjacent, otherwise empty chunk.
    ;; Streaming must therefore materialize that empty owner; meshing only
    ;; chunks present in the sparse solid is not a complete surface oracle.
    (dolist (seam '((63 20 64 20) (20 63 20 64)))
      (destructuring-bind (cell-x cell-y empty-x empty-y) seam
        (let* ((domain (make-world-domain :horizontal-bits 7))
               (cell (make-site domain cell-x cell-y 20 +cell-extent+ 1))
               (solid (%chain-from-sites domain (list cell)))
               (occupied-key (site-chunk-key cell))
               (empty-key (chunk-key-at empty-x empty-y))
               (empty (%chain-from-sites domain '()))
               (store (make-hash-table :test #'eql))
               (whole (make-surface-mesh solid)))
          (setf (gethash occupied-key store) solid
                (gethash empty-key store) empty)
          (labels ((owner-mesh (key)
                     (handler-bind
                         ((missing-chunk
                            (lambda (condition)
                              (multiple-value-bind (neighbor present-p)
                                  (gethash (missing-chunk-key condition) store)
                                (if present-p
                                    (invoke-restart 'use-chunk neighbor)
                                    (invoke-restart 'treat-as-air)))))
                          (outside-domain
                            (lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart 'treat-as-air))))
                       (mesh-chunk (gethash key store) key))))
            (let* ((occupied-mesh (owner-mesh occupied-key))
                   (empty-owner-mesh (owner-mesh empty-key))
                   (cohort (list occupied-mesh empty-owner-mesh)))
              (%check (not (%mesh-closed-p occupied-mesh)))
              (%check (plusp (surface-mesh-triangle-count empty-owner-mesh)))
              (%check (%meshes-closed-p cohort))
              (%check
               (%triangle-counts=
                (%canonical-triangle-record-counts (list whole))
                (%canonical-triangle-record-counts cohort))
               "explicit empty seam owner differs from whole-world oracle"))))))))

(defun %test-owner-preserving-variable-bevel-cohort ()
  (%with-test-section ("owner-preserving variable bevel cohort")
    (let* ((domain (make-world-domain :horizontal-bits 7))
           ;; Translate the retained five-cell 1/2/4 medial T-junction fixture
           ;; onto the X=64,Y=64 chunk corner.  Three chunks contain cells;
           ;; the diagonal +X+Y chunk is a sparse, virtual owner whose edge and
           ;; vertex primitives must nevertheless participate in the cohort.
           (solid
             (%chain-from-sites
              domain
              (list (make-site domain 64 63 2 +cell-extent+ 1)
                    (make-site domain 64 63 3 +cell-extent+ 1)
                    (make-site domain 63 63 2 +cell-extent+ 1)
                    (make-site domain 63 64 2 +cell-extent+ 1)
                    (make-site domain 63 64 3 +cell-extent+ 1))))
           (store (make-hash-table :test #'eql))
           (diagonal-owner (chunk-key-at 64 64))
           (stock-masks
             (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3)))
           (site-widths
             (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(0 4 1 2))))
      (map-chain-chunks
       (lambda (key chain) (setf (gethash key store) chain))
       solid)
      (setf (gethash diagonal-owner store) (make-chain domain))
      (let ((keys (sort (loop for key being the hash-keys of store collect key)
                        #'<)))
        (%check (= 4 (length keys)))
        (%check (chain-empty-p (gethash diagonal-owner store)))
        (labels ((face-solid-cell (face)
                   (let* ((extent (site-extent face))
                          (axis (cond ((= extent +xy-face-extent+) :z)
                                      ((= extent +xz-face-extent+) :y)
                                      (t :x)))
                          (x (site-x face))
                          (y (site-y face))
                          (z (site-z face))
                          (back-x (if (eq axis :x) (1- x) x))
                          (back-y (if (eq axis :y) (1- y) y))
                          (back-z (if (eq axis :z) (1- z) z)))
                     (if (= 1 (chain-cell-occupancy-bit solid x y z))
                         (make-site domain x y z +cell-extent+ 1)
                         (make-site domain back-x back-y back-z
                                    +cell-extent+ 1))))
                 (face-stock (face)
                   (let ((cell (face-solid-cell face)))
                     (if (= 63 (site-y cell))
                         1
                         0)))
                 (chamfer-stock (stocks)
                   (if (or (member 2 stocks :test #'=)
                           (and (member 0 stocks :test #'=)
                                (member 1 stocks :test #'=)))
                       2
                       (first stocks)))
                 (chunk-witness (key)
                   (handler-bind
                       ((missing-chunk
                          (lambda (condition)
                            (let ((neighbor
                                    (gethash (missing-chunk-key condition)
                                             store)))
                              (if neighbor
                                  (invoke-restart 'use-chunk neighbor)
                                  (invoke-restart 'treat-as-air)))))
                        (outside-domain
                          (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'treat-as-air))))
                     (mesh-chunk
                      (gethash key store) key
                      :stock-function #'face-stock
                      :chamfer-stock-function #'chamfer-stock
                      :bevel-width 1))))
          (let* ((whole-witness
                   (make-surface-mesh
                    solid :stock-function #'face-stock
                    :chamfer-stock-function #'chamfer-stock
                    :bevel-width 1))
                 (owner-witnesses
                   (mapcar (lambda (key) (cons key (chunk-witness key))) keys)))
            (%check
             (plusp
              (surface-mesh-triangle-count
               (cdr (assoc diagonal-owner owner-witnesses :test #'=))))
             "empty diagonal owner did not receive canonical primitives")
            (let ((stocks-by-site (make-hash-table :test #'eql))
                  (owners-by-site (make-hash-table :test #'eql)))
              (dolist (owner-witness owner-witnesses)
                (let ((owner (car owner-witness)))
                  (maphash
                   (lambda (site stocks)
                     (pushnew owner (gethash site owners-by-site) :test #'=)
                     (dolist (stock stocks)
                       (pushnew stock (gethash site stocks-by-site) :test #'=)))
                   (%witness-site-stock-table (cdr owner-witness)))))
              (%check
               (loop for site being the hash-keys of stocks-by-site
                       using (hash-value stocks)
                     thereis
                     (and (= 64 (%lattice-key-x site))
                          (= 64 (%lattice-key-y site))
                          (> (length (gethash site owners-by-site)) 1)
                          (> (length stocks) 1)))
               "no mixed-material site crossed the diagonal owner corner"))
            (multiple-value-bind
                  (whole width-census whole-diagnostics)
                (vary-surface-mesh-bevel-widths-from-stock-masks
                 whole-witness stock-masks site-widths)
              (multiple-value-bind
                    (owner-meshes cohort-census cohort-diagnostics)
                  (vary-surface-mesh-cohort-bevel-widths-from-stock-masks
                   owner-witnesses stock-masks site-widths)
                (%check (equal keys (mapcar #'car owner-meshes)))
                (%check (equalp width-census cohort-census))
                (%check (zerop (getf cohort-diagnostics
                                     :residual-edge-count)))
                (%check (plusp (getf cohort-diagnostics
                                     :repaired-edge-count))
                        (format nil "~S ~S"
                                cohort-census cohort-diagnostics))
                (%check (%meshes-closed-p (mapcar #'cdr owner-meshes)))
                (%check (every #'%mesh-nondegenerate-p
                               (mapcar #'cdr owner-meshes)))
                (%check
                 (%triangle-counts=
                  (%canonical-triangle-record-counts (list whole))
                  (%canonical-triangle-record-counts
                   (mapcar #'cdr owner-meshes)))
                 "owner-keyed transformed triangles differ from whole oracle")
                (%check (= (getf whole-diagnostics :collapsed-triangle-count)
                           (getf cohort-diagnostics
                                 :collapsed-triangle-count)))
                ;; Rebuild each owner while the complete four-owner closure is
                ;; guard context.  The selected product must be byte-for-byte
                ;; the same owner mesh as the all-output cohort; replacing it
                ;; beside all retained owners must recover the whole oracle.
                (let ((subset-repair-count 0)
                      (subset-repair-owner-count 0))
                  (dolist (output-owner keys)
                    (multiple-value-bind
                          (output-meshes subset-census subset-diagnostics)
                        (vary-surface-mesh-cohort-bevel-widths-from-stock-masks
                         owner-witnesses stock-masks site-widths
                         :output-owners (list output-owner))
                      (%check (= 1 (length output-meshes)))
                      (%check (equal output-owner (caar output-meshes)))
                      (%check (equalp width-census subset-census))
                      (%check (zerop (getf subset-diagnostics
                                           :residual-edge-count)))
                      (incf subset-repair-count
                            (getf subset-diagnostics :repaired-edge-count))
                      (when (plusp (getf subset-diagnostics
                                        :repaired-edge-count))
                        (incf subset-repair-owner-count))
                      (%check
                       (%same-surface-mesh-representation-p
                        (cdar output-meshes)
                        (cdr (assoc output-owner owner-meshes :test #'equal))))
                      (let ((replacement
                              (cons
                               (cdar output-meshes)
                               (loop for owner-mesh in owner-meshes
                                     unless (equal output-owner
                                                   (car owner-mesh))
                                       collect (cdr owner-mesh)))))
                        (%check (%meshes-closed-p replacement))
                        (%check
                         (%triangle-counts=
                          (%canonical-triangle-record-counts (list whole))
                          (%canonical-triangle-record-counts replacement))))))
                  (%check (plusp subset-repair-count))
                  (%check (plusp subset-repair-owner-count)))))))))))

(defun %star-triangle-p (triangle)
  (and (= 3 (length triangle))
       (every (lambda (point)
                (and (= 3 (length point))
                     (every #'integerp point)))
              triangle)))

(defun %star-orbit-count (&key reflections complement)
  (loop with unseen = (loop for star below 256 collect star)
        while unseen
        for orbit = (star-orbit (first unseen)
                                :reflections reflections
                                :complement complement)
        do (setf unseen (set-difference unseen orbit))
        count t))

(defun %same-geometric-triangle-p (left right)
  (equal (%unoriented-triangle-key left)
         (%unoriented-triangle-key right)))

(defun %geometric-triangle-subset-p (subset set)
  (every (lambda (triangle)
           (member triangle set :test #'%same-geometric-triangle-p))
         subset))

(defun %same-geometric-triangle-set-p (left right)
  (and (%geometric-triangle-subset-p left right)
       (%geometric-triangle-subset-p right left)))

(defun %star-face-corner-count (star)
  "Count solid/air cell adjacencies among the eight cells of STAR."
  (loop for sample below 8
        sum
        (loop for axis below 3
              unless (logbitp axis sample)
                count (not (eql (logbitp sample star)
                                (logbitp (logxor sample (ash 1 axis))
                                         star))))))

(defun %test-star-geometry ()
  (%with-test-section ("plain width-one star triangle geometry")
    (dotimes (star 256)
      (let* ((geometry (star-triangles star))
             (local-surface (star-local-surface-triangles star))
             (faces (getf geometry :faces))
             (bands (getf geometry :bands))
             (junctions (getf geometry :junctions))
             (local-faces (getf local-surface :faces))
             (local-bands (getf local-surface :bands))
             (local-junctions (getf local-surface :junctions)))
        (%check (equal faces (star-face-triangles star)))
        (%check (equal bands (star-band-triangles star)))
        (%check (equal junctions (star-junction-triangles star)))
        (%check (every #'%star-triangle-p
                       (append faces bands junctions)))
        (%check (every #'%star-triangle-p
                       (append local-faces local-bands local-junctions)))
        (%check (%geometric-triangle-subset-p faces local-faces))
        (%check (%geometric-triangle-subset-p bands local-bands))
        (%check (equal junctions local-junctions))
        ;; Every solid/air cell adjacency contributes one inset quadrilateral.
        (%check (= (length local-faces)
                   (* 2 (%star-face-corner-count star))))))
    (%check (equal '(:faces nil :bands nil :junctions nil)
                   (star-triangles #x00)))
    (%check (equal '(:faces nil :bands nil :junctions nil)
                   (star-triangles #xff)))
    (%check (equal '(2 4 1)
                   (let ((geometry (star-triangles #x08)))
                     (list (length (getf geometry :faces))
                           (length (getf geometry :bands))
                           (length (getf geometry :junctions)))))
            "representative star lost a face, band, or junction")
    (%check (equal '(6 12 1)
                   (let ((geometry (star-local-surface-triangles #x08)))
                     (list (length (getf geometry :faces))
                           (length (getf geometry :bands))
                           (length (getf geometry :junctions)))))
            "one occupied octant should expose its three complete corners"))
  (%with-test-section ("cubical star symmetries")
    (let* ((rotations (star-rotations))
           (reflections (star-reflections))
           (transformations (append rotations reflections))
           (axis-permutations
             (remove-if (lambda (transformation)
                          (some (lambda (row) (member -1 row))
                                transformation))
                        transformations))
           (swap-x-y '((0 1 0) (1 0 0) (0 0 1))))
      (%check (= 24 (length rotations)))
      (%check (= 24 (length reflections)))
      (%check (= 48 (length (remove-duplicates transformations :test #'equal))))
      (%check (= 6 (length axis-permutations)))
      (%check (= #x80
                 (transform-star '((-1 0 0) (0 -1 0) (0 0 -1)) #x01)))
      (%check (= #x04 (transform-star swap-x-y #x02)))
      (%check (= 23 (%star-orbit-count)))
      (%check (= 22 (%star-orbit-count :reflections t)))
      (%check (= 15 (%star-orbit-count :complement t)))
      (%check (= 14 (%star-orbit-count :reflections t :complement t)))
      ;; #x1b and #x1d are the one chiral pair of proper-rotation classes.
      (%check (not (member #x1d (star-orbit #x1b))))
      (%check (member #x1d (star-orbit #x1b :reflections t)))
      ;; Axis names do not affect the row census, although row ownership and
      ;; template triangulation mean that the coordinate lists themselves are
      ;; deliberately not a representation of a complete symmetric star.
      (dolist (transformation axis-permutations)
        (dotimes (star 256)
          (let ((left (star-triangles star))
                (right
                  (star-triangles (transform-star transformation star))))
            (%check
             (equal (mapcar (lambda (key) (length (getf left key)))
                            '(:faces :bands :junctions))
                    (mapcar (lambda (key) (length (getf right key)))
                            '(:faces :bands :junctions)))))))
      (%check (= #x06 (transform-star swap-x-y #x06)))
      (%check
       (not (equal (transform-star-triangles
                    swap-x-y (star-band-triangles #x06))
                   (star-band-triangles #x06)))
       "one query row must not masquerade as a complete symmetric star")
      ;; Forgetting ownership restores proper rotational symmetry to the
      ;; incident faces and bands, even though no individual query row has it.
      (dolist (star '(#x08 #x1f #x1b))
        (let ((surface (star-local-surface-triangles star)))
          (dolist (rotation rotations)
            (let ((rotated-surface
                    (star-local-surface-triangles
                     (transform-star rotation star))))
              (dolist (key '(:faces :bands))
                (%check
                 (%same-geometric-triangle-set-p
                  (transform-star-triangles rotation (getf surface key))
                  (getf rotated-surface key)))))))))))

(defun run-luft-tests (&key (stream *standard-output*))
  "Run the retained topology and replacement manifold-sheet mesh claims."
  (let ((*luft-test-count* 0)
        (*luft-test-section* nil))
    (%test-sites-and-chains)
    (%test-voxel-light)
    (%test-source-stock-provenance)
    (%test-surface-attachment-square-chart)
    (%test-surface-attachment-off-ray-shared-edge-ties)
    (%test-surface-attachment-local-support-chart)
    (%test-sheet-decomposition)
    (%test-surface-mesh)
    (%test-width-one-z-fibers)
    (%test-width-one-query-dimension)
    (%test-star-geometry)
    (%test-width-one-local-kernel)
    (%test-width-one-material-lanes)
    (%test-chunked-meshing)
    (%test-owner-preserving-variable-bevel-cohort)
    (when stream
      (format stream "~&LUFT: ~D checks passed.~%" *luft-test-count*))
    (values t *luft-test-count*)))
