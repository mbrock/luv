(defpackage #:luv/tests
  (:use #:cl #:rove #:luv))

(in-package #:luv/tests)

(defun coordinate= (left right)
  (and (= (world-coordinate-x left) (world-coordinate-x right))
       (= (world-coordinate-y left) (world-coordinate-y right))
       (= (world-coordinate-z left) (world-coordinate-z right))))

(deftest vec3-bundles-continuous-components
  (let ((vector (make-vec3 3d0 4d0 0d0)))
    (ok (= (vec3-length vector) 5d0))
    (ok (= (vec3-dot vector (make-vec3 2d0 0d0 1d0)) 6d0))
    (ok (equalp (vec3-scale vector 2d0) (make-vec3 6d0 8d0 0d0)))
    (ok (equalp (vec3-cross (make-vec3 1d0 0d0 0d0)
                            (make-vec3 0d0 1d0 0d0))
                (make-vec3 0d0 0d0 1d0)))
    (let ((normalized (vec3-normalize vector)))
      (ok (< (abs (- (vec3-x normalized) 0.6d0)) 1d-12))
      (ok (< (abs (- (vec3-y normalized) 0.8d0)) 1d-12))
      (ok (zerop (vec3-z normalized))))
    (setf (vec3-component vector :z) 5d0)
    (ok (equal (vec3-list vector) '(3d0 4d0 5d0)))))

(deftest signed-world-coordinate-decomposition
  (let ((space (make-voxel-space
                :chunk-shape (make-chunk-shape :width 16
                                               :height 8
                                               :depth 4)
                :cell-extent #(0.5d0 2d0 4d0))))
    (ok (equalp (world-coordinate-cell-origin
                 space (make-world-coordinate -2 3 1))
                (make-vec3 -1d0 6d0 4d0)))
    (dolist (case '((-17 -2 15) (-16 -1 0) (-1 -1 15)
                    (0 0 0) (15 0 15) (16 1 0)))
      (destructuring-bind (world-x expected-chunk-x expected-local-x) case
        (multiple-value-bind (chunk local)
            (world-coordinate-chunk-and-local
             space (make-world-coordinate world-x -1 4))
          (ok (= (chunk-coordinate-x chunk) expected-chunk-x))
          (ok (= (local-coordinate-x local) expected-local-x))
          (ok (= (chunk-coordinate-y chunk) -1))
          (ok (= (local-coordinate-y local) 7))
          (ok (= (chunk-coordinate-z chunk) 1))
          (ok (= (local-coordinate-z local) 0))
          (ok (coordinate=
               (chunk-local-world-coordinate space chunk local)
               (make-world-coordinate world-x -1 4))))))))

(deftest lattice-and-metric-storage-declarations-stay-distinct
  (let* ((extent
           (luv.arithmetic.records:record-slot-declaration
            'voxel-space 'luv::cell-extent))
         (distance
           (luv.arithmetic.records:record-slot-declaration
            'block-ray-hit 'luv::distance))
         (extent-quantity
           (luv.arithmetic:declaration-quantity-specification extent))
         (distance-quantity
           (luv.arithmetic:declaration-quantity-specification distance)))
    (ok (eq :voxel-cell-extent
            (luv.arithmetic:quantity-specification-name extent-quantity)))
    (ok (luv.arithmetic:unit-expression=
         '((:metre 1) (:cell -1))
         (luv.arithmetic:quantity-specification-unit extent-quantity)))
    (ok (eq :ray-distance
            (luv.arithmetic:quantity-specification-name distance-quantity)))
    (ok (luv.arithmetic:unit-expression=
         :cell (luv.arithmetic:quantity-specification-unit distance-quantity)))
    (ok (not (luv.arithmetic:unitless-p
              (luv.arithmetic:make-unit-expression :cell))))))

(deftest chunk-domain-indexing
  (let* ((space (make-voxel-space
                 :chunk-shape (make-chunk-shape :width 4
                                                :height 3
                                                :depth 2)))
         (domain (make-chunk-domain space (make-chunk-coordinate -2 1 3))))
    (ok (= (chunk-domain-cardinality domain) 24))
    (ok (= (chunk-domain-offset domain (make-local-coordinate 1 0 0)) 1))
    (ok (= (chunk-domain-offset domain (make-local-coordinate 0 1 0)) 4))
    (ok (= (chunk-domain-offset domain (make-local-coordinate 0 0 1)) 12))
    (dotimes (offset (chunk-domain-cardinality domain))
      (ok (= (chunk-domain-offset
              domain (chunk-domain-local-coordinate domain offset))
             offset)))
    (ok (coordinate= (chunk-domain-origin domain)
                     (make-world-coordinate -8 3 6)))
    (ok (signals
         (chunk-domain-offset domain (make-local-coordinate 4 0 0))))))

(deftest chunk-domain-coordinate-traversal
  (let* ((space (make-voxel-space
                 :chunk-shape (make-chunk-shape :width 4
                                                :height 3
                                                :depth 2)))
         (domain (make-chunk-domain space (make-chunk-coordinate -2 1 3)))
         (sites nil)
         (positive-x-face nil))
    (multiple-value-bind (chunk local)
        (world-coordinate-chunk-and-local
         space (make-world-coordinate -5 5 5))
      (ok (equalp chunk (make-chunk-coordinate -2 1 2)))
      (ok (equalp local (make-local-coordinate 3 2 1))))
    (let ((local (chunk-domain-local-coordinate domain 23)))
      (ok (equalp local (make-local-coordinate 3 2 1)))
      (ok (equalp (chunk-domain-world-coordinate domain local)
                  (make-world-coordinate -5 5 7))))
    (multiple-value-bind (offset destination crossing)
        (step-chunk-domain-site
         domain (make-local-coordinate 1 1 1) +voxel-positive-x+)
      (ok (= offset 18))
      (ok (equalp destination (make-local-coordinate 2 1 1)))
      (ok (null crossing)))
    (multiple-value-bind (offset destination crossing)
        (step-chunk-domain-site
         domain (make-local-coordinate 0 1 1) +voxel-negative-x+)
      (ok (= offset 19))
      (ok (equalp destination (make-local-coordinate 3 1 1)))
      (ok (eq crossing +voxel-negative-x+)))
    (do-chunk-domain-sites (offset local domain)
      (push (list offset
                  (local-coordinate-x local)
                  (local-coordinate-y local)
                  (local-coordinate-z local))
            sites))
    (ok (= (length sites) 24))
    (ok (equal (first (last sites)) '(0 0 0 0)))
    (ok (equal (first sites) '(23 3 2 1)))
    (do-chunk-domain-face
        (offset local domain +voxel-positive-x+)
      (declare (ignore local))
      (push offset positive-x-face))
    (ok (equal (nreverse positive-x-face) '(3 7 11 15 19 23)))
    (ok (signals (make-voxel-direction 1 1 0)))))

(deftest palette-backed-block-content
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (chunk (ensure-world-chunk world 0 0 0))
         (column (block-chunk-content chunk))
         (stone (list :stone)))
    (ok (equal (array-element-type (block-content-column-indices column))
               '(unsigned-byte 16)))
    (ok (= (length (block-content-column-indices column)) 64))
    (ok (= (length (block-content-column-palette column)) 1))
    (ok (null (aref (block-content-column-palette column) 0)))
    (setf (world-block-at world 0 0 0) stone
          (world-block-at world 1 0 0) stone)
    (ok (= (length (block-content-column-palette column)) 2))
    (ok (= (aref (block-content-column-indices column) 0) 1))
    (ok (= (aref (block-content-column-indices column) 1) 1))
    (multiple-value-bind (block status) (world-block-at world 1 0 0)
      (ok (eq block stone))
      (ok (eq status :resident)))
    (multiple-value-bind (block status) (world-block-at world 2 0 0)
      (ok (null block))
      (ok (eq status :resident)))
    (multiple-value-bind (block status) (world-block-at world 4 0 0)
      (ok (null block))
      (ok (eq status :absent)))))

(deftest whole-domain-block-storage-is-borrowed-without-row-objects
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 3
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (stone (list :stone))
         (described nil))
    (setf (chunk-block-at-offset chunk 1) stone)
    (ok (= (block-chunk-revision chunk) 1))
    (ok (= (block-world-revision world) 2))
    (setf (chunk-block-at-offset chunk 1) stone)
    (ok (= (block-chunk-revision chunk) 1))
    (ok (= (block-world-revision world) 2))
    (ok (= (block-chunk-boundary-revision chunk +voxel-negative-y+) 1))
    (ok (= (block-chunk-boundary-revision chunk +voxel-negative-z+) 1))
    (with-block-content-storage (domain palette indices) chunk
      (ok (eq domain (block-chunk-domain chunk)))
      (ok (eq palette
              (block-content-column-palette (block-chunk-content chunk))))
      (ok (eq indices
              (block-content-column-indices (block-chunk-content chunk))))
      (ok (= (length indices) (chunk-domain-cardinality domain)))
      (ok (eq (aref palette (aref indices 1)) stone)))
    (map-chunk-blocks
     (lambda (block local)
       (when block
         ;; Retaining LOCAL is valid in this presentation protocol; the dense
         ;; traversal macro itself instead lends a dynamic-extent value.
         (push local described)))
     chunk)
    (ok (= (length described) 1))
    (ok (equalp (first described) (make-local-coordinate 1 0 0)))))

(deftest chunk-and-residency-revisions
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (stone (list :stone))
         (chunk (ensure-world-chunk world 0 0 0)))
    (ok (= (block-world-residency-revision world) 1))
    (ok (= (block-world-revision world) 1))
    (ok (eq (ensure-world-chunk world 0 0 0) chunk))
    (ok (= (block-world-residency-revision world) 1))
    (ok (= (block-world-revision world) 1))
    (ok (= (block-chunk-revision chunk) 0))
    (setf (world-block-at world 0 0 0) stone)
    (ok (= (block-chunk-revision chunk) 1))
    (ok (= (block-world-revision world) 2))
    (setf (world-block-at world 0 0 0) stone)
    (ok (= (block-chunk-revision chunk) 1))
    (ok (= (block-world-revision world) 2))
    (setf (world-block-at world 0 0 0) nil)
    (ok (= (block-chunk-revision chunk) 2))
    (ok (= (block-world-revision world) 3))
    ;; A resident chunk is still world-owned: lower-level writes must also
    ;; invalidate world-derived products such as meshes.
    (setf (chunk-block-at chunk 1 0 0) stone)
    (ok (= (block-chunk-revision chunk) 3))
    (ok (= (block-world-revision world) 4))
    (multiple-value-bind (removed present-p) (remove-world-chunk world 0 0 0)
      (ok (eq removed chunk))
      (ok present-p))
    (ok (= (block-world-residency-revision world) 2))
    (ok (= (block-world-revision world) 5))
    (setf (chunk-block-at chunk 2 0 0) stone)
    (ok (= (block-chunk-revision chunk) 4))
    (ok (= (block-world-revision world) 5))))

(deftest chunk-boundaries-and-world-change-transactions
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (stone (list :stone))
         (chunk nil))
    (with-world-change-transaction (world)
      (setf chunk (ensure-world-chunk world 0 0 0))
      ;; Interior content changes do not invalidate neighbor-derived products.
      (setf (chunk-block-at chunk 1 1 1) stone)
      (with-world-change-transaction (world)
        (setf (chunk-block-at chunk 0 1 1) stone)))
    (ok (= (block-world-revision world) 1))
    (ok (= (block-world-residency-revision world) 1))
    (ok (= (block-chunk-revision chunk) 2))
    (ok (= (block-chunk-boundary-revision chunk +voxel-negative-x+) 1))
    (ok (= (block-chunk-boundary-revision chunk +voxel-positive-x+) 0))
    (ok (= (block-chunk-boundary-revision chunk +voxel-negative-y+) 0))
    (ok (= (block-chunk-boundary-revision chunk +voxel-positive-y+) 0))
    (ok (= (block-chunk-boundary-revision chunk +voxel-negative-z+) 0))
    (ok (= (block-chunk-boundary-revision chunk +voxel-positive-z+) 0))
    ;; A transaction containing only no-op assignments is itself a no-op.
    (with-world-change-transaction (world)
      (setf (chunk-block-at chunk 1 1 1) stone))
    (ok (= (block-world-revision world) 1))
    (setf (chunk-block-at chunk 3 3 3) stone)
    (ok (= (block-world-revision world) 2))
    (ok (= (block-chunk-boundary-revision chunk +voxel-positive-x+) 1))
    (ok (= (block-chunk-boundary-revision chunk +voxel-positive-y+) 1))
    (ok (= (block-chunk-boundary-revision chunk +voxel-positive-z+) 1))))

(deftest negative-and-cross-chunk-access
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world -1 0 0))
         (right (ensure-world-chunk world 0 0 0))
         (stone (list :stone))
         (dirt (list :dirt)))
    (setf (world-block-at world -1 0 0) stone
          (world-block-at world 0 0 0) dirt)
    (ok (eq (chunk-block-at left 3 0 0) stone))
    (ok (eq (chunk-block-at right 0 0 0) dirt))
    (multiple-value-bind (block status) (world-block-at world -5 0 0)
      (ok (null block))
      (ok (eq status :absent)))
    (ok (signals (setf (world-block-at world -5 0 0) stone)
                 'chunk-not-resident))))

(deftest domain-identity-and-deterministic-residency
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 1 0 0)
    (ensure-world-chunk world -1 0 0)
    (ensure-world-chunk world 0 0 1)
    (let* ((chunks (resident-world-chunks world))
           (domains (mapcar #'block-chunk-domain chunks))
           (x-coordinates
             (mapcar (lambda (domain)
                       (chunk-coordinate-x
                        (chunk-domain-coordinate domain)))
                     domains)))
      (ok (equal x-coordinates '(-1 0 1)))
      (ok (every (lambda (domain)
                   (eq (chunk-domain-space domain) (block-world-space world)))
                 domains))
      (ok (not (eq (first domains) (second domains)))))))

(deftest chunk-incarnations-survive-eviction-and-storage-transfer-is-owned
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (first (ensure-world-chunk world 0 0 0))
         (palette (make-array 2 :initial-contents
                              (list nil :transferred-stone)))
         (indices (make-array 8 :element-type '(unsigned-byte 16)
                               :initial-element 0)))
    (setf (aref indices 3) 1)
    (remove-world-chunk world 0 0 0)
    (let ((second (install-world-chunk-storage
                   world 0 0 0 palette indices)))
      (ok (> (block-chunk-incarnation second)
             (block-chunk-incarnation first)))
      (ok (eq (chunk-block-at-offset second 3) :transferred-stone))
      (ok (eq (block-content-column-palette (block-chunk-content second))
              palette))
      (ok (eq (block-content-column-indices (block-chunk-content second))
              indices)))))

(deftest resident-lattice-raycast
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (stone (list :stone)))
    (ensure-world-chunk world 0 0 0)
    (setf (world-block-at world 2 1 1) stone)
    (multiple-value-bind (hit status)
        (raycast-block-world world
                             (make-vec3 0.5d0 1.5d0 1.5d0)
                             (make-vec3 1d0 0d0 0d0)
                             #'identity :max-distance 8d0)
      (ok (eq status :hit))
      (ok (eq (block-ray-hit-block hit) stone))
      (ok (coordinate= (block-ray-hit-coordinate hit)
                       (make-world-coordinate 2 1 1)))
      (ok (coordinate= (block-ray-hit-adjacent-coordinate hit)
                       (make-world-coordinate 1 1 1)))
      (ok (= (block-ray-hit-distance hit) 1.5d0)))
    (setf (world-block-at world 2 1 1) nil)
    (multiple-value-bind (hit status)
        (raycast-block-world world
                             (make-vec3 0.5d0 1.5d0 1.5d0)
                             (make-vec3 1d0 0d0 0d0)
                             #'identity :max-distance 2d0)
      (ok (null hit))
      (ok (eq status :miss)))
    (multiple-value-bind (hit status)
        (raycast-block-world world
                             (make-vec3 0.5d0 1.5d0 1.5d0)
                             (make-vec3 1d0 0d0 0d0)
                             #'identity :max-distance 8d0)
      (ok (null hit))
      (ok (eq status :absent)))
    (ok (signals
         (raycast-block-world world
                              (make-vec3 0d0 0d0 0d0)
                              (make-vec3 0d0 0d0 0d0)
                              #'identity)))))
