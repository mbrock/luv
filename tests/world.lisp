(defpackage #:luv/tests
  (:use #:cl #:rove #:luv))

(in-package #:luv/tests)

(defun coordinate= (left right)
  (and (= (world-coordinate-x left) (world-coordinate-x right))
       (= (world-coordinate-y left) (world-coordinate-y right))
       (= (world-coordinate-z left) (world-coordinate-z right))))

(deftest signed-world-coordinate-decomposition
  (let ((space (make-voxel-space
                :chunk-shape (make-chunk-shape :width 16
                                               :height 8
                                               :depth 4)
                :cell-extent #(0.5d0 2d0 4d0))))
    (ok (equalp (world-coordinate-cell-origin
                 space (make-world-coordinate -2 3 1))
                #(-1d0 6d0 4d0)))
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
    (setf (block-at world 0 0 0) stone
          (block-at world 1 0 0) stone)
    (ok (= (length (block-content-column-palette column)) 2))
    (ok (= (aref (block-content-column-indices column) 0) 1))
    (ok (= (aref (block-content-column-indices column) 1) 1))
    (multiple-value-bind (block status) (block-at world 1 0 0)
      (ok (eq block stone))
      (ok (eq status :resident)))
    (multiple-value-bind (block status) (block-at world 2 0 0)
      (ok (null block))
      (ok (eq status :resident)))
    (multiple-value-bind (block status) (block-at world 4 0 0)
      (ok (null block))
      (ok (eq status :absent)))))

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
    (setf (block-at world 0 0 0) stone)
    (ok (= (block-chunk-revision chunk) 1))
    (ok (= (block-world-revision world) 2))
    (setf (block-at world 0 0 0) stone)
    (ok (= (block-chunk-revision chunk) 1))
    (ok (= (block-world-revision world) 2))
    (setf (block-at world 0 0 0) nil)
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

(deftest negative-and-cross-chunk-access
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world -1 0 0))
         (right (ensure-world-chunk world 0 0 0))
         (stone (list :stone))
         (dirt (list :dirt)))
    (setf (block-at world -1 0 0) stone
          (block-at world 0 0 0) dirt)
    (ok (eq (chunk-block-at left 3 0 0) stone))
    (ok (eq (chunk-block-at right 0 0 0) dirt))
    (multiple-value-bind (block status) (block-at world -5 0 0)
      (ok (null block))
      (ok (eq status :absent)))
    (ok (signals (setf (block-at world -5 0 0) stone)
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

(deftest resident-lattice-raycast
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (stone (list :stone)))
    (ensure-world-chunk world 0 0 0)
    (setf (block-at world 2 1 1) stone)
    (multiple-value-bind (hit status)
        (raycast-block-world world #(0.5d0 1.5d0 1.5d0) #(1d0 0d0 0d0)
                             #'identity :max-distance 8d0)
      (ok (eq status :hit))
      (ok (eq (block-ray-hit-block hit) stone))
      (ok (coordinate= (block-ray-hit-coordinate hit)
                       (make-world-coordinate 2 1 1)))
      (ok (coordinate= (block-ray-hit-adjacent-coordinate hit)
                       (make-world-coordinate 1 1 1)))
      (ok (= (block-ray-hit-distance hit) 1.5d0)))
    (setf (block-at world 2 1 1) nil)
    (multiple-value-bind (hit status)
        (raycast-block-world world #(0.5d0 1.5d0 1.5d0) #(1d0 0d0 0d0)
                             #'identity :max-distance 2d0)
      (ok (null hit))
      (ok (eq status :miss)))
    (multiple-value-bind (hit status)
        (raycast-block-world world #(0.5d0 1.5d0 1.5d0) #(1d0 0d0 0d0)
                             #'identity :max-distance 8d0)
      (ok (null hit))
      (ok (eq status :absent)))
    (ok (signals
         (raycast-block-world world #(0d0 0d0 0d0) #(0d0 0d0 0d0)
                              #'identity)))))
