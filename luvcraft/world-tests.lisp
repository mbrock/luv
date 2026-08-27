(in-package #:luvcraft.tests)

(luvcraft.world.fields:define-voxel-field :test-voxel-reading
  :site-kind :voxel-cell
  :value-type double-float
  :quantity (:quantity :distance :unit :metre)
  :missing-value :unavailable
  :legal-values double-float
  :representation :test-v1)

(defun coordinate= (left right)
  (and (= (world-coordinate-x left) (world-coordinate-x right))
       (= (world-coordinate-y left) (world-coordinate-y right))
       (= (world-coordinate-z left) (world-coordinate-z right))))

(define-test signed-world-coordinate-decomposition
  (let ((space (make-voxel-space
                :chunk-shape (make-chunk-shape :width 16
                                               :height 8
                                               :depth 4)
                :cell-extent #(0.5d0 2d0 4d0))))
    (true (equalp (world-coordinate-cell-origin
                   space (make-world-coordinate -2 3 1))
                  (make-vec3 -1d0 6d0 4d0)))
    (dolist (case '((-17 -2 15) (-16 -1 0) (-1 -1 15)
                    (0 0 0) (15 0 15) (16 1 0)))
      (destructuring-bind (world-x expected-chunk-x expected-local-x) case
        (multiple-value-bind (chunk local)
            (world-coordinate-chunk-and-local
             space (make-world-coordinate world-x -1 4))
          (true (= (chunk-coordinate-x chunk) expected-chunk-x))
          (true (= (local-coordinate-x local) expected-local-x))
          (true (= (chunk-coordinate-y chunk) -1))
          (true (= (local-coordinate-y local) 7))
          (true (= (chunk-coordinate-z chunk) 1))
          (true (= (local-coordinate-z local) 0))
          (true (coordinate=
                 (chunk-local-world-coordinate space chunk local)
                 (make-world-coordinate world-x -1 4))))))))

(define-test lattice-and-metric-storage-declarations-stay-distinct
  (let* ((extent
           (luv.arithmetic.records:record-slot-declaration
            'voxel-space 'luvcraft.world::cell-extent))
         (distance
           (luv.arithmetic.records:record-slot-declaration
            'block-ray-hit 'luvcraft.world::distance))
         (extent-quantity
           (luv.arithmetic:declaration-quantity-specification extent))
         (distance-quantity
           (luv.arithmetic:declaration-quantity-specification distance)))
    (true (eq :voxel-cell-extent
              (luv.arithmetic:quantity-specification-name extent-quantity)))
    (true (luv.arithmetic:unit-expression=
           '((:metre 1) (:cell -1))
           (luv.arithmetic:quantity-specification-unit extent-quantity)))
    (true (eq :ray-distance
              (luv.arithmetic:quantity-specification-name distance-quantity)))
    (true (luv.arithmetic:unit-expression=
           :cell (luv.arithmetic:quantity-specification-unit distance-quantity)))
    (true (not (luv.arithmetic:unitless-p
                (luv.arithmetic:make-unit-expression :cell))))))

(define-test voxel-field-definitions-separate-meaning-from-storage-policy
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (column (block-chunk-content chunk))
         (definition
           (luvcraft.world.fields:field-definition-for :block-content)))
    (true (eq :block-content
              (luvcraft.world.fields:voxel-field-definition-name definition)))
    (true (eq :voxel-cell
              (luvcraft.world.fields:voxel-field-definition-site-kind definition)))
    (true (eq :palette-u16
              (luvcraft.world.fields:voxel-field-definition-representation-policy
               definition)))
    (true (null
           (luv.arithmetic:declaration-quantity-specification definition)))
    (true (eq definition
              (luvcraft.world.fields:materialized-field-definition
               column :block-content)))
    (true (eq column
              (luvcraft.world.fields:materialized-field-representation
               chunk :block-content)))
    (true (eq (block-chunk-domain chunk)
              (luvcraft.world.fields:field-representation-domain column)))
    (true (luvcraft.world.fields:materialized-field-current-p
           column :block-content))))

(define-test field-redefinition-has-visible-definition-identity
  (let* ((old (luvcraft.world.fields:field-definition-for :test-voxel-reading))
         (old-revision
           (luvcraft.world.fields:voxel-field-definition-revision old)))
    (handler-bind ((warning #'muffle-warning))
      (eval
       '(luvcraft.world.fields:define-voxel-field :test-voxel-reading
          :site-kind :voxel-cell
          :value-type double-float
          :quantity (:quantity :distance :unit :metre)
          :missing-value :unavailable
          :legal-values double-float
          :representation :test-v2)))
    (let ((new (luvcraft.world.fields:field-definition-for :test-voxel-reading)))
      (true (not (eq old new)))
      (true (not (eq old-revision
                     (luvcraft.world.fields:voxel-field-definition-revision new))))
      (true (eq :test-v2
                (luvcraft.world.fields:voxel-field-definition-representation-policy
                 new))))))

(define-test chunk-domain-indexing
  (let* ((space (make-voxel-space
                 :chunk-shape (make-chunk-shape :width 4
                                                :height 3
                                                :depth 2)))
         (domain (make-chunk-domain space (make-chunk-coordinate -2 1 3))))
    (true (= (chunk-domain-cardinality domain) 24))
    (true (= (chunk-domain-offset domain (make-local-coordinate 1 0 0)) 1))
    (true (= (chunk-domain-offset domain (make-local-coordinate 0 1 0)) 4))
    (true (= (chunk-domain-offset domain (make-local-coordinate 0 0 1)) 12))
    (dotimes (offset (chunk-domain-cardinality domain))
      (true (= (chunk-domain-offset
                domain (chunk-domain-local-coordinate domain offset))
               offset)))
    (true (coordinate= (chunk-domain-origin domain)
                       (make-world-coordinate -8 3 6)))
    (fail
     (chunk-domain-offset domain (make-local-coordinate 4 0 0)))))

(define-test chunk-domain-coordinate-traversal
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
      (true (equalp chunk (make-chunk-coordinate -2 1 2)))
      (true (equalp local (make-local-coordinate 3 2 1))))
    (let ((local (chunk-domain-local-coordinate domain 23)))
      (true (equalp local (make-local-coordinate 3 2 1)))
      (true (equalp (chunk-domain-world-coordinate domain local)
                    (make-world-coordinate -5 5 7))))
    (multiple-value-bind (offset destination crossing)
        (step-chunk-domain-site
         domain (make-local-coordinate 1 1 1) +voxel-positive-x+)
      (true (= offset 18))
      (true (equalp destination (make-local-coordinate 2 1 1)))
      (true (null crossing)))
    (multiple-value-bind (offset destination crossing)
        (step-chunk-domain-site
         domain (make-local-coordinate 0 1 1) +voxel-negative-x+)
      (true (= offset 19))
      (true (equalp destination (make-local-coordinate 3 1 1)))
      (true (eq crossing +voxel-negative-x+)))
    (do-chunk-domain-sites (offset local domain)
      (push (list offset
                  (local-coordinate-x local)
                  (local-coordinate-y local)
                  (local-coordinate-z local))
            sites))
    (true (= (length sites) 24))
    (true (equal (first (last sites)) '(0 0 0 0)))
    (true (equal (first sites) '(23 3 2 1)))
    (do-chunk-domain-face
        (offset local domain +voxel-positive-x+)
      (declare (ignore local))
      (push offset positive-x-face))
    (true (equal (nreverse positive-x-face) '(3 7 11 15 19 23)))
    (fail (make-voxel-direction 1 1 0))))

(define-test palette-backed-block-content
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (chunk (ensure-world-chunk world 0 0 0))
         (column (block-chunk-content chunk))
         (stone (list :stone)))
    (true (equal (array-element-type (block-content-column-indices column))
                 '(unsigned-byte 16)))
    (true (= (length (block-content-column-indices column)) 64))
    (true (= (length (block-content-column-palette column)) 1))
    (true (null (aref (block-content-column-palette column) 0)))
    (setf (world-block-at world 0 0 0) stone
          (world-block-at world 1 0 0) stone)
    (true (= (length (block-content-column-palette column)) 2))
    (true (= (aref (block-content-column-indices column) 0) 1))
    (true (= (aref (block-content-column-indices column) 1) 1))
    (multiple-value-bind (block status) (world-block-at world 1 0 0)
      (true (eq block stone))
      (true (eq status :resident)))
    (multiple-value-bind (block status) (world-block-at world 2 0 0)
      (true (null block))
      (true (eq status :resident)))
    (multiple-value-bind (block status) (world-block-at world 4 0 0)
      (true (null block))
      (true (eq status :absent)))))

(define-test whole-domain-block-storage-is-borrowed-without-row-objects
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 3
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (stone (list :stone))
         (described nil))
    (setf (chunk-block-at-offset chunk 1) stone)
    (true (= (block-chunk-revision chunk) 1))
    (true (= (block-world-revision world) 2))
    (setf (chunk-block-at-offset chunk 1) stone)
    (true (= (block-chunk-revision chunk) 1))
    (true (= (block-world-revision world) 2))
    (true (= (block-chunk-boundary-revision chunk +voxel-negative-y+) 1))
    (true (= (block-chunk-boundary-revision chunk +voxel-negative-z+) 1))
    (with-block-content-storage (domain palette indices) chunk
      (true (eq domain (block-chunk-domain chunk)))
      (true (eq palette
                (block-content-column-palette (block-chunk-content chunk))))
      (true (eq indices
                (block-content-column-indices (block-chunk-content chunk))))
      (true (= (length indices) (chunk-domain-cardinality domain)))
      (true (eq (aref palette (aref indices 1)) stone)))
    (map-chunk-blocks
     (lambda (block local)
       (when block
         ;; Retaining LOCAL is valid in this presentation protocol; the dense
         ;; traversal macro itself instead lends a dynamic-extent value.
         (push local described)))
     chunk)
    (true (= (length described) 1))
    (true (equalp (first described) (make-local-coordinate 1 0 0)))))

(define-test chunk-and-residency-revisions
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (stone (list :stone))
         (chunk (ensure-world-chunk world 0 0 0)))
    (true (= (block-world-residency-revision world) 1))
    (true (= (block-world-revision world) 1))
    (true (eq (ensure-world-chunk world 0 0 0) chunk))
    (true (= (block-world-residency-revision world) 1))
    (true (= (block-world-revision world) 1))
    (true (= (block-chunk-revision chunk) 0))
    (setf (world-block-at world 0 0 0) stone)
    (true (= (block-chunk-revision chunk) 1))
    (true (= (block-world-revision world) 2))
    (setf (world-block-at world 0 0 0) stone)
    (true (= (block-chunk-revision chunk) 1))
    (true (= (block-world-revision world) 2))
    (setf (world-block-at world 0 0 0) nil)
    (true (= (block-chunk-revision chunk) 2))
    (true (= (block-world-revision world) 3))
    ;; A resident chunk is still world-owned: lower-level writes must also
    ;; invalidate world-derived products such as meshes.
    (setf (chunk-block-at chunk 1 0 0) stone)
    (true (= (block-chunk-revision chunk) 3))
    (true (= (block-world-revision world) 4))
    (multiple-value-bind (removed present-p) (remove-world-chunk world 0 0 0)
      (true (eq removed chunk))
      (true present-p))
    (true (= (block-world-residency-revision world) 2))
    (true (= (block-world-revision world) 5))
    (setf (chunk-block-at chunk 2 0 0) stone)
    (true (= (block-chunk-revision chunk) 4))
    (true (= (block-world-revision world) 5))))

(define-test chunk-boundaries-and-world-change-transactions
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
    (true (= (block-world-revision world) 1))
    (true (= (block-world-residency-revision world) 1))
    (true (= (block-chunk-revision chunk) 2))
    (true (= (block-chunk-boundary-revision chunk +voxel-negative-x+) 1))
    (true (= (block-chunk-boundary-revision chunk +voxel-positive-x+) 0))
    (true (= (block-chunk-boundary-revision chunk +voxel-negative-y+) 0))
    (true (= (block-chunk-boundary-revision chunk +voxel-positive-y+) 0))
    (true (= (block-chunk-boundary-revision chunk +voxel-negative-z+) 0))
    (true (= (block-chunk-boundary-revision chunk +voxel-positive-z+) 0))
    ;; A transaction containing only no-op assignments is itself a no-op.
    (with-world-change-transaction (world)
      (setf (chunk-block-at chunk 1 1 1) stone))
    (true (= (block-world-revision world) 1))
    (setf (chunk-block-at chunk 3 3 3) stone)
    (true (= (block-world-revision world) 2))
    (true (= (block-chunk-boundary-revision chunk +voxel-positive-x+) 1))
    (true (= (block-chunk-boundary-revision chunk +voxel-positive-y+) 1))
    (true (= (block-chunk-boundary-revision chunk +voxel-positive-z+) 1))))

(define-test negative-and-cross-chunk-access
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world -1 0 0))
         (right (ensure-world-chunk world 0 0 0))
         (stone (list :stone))
         (dirt (list :dirt)))
    (setf (world-block-at world -1 0 0) stone
          (world-block-at world 0 0 0) dirt)
    (true (eq (chunk-block-at left 3 0 0) stone))
    (true (eq (chunk-block-at right 0 0 0) dirt))
    (multiple-value-bind (block status) (world-block-at world -5 0 0)
      (true (null block))
      (true (eq status :absent)))
    (fail (setf (world-block-at world -5 0 0) stone)
          'chunk-not-resident)))

(define-test domain-identity-and-deterministic-residency
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
      (true (equal x-coordinates '(-1 0 1)))
      (true (every (lambda (domain)
                     (eq (chunk-domain-space domain) (block-world-space world)))
                   domains))
      (true (not (eq (first domains) (second domains)))))))

(define-test chunk-incarnations-survive-eviction-and-storage-transfer-is-owned
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (first (ensure-world-chunk world 0 0 0))
         (transferred (block-chunk-content first)))
    (setf (chunk-block-at-offset first 3) :transferred-stone)
    (let ((palette (block-content-column-palette transferred))
          (indices (block-content-column-indices transferred))
          (source-domain (block-content-column-domain transferred)))
      (remove-world-chunk world 0 0 0)
      (let ((second (install-world-chunk-storage
                     world 0 0 0 transferred)))
        (true (> (block-chunk-incarnation second)
                 (block-chunk-incarnation first)))
        (true (eq (chunk-block-at-offset second 3) :transferred-stone))
        (true (eq (block-content-column-palette (block-chunk-content second))
                  palette))
        (true (eq (block-content-column-indices (block-chunk-content second))
                  indices))
        (true (not (eq source-domain (block-chunk-domain second))))
        (true (eq (block-chunk-domain second)
                  (block-content-column-domain
                   (block-chunk-content second))))))))

(define-test one-world-vocabulary-closes-every-chunk-column
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (stone (list :stone))
         (dirt (list :dirt))
         (first (ensure-world-chunk world 0 0 0))
         (second (ensure-world-chunk world 1 0 0))
         (vocabulary (block-world-vocabulary world)))
    ;; Both chunks are closed by the same vocabulary, so the same block has
    ;; the same index everywhere and palettes are one shared vector.
    (setf (chunk-block-at-offset first 0) stone
          (chunk-block-at-offset second 0) dirt
          (chunk-block-at-offset second 1) stone)
    (true (eq (block-content-column-palette (block-chunk-content first))
              (block-content-column-palette (block-chunk-content second))))
    (true (= 1 (block-vocabulary-offset vocabulary stone nil)))
    (true (= 2 (block-vocabulary-offset vocabulary dirt nil)))
    (true (null (block-vocabulary-offset vocabulary (list :unknown) nil)))
    (true (= 1 (aref (block-content-column-indices (block-chunk-content first)) 0)))
    (true (= 1 (aref (block-content-column-indices (block-chunk-content second)) 1)))
    (true (= 3 (block-vocabulary-cardinality vocabulary)))
    ;; A column produced under another world's vocabulary is translated in
    ;; place when it is installed, and new members are appended, not moved.
    (let* ((other (make-block-world :chunk-width 2
                                    :chunk-height 2
                                    :chunk-depth 2))
           (foreign (ensure-world-chunk other 0 0 0))
           (sand (list :sand)))
      (setf (chunk-block-at-offset foreign 0) sand
            (chunk-block-at-offset foreign 1) stone)
      (true (= 1 (aref (block-content-column-indices (block-chunk-content foreign)) 0)))
      (let ((installed (install-world-chunk-storage
                        world 0 1 0 (block-chunk-content foreign))))
        (true (eq (block-content-column-vocabulary (block-chunk-content installed))
                  vocabulary))
        (true (eq (chunk-block-at-offset installed 0) sand))
        (true (eq (chunk-block-at-offset installed 1) stone))
        (true (= 3 (block-vocabulary-offset vocabulary sand nil)))
        (true (= 1 (block-vocabulary-offset vocabulary stone nil)))
        (true (= 4 (block-vocabulary-cardinality vocabulary)))))))

(define-test resident-lattice-raycast
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
      (true (eq status :hit))
      (true (eq (block-ray-hit-block hit) stone))
      (true (coordinate= (block-ray-hit-coordinate hit)
                         (make-world-coordinate 2 1 1)))
      (true (coordinate= (block-ray-hit-adjacent-coordinate hit)
                         (make-world-coordinate 1 1 1)))
      (true (= (block-ray-hit-distance hit) 1.5d0)))
    (setf (world-block-at world 2 1 1) nil)
    (multiple-value-bind (hit status)
        (raycast-block-world world
                             (make-vec3 0.5d0 1.5d0 1.5d0)
                             (make-vec3 1d0 0d0 0d0)
                             #'identity :max-distance 2d0)
      (true (null hit))
      (true (eq status :miss)))
    (multiple-value-bind (hit status)
        (raycast-block-world world
                             (make-vec3 0.5d0 1.5d0 1.5d0)
                             (make-vec3 1d0 0d0 0d0)
                             #'identity :max-distance 8d0)
      (true (null hit))
      (true (eq status :absent)))
    (fail
     (raycast-block-world world
                          (make-vec3 0d0 0d0 0d0)
                          (make-vec3 0d0 0d0 0d0)
                          #'identity :max-distance 8d0))))

(define-test trusted-site-neighbors-agree-with-checked-window-steps
  ;; DO-CHUNK-SITE-NEIGHBORS validates a site once and steps primitively;
  ;; it must expose exactly the offsets, crossings, and window results of the
  ;; checked DO-CHUNK-WINDOW-NEIGHBORS at every site of a small domain,
  ;; consulting the window only at crossings.  #FGT96H
  (let* ((world (make-block-world :chunk-width 4 :chunk-height 3 :chunk-depth 5))
         (chunk (luvcraft::ensure-world-chunk world 1 -1 2))
         (neighbor (luvcraft::ensure-world-chunk world 2 -1 2))
         (domain (block-chunk-domain chunk)))
    (declare (ignore neighbor))
    (let ((agreements 0))
      (dotimes (offset (chunk-domain-cardinality domain))
        (let ((checked nil) (trusted nil))
          (let ((local (chunk-domain-local-coordinate domain offset)))
            (do-chunk-window-neighbors
                (target destination crossing direction materialization
                 availability world domain local *voxel-face-directions*)
              (values destination)
              (push (list target (and crossing t) direction materialization
                          availability)
                    checked)))
          (do-chunk-site-neighbors
              (target crossing direction materialization availability
               world domain offset *voxel-face-directions*)
            (push (list target (and crossing t) direction materialization
                        availability)
                  trusted))
          (when (equal checked trusted) (incf agreements))))
      (true (= agreements (chunk-domain-cardinality domain))))
    (fail (do-chunk-site-neighbors
              (target crossing direction materialization availability
               world domain 60 *voxel-face-directions*)
            (values target crossing direction materialization
                    availability))
          'error)))
