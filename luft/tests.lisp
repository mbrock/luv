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
        (vary-surface-mesh-bevel-widths
         witness #'width :contract-t-junctions-p contract-t-junctions-p)
      (multiple-value-bind (compiled compiled-census compiled-diagnostics)
          (vary-surface-mesh-bevel-widths-from-stock-masks
           witness stock-masks site-widths
           :contract-t-junctions-p contract-t-junctions-p)
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

(defun %mesh-closed-p (mesh)
  (loop for record being the hash-values of (%mesh-geometric-edge-records mesh)
        always (and (= (car record) 2) (zerop (cdr record)))))

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
          (vary-surface-mesh-bevel-widths
           witness
           (lambda (x y z stocks)
             (declare (ignore stocks))
             (1+ (mod (+ x y z) 4)))
           :contract-t-junctions-p nil)
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

(defun %triangle-counts= (left right)
  (and (= (hash-table-count left) (hash-table-count right))
       (loop for key being the hash-keys of left using (hash-value count)
             always (= count (gethash key right 0)))))

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
              "chunked triangles differ from the whole-world mesh"))))

(defun run-luft-tests (&key (stream *standard-output*))
  "Run the retained topology and replacement manifold-sheet mesh claims."
  (let ((*luft-test-count* 0)
        (*luft-test-section* nil))
    (%test-sites-and-chains)
    (%test-sheet-decomposition)
    (%test-surface-mesh)
    (%test-chunked-meshing)
    (when stream
      (format stream "~&LUFT: ~D checks passed.~%" *luft-test-count*))
    (values t *luft-test-count*)))
