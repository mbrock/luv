;;;; Disposable CPU checks, from checkout root:
;;;; ./env sbcl --script wiki/measurements/yawl-terrain-lod-7e857b7/check.lisp
;;;; No historical batch wrapper, GPU-FPS, GPU device, or live image.
(require :asdf)
(asdf:load-system "luft/renderer")
(load (merge-pathnames "geometry.lisp" *load-truename*))
(in-package #:terrain-lod)

(defun oracle-corner (geometry stream corner)
  ;; Independent of CORNER-DATA and its conservative key. Observe ordered
  ;; material contributions, first chosen descriptor, convexity, normal,
  ;; decoded edge classes, and all four light channels.
  (let* ((sites (getf geometry :sites)) (app (getf geometry :appearances))
         (lights (getf geometry :lights)) (words (getf geometry :vertices))
         (normals (getf geometry :normals))
         (site (aref stream (* 2 corner)))
         (vertex (* 4 (aref stream (1+ (* 2 corner)))))
         (w (aref words (+ vertex 3)))
         (mask (ldb (byte 8 0) w))
         (normal (loop for axis below 3
                       collect (aref normals (+ (* 4 (ash w -16)) axis))))
         (point (loop for axis below 3
                      collect (+ (* 8 (aref sites (+ (* 4 site) axis)))
                                 (- (aref words (+ vertex axis)) 8))))
         (light-index (loop for axis below 3
                            when (> (aref words (+ vertex axis)) 11)
                              sum (expt 2 axis)))
         (packed (ldb (byte 16 (* 16 (mod light-index 2)))
                      (aref lights (+ (* 4 site) (floor light-index 2)))))
         (codes (loop for lane below 8 when (logbitp lane mask)
                      collect (ldb (byte 8 (* 8 (mod lane 4)))
                                   (aref app (+ (* 2 site) (floor lane 4))))))
         (selected (remove 0 codes))
         (axis (or (position-if (lambda (n) (> (abs n) 0.5))
                                (subseq normal 0 2)) 2))
         (edge-word (ldb (byte 8 (+ 8 (* 8 axis)))
                         (aref sites (+ (* 4 site) 3)))))
    (list point
          (list normal selected (or (first selected) 0)
                ;; Raw counts conservatively retain the shader's exact ratio.
                (logcount mask) (logcount (ldb (byte 8 8) w))
                (loop for shift from 0 below 8 by 2
                      collect (case (ldb (byte 2 shift) edge-word)
                                (1 1) (2 -1) (t 0))))
          (loop for shift from 0 below 16 by 4
                collect (ldb (byte 4 shift) packed)))))

(defun cancel-edge (table key a b)
  (let ((forward (list key a b)) (reverse (list key b a)))
    (if (gethash reverse table)
        (when (zerop (decf (gethash reverse table))) (remhash reverse table))
        (incf (gethash forward table 0)))))

(defun surface-signature (geometry stream &optional (first 0) (count (/ (length stream) 2)))
  ;; Oriented primitive tick segments per plane and shader outputs.
  ;; Varying light must retain the literal ordered triangle AND original refs.
  (let ((signature (make-hash-table :test #'equal)))
    (loop for i from first below (+ first count) by 3
          for corners = (loop for j below 3 collect (oracle-corner geometry stream (+ i j)))
          for points = (mapcar #'first corners)
          for normal = (r::star-triangle-normal-key points)
          for base = (list normal (point-dot normal (first points)))
          do (assert (some (complement #'zerop) normal))
             (if (and (equal (rest (first corners)) (rest (second corners)))
                      (equal (rest (first corners)) (rest (third corners))))
                 (let ((key (list base (rest (first corners)))))
                   (loop for a in points for b in (append (rest points) (list (first points)))
                         for delta = (mapcar #'- b a)
                         for steps = (abs (reduce #'gcd delta))
                         for step = (mapcar (lambda (v) (/ v steps)) delta)
                         do (dotimes (j steps)
                              (let ((p (mapcar (lambda (v d) (+ v (* j d))) a step)))
                                (cancel-edge signature key p (mapcar #'+ p step))))))
                 (incf (gethash (list :gradient base corners
                                     (coerce (subseq stream (* 2 i) (* 2 (+ i 3))) 'list))
                               signature 0))))
    signature))

(defun unsplit-boundary (geometry stream &optional (first 0) (count (/ (length stream) 2)))
  ;; Unlike primitive segments this detects altered unmatched resident seams
  ;; and T-junctions left by independently simplifying neighbouring groups.
  (let ((edges (make-hash-table :test #'equal)))
    (loop for i from first below (+ first count) by 3
          for points = (loop for j below 3 collect (first (oracle-corner geometry stream (+ i j))))
          do (loop for a in points for b in (append (rest points) (list (first points)))
                   do (cancel-edge edges nil a b)))
    edges))

(defun test-population (sites appearances lights)
  (r::%make-render-population
   (u32-array sites) (u32-array appearances)
   (make-array 2048 :element-type '(unsigned-byte 32) :initial-element 0)
   (/ (length sites) 4) (u32-array lights)))

(defun check-geometry (populations)
  (let* ((saved (loop for p in populations
                      collect (mapcar #'copy-seq
                                      (list (r::render-population-instance-words p)
                                            (r::render-population-appearance-words p)
                                            (r::render-population-light-words p)))))
         ;; Explicit T atlas must ignore the current production setting.
         (geometry (let ((r::*terrain-bevels-p* nil)) (build-geometry populations)))
         (reference (getf geometry :reference)) (merged (getf geometry :merged))
         (stats (getf geometry :stats))
         (site-first 0) (reference-first 0) (merged-first 0))
    (dolist (key '(:sites :appearances :lights :vertices :reference :merged))
      (assert (typep (getf geometry key) '(simple-array (unsigned-byte 32) (*)))))
    (assert (typep (getf geometry :normals) '(simple-array single-float (*))))
    (assert (= (getf stats :numeric-array-bytes)
               (loop for key in '(:sites :appearances :lights :vertices :normals :reference :merged)
                     sum (* 4 (length (getf geometry key))))))
    (multiple-value-bind (words normals atlas sw si ranges) (r::star-patch-tables t)
      (declare (ignore sw si))
      (assert (equalp words (getf geometry :vertices)))
      (assert (equalp normals (getf geometry :normals)))
      (loop for p in populations for resident in (getf geometry :residents) for original in saved do
        (assert (eq p (getf resident :population)))
        (assert (= reference-first (getf resident :reference-first)))
        (assert (= merged-first (getf resident :merged-first)))
        (assert (equalp original
                        (list (r::render-population-instance-words p)
                              (r::render-population-appearance-words p)
                              (r::render-population-light-words p))))
        (multiple-value-bind (sites app runs light)
            (r::sort-sites-by-star (first original) (second original) (third original))
          (declare (ignore runs))
          (assert (equalp sites (subseq (getf geometry :sites) (* 4 site-first)
                                        (+ (* 4 site-first) (length sites)))))
          (assert (equalp app (subseq (getf geometry :appearances) (* 2 site-first)
                                      (+ (* 2 site-first) (length app)))))
          (assert (equalp light (subseq (getf geometry :lights) (* 4 site-first)
                                        (+ (* 4 site-first) (length light)))))
          ;; Independently walk sorted sites and the production atlas, not
          ;; REFERENCE-STREAM, to prove the expanded control contains EVERY corner.
          (let ((cursor reference-first) (pairs (make-hash-table :test #'equal)))
            (dotimes (site (/ (length sites) 4))
              (let ((range (aref ranges (logand #xff (aref sites (+ (* 4 site) 3))))))
                (dotimes (i (aref range 1))
                  (let ((pair (list (+ site site-first) (aref atlas (+ (aref range 0) i)))))
                    (assert (equal pair (list (aref reference (* 2 cursor))
                                             (aref reference (1+ (* 2 cursor))))))
                    (setf (gethash pair pairs) t)
                    (incf cursor)))))
            (assert (= cursor (+ reference-first (getf resident :reference-count))))
            (loop for corner from merged-first below (+ merged-first (getf resident :merged-count))
                  do (assert (gethash (list (aref merged (* 2 corner))
                                           (aref merged (1+ (* 2 corner)))) pairs))))
          (incf site-first (/ (length sites) 4)))
        (let ((rc (getf resident :reference-count)) (mc (getf resident :merged-count)))
          (assert (and (zerop (mod rc 3)) (zerop (mod mc 3)) (<= mc rc)))
          (assert (equalp (surface-signature geometry reference reference-first rc)
                          (surface-signature geometry merged merged-first mc)))
          (assert (equalp (unsplit-boundary geometry reference reference-first rc)
                          (unsplit-boundary geometry merged merged-first mc)))
          (incf reference-first rc)
          (incf merged-first mc))))
    (assert (= (* 2 reference-first) (length reference)))
    (assert (= (* 2 merged-first) (length merged)))
    geometry))

(defun voxel-population (occupied varied-material varied-light varied-edge)
  (let ((sites nil) (appearances nil) (lights nil))
    (dotimes (z 7)
      (dotimes (y 7)
        (dotimes (x 7)
          (let ((star 0) (appearance 0) (light 0) (edges 0))
            (dotimes (axis 3)
              (setf (ldb (byte 8 (* 8 (1+ axis))) edges)
                    (if varied-edge (mod (+ (* 17 x) (* 29 y) (* 47 z) (* 61 axis)) 256) 0)))
            (dotimes (bit 8)
              (let ((cell (list (+ x (ldb (byte 1 0) bit) -1)
                                (+ y (ldb (byte 1 1) bit) -1)
                                (+ z (ldb (byte 1 2) bit) -1))))
                (when (gethash cell occupied)
                  (setf (ldb (byte 1 bit) star) 1
                        (ldb (byte 8 (* 8 bit)) appearance)
                        (if varied-material (1+ (mod (reduce #'+ cell) 3)) 1)))
                (setf (ldb (byte 16 (* bit 16)) light)
                      (if varied-light (logior #x8000 (* 17 (+ x (* 2 y) (* 3 z) bit))) #xf000))))
            (when (< 0 star 255)
              (setf sites (nconc sites (list (+ x 8) (+ y 9) (+ z 7) (logior star edges)))
                    appearances (nconc appearances
                                       (list (ldb (byte 32 0) appearance)
                                             (ldb (byte 32 32) appearance)))
                    lights (nconc lights (loop for lane below 4
                                              collect (ldb (byte 32 (* 32 lane)) light)))))))))
    (test-population sites appearances lights)))

(defun split-population (population)
  ;; Partition sites, not occupied cells: no duplicated/missing owned patches.
  ;; The checker verifies each resident's unmatched edges independently.
  (loop with source = (r::render-population-instance-words population)
        with cut = (/ (+ (loop for i from 0 below (length source) by 4 minimize (aref source i))
                         (loop for i from 0 below (length source) by 4 maximize (aref source i))) 2)
        for side below 2 collect
    (let ((sites nil) (app nil) (lights nil)
          (source (r::render-population-instance-words population)))
      (dotimes (i (r::render-population-site-count population))
        (when (= side (if (<= (aref source (* 4 i)) cut) 0 1))
          (setf sites (nconc sites (coerce (subseq source (* 4 i) (* 4 (1+ i))) 'list))
                app (nconc app (coerce (subseq (r::render-population-appearance-words population)
                                               (* 2 i) (* 2 (1+ i))) 'list))
                lights (nconc lights (coerce (subseq (r::render-population-light-words population)
                                                     (* 4 i) (* 4 (1+ i))) 'list)))))
      (test-population sites app lights))))

(defun check-edge-selection (words normals)
  ;; Every packed edge byte, for every signed production atlas normal. W's
  ;; three bytes differ; bevel normals must obey X-before-Y-before-Z priority.
  (let ((checks 0) (seen (make-hash-table)))
    (dotimes (vertex (/ (length words) 4))
      (let ((index (ash (aref words (+ (* 4 vertex) 3)) -16)))
        (unless (gethash index seen)
          (setf (gethash index seen) t)
          (dotimes (byte 256)
            (let* ((site-w (logior 1 (ash byte 8) (ash (logxor byte #x55) 16)
                                    (ash (logxor byte #xaa) 24)))
                   (sites (u32-array (list 1 2 3 site-w)))
                   (normal (loop for axis below 3 collect (aref normals (+ (* 4 index) axis))))
                   (axis (or (position-if (lambda (n) (> (abs n) 0.5))
                                          (subseq normal 0 2)) 2)))
              (multiple-value-bind (position attributes)
                  (corner-data sites (u32-array '(0 0)) (u32-array '(0 0 0 0))
                               words normals 0 vertex)
                (declare (ignore position))
                (assert (= (sixth attributes) (ldb (byte 8 (* 8 (1+ axis))) site-w))))
              (incf checks))))))
    checks))

(defun check-material-order (words normals)
  (let ((vertex (loop for v below (/ (length words) 4)
                      when (>= (logcount (logand #xff (aref words (+ (* 4 v) 3)))) 2)
                        return v)))
    (assert vertex)
    (let* ((mask (ldb (byte 8 0) (aref words (+ (* 4 vertex) 3))))
           (lanes (loop for lane below 8 when (logbitp lane mask) collect lane))
           (a (logior (ash 1 (* 8 (first lanes))) (ash 2 (* 8 (second lanes)))))
           (b (logior (ash 2 (* 8 (first lanes))) (ash 1 (* 8 (second lanes))))))
      (flet ((key (appearance)
               (nth-value 1 (corner-data (u32-array '(1 2 3 1))
                                         (u32-array (list (ldb (byte 32 0) appearance)
                                                          (ldb (byte 32 32) appearance)))
                                         (u32-array '(0 0 0 0)) words normals 0 vertex))))
        (assert (not (equal (key a) (key b))))))))

(defun synthetic-geometry (points triangles)
  ;; Minimal same-normal/same-mask records to isolate topology branches that
  ;; the production bevel atlas often separates into different mask groups.
  ;; TRIANGLES contains (site v0 v1 v2); two sites differ only in material.
  (list :sites (u32-array '(0 0 0 1 0 0 0 1))
        :appearances (u32-array '(1 0 2 0)) :lights (u32-array '(0 0 0 0 0 0 0 0))
        :vertices (u32-array (loop for p in points append (append (mapcar (lambda (x) (+ x 8)) p)
                                                                (list #x101))))
        :normals (make-array 4 :element-type 'single-float :initial-contents '(0.0 0.0 1.0 0.0))
        :reference (u32-array (loop for (site . vertices) in triangles
                                   append (loop for v in vertices append (list site v))))))

(defun check-synthetic-merge (geometry)
  (multiple-value-bind (merged stats)
      (merge-stream (getf geometry :sites) (getf geometry :appearances) (getf geometry :lights)
                    (getf geometry :vertices) (getf geometry :normals) (getf geometry :reference))
    (assert (equalp (surface-signature geometry (getf geometry :reference))
                    (surface-signature geometry merged)))
    (assert (equalp (unsplit-boundary geometry (getf geometry :reference))
                    (unsplit-boundary geometry merged)))
    (values merged stats)))

(defun check-topology-controls ()
  (let* ((ring-points (loop for y below 4 append
                       (loop for x below 4 collect (list x y 0))))
         (ring-triangles
           (loop for y below 3 append
             (loop for x below 3 unless (and (= x 1) (= y 1)) append
               (let ((a (+ x (* 4 y))))
                 (list (list 0 a (1+ a) (+ a 5)) (list 0 a (+ a 5) (+ a 4)))))))
         (fallbacks 0))
    (dolist (triangles (list ring-triangles (cons (first ring-triangles) ring-triangles)))
      (let ((geometry (synthetic-geometry ring-points triangles)))
        (multiple-value-bind (merged stats) (check-synthetic-merge geometry)
          (assert (= (length merged) (length (getf geometry :reference))))
          (assert (= 1 (getf stats :fallback-groups)))
          (incf fallbacks (getf stats :fallback-groups)))))
    (let* ((points '((0 0 0) (2 0 0) (2 2 0) (0 2 0) (2 1 0) (4 0 0) (4 2 0)))
           (right '((1 1 5 4) (1 5 6 4) (1 6 2 4)))
           (geometry (synthetic-geometry points (append '((0 0 1 4) (0 0 4 3) (0 4 2 3)) right)))
           (bad (getf (synthetic-geometry points (append '((0 0 1 2) (0 0 2 3)) right)) :reference)))
      ;; Removing the midpoint on JUST one side preserves exact oriented area,
      ;; but creates a T-junction. The unsplit oracle must reject it.
      (assert (equalp (surface-signature geometry (getf geometry :reference))
                      (surface-signature geometry bad)))
      (assert (not (equalp (unsplit-boundary geometry (getf geometry :reference))
                           (unsplit-boundary geometry bad))))
      (let ((merged (check-synthetic-merge geometry)))
        (assert (< (length merged) (length (getf geometry :reference))))))
    fallbacks))

(defun check-edge-omission-controls ()
  ;; A different edge byte across an otherwise mergeable coplanar join must
  ;; retain that join. Exercise each selected site-W byte independently.
  (dotimes (axis 3 3)
    (let* ((points (loop for (x y z) in '((0 0 0) (2 0 0) (2 2 0) (0 2 0)
                                          (2 1 0) (4 0 0) (4 2 0))
                         collect (case axis (0 (list z x y)) (1 (list y z x)) (t (list x y z)))))
           (geometry (synthetic-geometry points '((0 0 1 4) (0 0 4 3) (0 4 2 3)
                                                  (1 1 5 4) (1 5 6 4) (1 6 2 4))))
           (sites (getf geometry :sites)))
      (setf (aref sites 3) (logior 1 (ash #x55 (* 8 (1+ axis))))
            (aref sites 7) (logior 1 (ash #xaa (* 8 (1+ axis))))
            (aref (getf geometry :appearances) 2) 1)
      (fill (getf geometry :normals) 0.0)
      (setf (aref (getf geometry :normals) axis) 1.0)
      (check-synthetic-merge geometry)
      (let ((bad (merge-stream (u32-array '(0 0 0 1 0 0 0 1))
                               (getf geometry :appearances) (getf geometry :lights)
                               (getf geometry :vertices) (getf geometry :normals)
                               (getf geometry :reference))))
        (assert (not (equalp (surface-signature geometry (getf geometry :reference))
                             (surface-signature geometry bad))))))))

(defun meshed-population ()
  ;; Exercise the REAL preparer, including its high-W edge-shaping packing,
  ;; not only hand-made ABI populations. No renderer or GPU is constructed.
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (builder (luft:make-chain-builder domain)))
    (loop for x from 4 to 8 do
      (loop for y from 4 to 8 do
        (loop for z from 3 to 5 unless (and (= x 6) (= y 6)) do
          (luft:chain-builder-add-site builder (luft:make-site domain x y z luft:+cell-extent+ 1)))))
    (handler-bind ((luft:missing-chunk (lambda (condition)
                                        (declare (ignore condition))
                                        (invoke-restart 'luft:treat-as-air))))
      (r::make-render-population
       (list (luft:mesh-star-chunk (luft:finish-chain-builder builder)
                                  (luft:chunk-key-at 4 4) :outside-domain-policy :air))))))

(let ((checks 0) (state (sb-ext:seed-random-state 20261025))
      (edge-negative-controls 0) (reduced 0) (fallbacks 0) (sample nil))
  (check-geometry nil)
  (check-geometry (list (test-population nil nil nil)))
  (incf checks 2)
  (incf fallbacks (check-topology-controls))
  (incf checks 3)
  (incf edge-negative-controls (check-edge-omission-controls))
  (incf checks 3)
  (let ((population (meshed-population)))
    (check-geometry (list population))
    (check-geometry (split-population population))
    (incf checks 2))
  (dolist (mixed '(nil t))
    (let ((sites nil) (app nil) (lights nil))
      ;; Reverse order exercises sorting, with ALL 256 stars including 0/255.
      (loop for star downfrom 255 to 0 do
        (setf sites (nconc sites (list (+ 8 (* 4 star)) 9 7
                                     (logior star (if mixed (ash #xe49339 8) 0))))
              app (nconc app (list (if mixed #x03020001 #x01010101)
                                   (if mixed #x00ff0201 #x01010101)))
              lights (nconc lights
                            (copy-list
                             (if mixed '(#xf123a456 #x789bcd0e #x13572468 #xace0bdf9)
                                 '(#xf000f000 #xf000f000 #xf000f000 #xf000f000))))))
      (let ((geometry (check-geometry (list (test-population sites app lights)))))
        (when mixed
          (format t "~&EDGE-BYTE CHECKS: ~D~%"
                  (check-edge-selection (getf geometry :vertices) (getf geometry :normals)))
          (check-material-order (getf geometry :vertices) (getf geometry :normals))))
      (incf checks)))
  (dotimes (case 12)
    (let ((occupied (make-hash-table :test #'equal)))
      (dotimes (x 6)
        (dotimes (y 5)
          (dotimes (z 4)
            (when (case case
                    (0 t)
                    (1 (not (and (= y 2) (= z 2)))) ; tunnel / planar holes
                    (2 (or (= x 0) (= x 5) (= y 0) (= y 4) (= z 0) (= z 3)))
                    (3 (or (< x 3) (< y 2) (< z 2))) ; concave/T-shaped joins
                    (t (< (random 1.0 state) 0.55)))
              (setf (gethash (list x y z) occupied) t)))))
      (dolist (material '(nil t))
        (dolist (light '(nil t))
          (dolist (edge '(nil t))
            (let* ((population (voxel-population occupied material light edge))
                   (geometry (check-geometry (list population)))
                   (stats (getf geometry :stats)))
              (incf checks)
              (incf reduced (- (getf stats :reference-triangles) (getf stats :merged-triangles)))
              (incf fallbacks (getf stats :fallback-groups))
              (when (and (= case 0) (not material) (not light) (not edge))
                (setf sample stats))
              (when (and edge (not material) (not light))
                ;; Regression control: deliberately forget edge W just during
                ;; grouping, then evaluate the resulting refs against REAL W.
                ;; This must fail the independent oracle for some fixtures.
                (let ((stripped (copy-seq (getf geometry :sites))))
                  (loop for i from 3 below (length stripped) by 4
                        do (setf (aref stripped i) (logand #xff (aref stripped i))))
                  (let ((bad (merge-stream stripped (getf geometry :appearances)
                                           (getf geometry :lights) (getf geometry :vertices)
                                           (getf geometry :normals) (getf geometry :reference))))
                    (unless (equalp (surface-signature geometry (getf geometry :reference))
                                    (surface-signature geometry bad))
                      (incf edge-negative-controls)))))
              (when (< case 4)
                (check-geometry (split-population population))
                (incf checks))))))))
  (assert (plusp reduced))
  (assert (plusp fallbacks))
  (assert (plusp edge-negative-controls))
  (format t "~&SOLID-FIXTURE STATS: ~S~%" sample)
  (format t "~&PASS: ~D geometry comparisons; ~D triangles removed; ~D fallback groups; ~D edge-omission negative controls rejected.~%"
          checks reduced fallbacks edge-negative-controls))
