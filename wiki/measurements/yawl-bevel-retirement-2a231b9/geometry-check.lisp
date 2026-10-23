;;;; CPU-only contract oracle for the proposed global-anchor bevel morph.
;;;; From the checkout root:
;;;;   sbcl --noinform --script wiki/measurements/yawl-bevel-retirement-2a231b9/geometry-check.lisp
;;;; No ASDF, renderer, GPU, or durable Lisp image is used. Only the atlas and
;;;; its address transformations are loaded, into this disposable process.
;;;; Closure means an oriented 2-chain with no boundary, NOT manifoldness.

(defpackage #:luft
  (:use #:cl)
  (:export #:star-atlas-owned-triangles #:star-atlas-flat-owned-triangles
           #:star-surface-sites))
(in-package #:luft)
(defconstant +mesh-cell-size+ 8)
(handler-bind ((style-warning #'muffle-warning))
  (load "luft/star-geometry.lisp")
  (load "luft/star-atlas.lisp")
  (load "luft/star-table.lisp"))

(defpackage #:bevel-geometry-check (:use #:cl))
(in-package #:bevel-geometry-check)

(defun translate (point site)
  (mapcar (lambda (c s) (+ c (* 8 s))) point site))

(defun anchor (point site)
  (translate (mapcar #'luft::%flat-face-coordinate point) site))

(defun morph (point site weight)
  (let* ((a (anchor point site))
         (w (funcall weight a)))
    (assert (<= 0 w 1))
    (mapcar (lambda (p c) (+ c (* w (- p c)))) (translate point site) a)))

(defun cross (a b)
  (destructuring-bind (x y z) a
    (destructuring-bind (u v w) b
      (list (- (* y w) (* z v))
            (- (* z u) (* x w))
            (- (* x v) (* y u))))))

(defun degenerate-p (triangle)
  (destructuring-bind (a b c) triangle
    (every #'zerop (cross (mapcar #'- b a) (mapcar #'- c a)))))

(defun triangle-counts (triangles)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (triangle triangles counts)
      (incf (gethash triangle counts 0)))))

(defun same-triangles-p (a b)
  (let ((counts (triangle-counts a)))
    (dolist (triangle b) (decf (gethash triangle counts 0)))
    (loop for count being the hash-values of counts always (zerop count))))

(defun mesh (cells weight)
  "Use flat faces only if ALL original owned triangle anchors have zero weight."
  (let ((triangles nil) (flat-sites 0))
    (maphash
     (lambda (site star)
       (let* ((original (luft:star-atlas-owned-triangles star))
              (flat-p
                (every (lambda (triangle)
                         (every (lambda (p) (zerop (funcall weight (anchor p site))))
                                triangle))
                       original)))
         (when (and flat-p original) (incf flat-sites))
         (dolist (triangle (if flat-p (luft:star-atlas-flat-owned-triangles star) original))
           (push (mapcar (lambda (p)
                           (if flat-p (translate p site) (morph p site weight)))
                         triangle)
                 triangles))))
     (luft:star-surface-sites cells))
    (values (remove-if #'degenerate-p triangles) flat-sites)))

(defun boundary-segments (triangles)
  "Exact directed edges, split at every possible half-tick vertex.
After scaling by two, split by the GCD of integer coordinate deltas. Unlike
an unsplit edge/index comparison, this accepts matching long/short edges and
detects a crack when a T-junction's subdivision no longer lies on its edge."
  (let ((segments (make-hash-table :test #'equal)))
    (dolist (triangle triangles segments)
      (let ((points (mapcar (lambda (p) (mapcar (lambda (c) (* 2 c)) p)) triangle)))
        (assert (every (lambda (p) (every #'integerp p)) points))
        (loop for (a b) on (append points (list (first points))) while b do
          (let* ((delta (mapcar #'- b a))
                 (steps (reduce #'gcd (mapcar #'abs delta))))
            (dotimes (i steps)
              (flet ((at (j)
                       (mapcar (lambda (c d) (+ c (/ (* d j) steps))) a delta)))
                (incf (gethash (list (at i) (at (1+ i))) segments 0))))))))))

(defun check-closed (triangles fixture)
  (let ((segments (boundary-segments triangles)))
    (maphash (lambda (edge count)
               (assert (= count (gethash (reverse edge) segments 0)) ()
                       "Open boundary in ~S: ~S has ~D incidences, reverse has ~D."
                       fixture edge count (gethash (reverse edge) segments 0)))
             segments)))

(defun cells-for-star (star)
  "Every occupancy of the eight cells around the origin, including negatives."
  (let ((cells (make-hash-table :test #'equal)))
    (dotimes (sample 8 cells)
      (when (logbitp sample star)
        (setf (gethash (loop for axis below 3
                            collect (if (logbitp axis sample) 0 -1)) cells) t)))))

(defun hashed-weight (seed)
  "Deterministic, traversal-independent {0, 1/2, 1} weights at global anchors."
  (lambda (a)
    (destructuring-bind (x y z) (mapcar (lambda (c) (/ c 8)) a)
      (/ (mod (logxor (* x 73856093) (* y 19349663) (* z 83492791)
                      (* seed 2654435761))
              3)
         2))))

(defun weight-fields ()
  (append
   (list (cons :zero (constantly 0))
         (cons :one (constantly 1))
         (cons :parity
               (lambda (a) (mod (reduce #'+ a :key (lambda (c) (/ c 8))) 2)))
         (cons :mixed
               (lambda (a)
                 (/ (mod (reduce #'+ (mapcar #'* '(1 2 4)
                                            (mapcar (lambda (c) (/ c 8)) a)))
                         3)
                    2))))
   (loop for seed below 4 collect (cons (list :hashed seed) (hashed-weight seed)))))

;; Endpoint identity is exhaustive over the CURRENT atlas, including singular
;; stars: discard zero-area original primitives, then compare exact oriented
;; triangle multisets, not just matching vertices or edge cancellation.
(dotimes (star 256)
  (let ((original (luft:star-atlas-owned-triangles star)))
    (dolist (triangle original)
      (dolist (p triangle)
        (assert (every (lambda (c a) (<= (abs (- c a)) 1)) p (anchor p '(0 0 0))))))
    (assert
     (same-triangles-p
      (remove-if #'degenerate-p
                 (mapcar (lambda (triangle)
                           (mapcar (lambda (p) (morph p '(0 0 0) (constantly 0))) triangle))
                         original))
      (luft:star-atlas-flat-owned-triangles star)))))
(format t "Zero endpoint: all 256 stars equal flat-owned exact oriented triangles.~%")

;; Structured fields explicitly include zero/nonzero checkerboard corners;
;; hashed fields add traversal-independent half widths. Every closure check
;; runs AFTER eligible flat substitutions and AFTER dropping degenerates.
(let ((cases 0) (flat-sites 0) (mixed-flat-sites 0) (state 7))
  (labels ((check (cells fixture)
             (dolist (field (weight-fields))
               (multiple-value-bind (triangles flats) (mesh cells (cdr field))
                 (check-closed triangles (list fixture (car field)))
                 (incf cases)
                 (incf flat-sites flats)
                 (unless (eq (car field) :zero)
                   (incf mixed-flat-sites flats))))))
    (dotimes (star 256) (check (cells-for-star star) (list :star star)))
    (dotimes (trial 8)
      (let ((cells (make-hash-table :test #'equal)))
        (dotimes (x 5)
          (dotimes (y 5)
            (dotimes (z 5)
              (setf state (ldb (byte 32 0) (+ (* state 1664525) 1013904223)))
              (when (< (mod state 100) 45)
                (setf (gethash (list x y z) cells) t)))))
        (check cells (list :solid trial)))))
  (assert (plusp flat-sites))
  (assert (plusp mixed-flat-sites))
  (format t "Mixed closure: ~D cases passed; ~D nonempty flat-site substitutions (~D outside all-zero fields).~%"
          cases flat-sites mixed-flat-sites))

;; EXPECTED NONMANIFOLD ENDPOINT, not a failed closure test. These diagonal
;; cells produce #x06 at the origin. The zero-width cubical surface has four
;; incident triangles on their common edge: two directed each way. It is a
;; closed oriented boundary but not an edge-manifold surface.
(let* ((cells (cells-for-star #x06))
       (a '(0 0 -8)) (b '(0 0 0))
       (triangles (mesh cells (constantly 0)))
       (incident
         (remove-if-not (lambda (triangle)
                          (and (member a triangle :test #'equal)
                               (member b triangle :test #'equal)))
                        triangles)))
  (assert (= #x06 (gethash '(0 0 0) (luft:star-surface-sites cells))))
  (check-closed triangles :checkerboard-zero-endpoint)
  (assert (= 4 (length incident)))
  (let ((*print-pretty* nil))
    (format t "Expected nonmanifold endpoint: cells ~S; edge ~S -> ~S has 4 incident triangles; boundary remains closed.~%"
            '((0 -1 -1) (-1 0 -1)) a b)))
(format t "Geometry contract oracle: PASS.~%")
