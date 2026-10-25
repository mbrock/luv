;;;; CPU-only, expanded control and exact coplanar merging experiment.
;;;; Load after LUFT/RENDERER. No GPU-FPS, shader replacement, GPU allocation,
;;;; live-image state, or changes to production defaults. Always use bevels.

(defpackage #:terrain-lod
  (:use #:cl)
  (:local-nicknames (#:r #:luft.render)
                    (#:s #:luft.render.shaders)
                    (#:q #:luv.shader))
  (:export #:build-geometry))
(in-package #:terrain-lod)

(defun milliseconds-since (start)
  (* 1000d0 (/ (- (get-internal-real-time) start)
               internal-time-units-per-second)))

(defun u32-array (contents)
  (make-array (length contents) :element-type '(unsigned-byte 32)
                               :initial-contents contents))

(defun point-cross (a b c)
  (destructuring-bind (ux uy uz) (mapcar #'- b a)
    (destructuring-bind (vx vy vz) (mapcar #'- c a)
      (list (- (* uy vz) (* uz vy)) (- (* uz vx) (* ux vz))
            (- (* ux vy) (* uy vx))))))

(defun point-dot (a b)
  (reduce #'+ (mapcar #'* a b)))

(defun triangulate-boundary (points normal)
  ;; Collinear vertices that survive coordinated protection MUST survive here.
  (let ((result nil))
    (loop while (> (length points) 3) do
      (let ((ear nil) (n (length points)))
        (dotimes (i n)
          (let ((a (nth (mod (1- i) n) points)) (b (nth i points))
                (c (nth (mod (1+ i) n) points)))
            (when (and (plusp (point-dot (point-cross a b c) normal))
                       (notany
                        (lambda (p)
                          (and (not (member p (list a b c) :test #'equal))
                               (>= (point-dot (point-cross a b p) normal) 0)
                               (>= (point-dot (point-cross b c p) normal) 0)
                               (>= (point-dot (point-cross c a p) normal) 0)))
                        points))
              (setf ear i)
              (return))))
        (unless ear (return-from triangulate-boundary nil))
        (push (list (nth (mod (1- ear) n) points) (nth ear points)
                    (nth (mod (1+ ear) n) points)) result)
        (setf points (loop for p in points for i from 0
                          unless (= i ear) collect p))))
    (unless (and (= (length points) 3)
                 (plusp (point-dot (apply #'point-cross points) normal)))
      (return-from triangulate-boundary nil))
    (nreverse (cons points result))))

(defun group-boundaries (triangles normal)
  ;; Fail closed on nonmanifold edges, shared boundary vertices, or holes.
  (let ((edges (make-hash-table :test #'equal))
        (uses (make-hash-table :test #'equal))
        (next (make-hash-table :test #'equal))
        (incoming (make-hash-table :test #'equal)))
    (dolist (tri triangles)
      (loop for a in tri for b in (append (rest tri) (list (first tri))) do
        (let ((edge (list a b)) (reverse (list b a)))
          (when (>= (+ (gethash edge uses 0) (gethash reverse uses 0)) 2)
            (return-from group-boundaries nil))
          (incf (gethash edge uses 0))
          (when (gethash edge edges) (return-from group-boundaries nil))
          (if (gethash reverse edges) (remhash reverse edges)
              (setf (gethash edge edges) t)))))
    (maphash (lambda (edge value)
               (declare (ignore value))
               (destructuring-bind (a b) edge
                 (when (or (gethash a next) (gethash b incoming))
                   (return-from group-boundaries nil))
                 (setf (gethash a next) b (gethash b incoming) t))) edges)
    (let ((result nil))
      (loop while (plusp (hash-table-count next)) do
        (let* ((start (loop for p being the hash-keys of next return p))
               (point start) (boundary nil))
          (loop do (push point boundary)
                   (let ((following (gethash point next)))
                     (unless following (return-from group-boundaries nil))
                     (remhash point next)
                     (setf point following))
                until (equal point start))
          (setf boundary (nreverse boundary))
          (unless (plusp
                   (loop for a in boundary
                         for b in (append (rest boundary) (list (first boundary)))
                         sum (point-dot (point-cross '(0 0 0) a b) normal)))
            (return-from group-boundaries nil))
          (push boundary result)))
      result)))

(defun protect-boundary-corners (boundaries protected edge-counts)
  (dolist (boundary boundaries)
    (loop with n = (length boundary)
          for b in boundary for i from 0
          for a = (nth (mod (1- i) n) boundary)
          for c = (nth (mod (1+ i) n) boundary)
          do (unless (every #'zerop (point-cross a b c))
               (setf (gethash b protected) t))
             (incf (gethash (list b c) edge-counts 0)))))

(defun corner-data (sites appearances lights words normals site vertex)
  "Exact ticks and a conservative equality key for current shader outputs.
Keep material lanes in order (floating addition and first chosen descriptor).
Only a single nonzero contribution may move among zero lanes. Keep packed
light, mask counts, normal, and the selected site-W edge byte, even when edge
shaping is disabled. Byte equality conservatively distinguishes classes 0/3,
although EDGE-CLASS-VALUE currently maps both to zero."
  (let* ((record (aref words (+ 3 (* 4 vertex))))
         (mask (ldb (byte 8 0) record))
         (normal (ldb (byte 16 16) record))
         (appearance (logior (aref appearances (* 2 site))
                             (ash (aref appearances (1+ (* 2 site))) 32)))
         (codes (loop for lane below 8
                      collect (if (logbitp lane mask)
                                  (ldb (byte 8 (* lane 8)) appearance) 0)))
         (axis (cond ((> (abs (aref normals (* 4 normal))) 0.5) 0)
                     ((> (abs (aref normals (1+ (* 4 normal)))) 0.5) 1)
                     (t 2)))
         (edge-byte (ldb (byte 8 (* 8 (1+ axis)))
                         (aref sites (+ 3 (* 4 site)))))
         (point 0)
         (position
           (loop for axis below 3
                 for coordinate = (aref words (+ (* 4 vertex) axis))
                 do (when (> coordinate 11) (incf point (ash 1 axis)))
                 collect (+ (* 8 (aref sites (+ (* 4 site) axis)))
                            (- coordinate 8)))))
    (when (= (count-if #'plusp codes) 1)
      (setf codes (list (find-if #'plusp codes))))
    (values position
            (list normal codes (logcount mask)
                  (logcount (ldb (byte 8 8) record))
                  (ldb (byte 16 (* 16 (mod point 2)))
                       (aref lights (+ (* 4 site) (floor point 2))))
                  edge-byte))))

(defun reference-stream (runs atlas-indices ranges)
  "Expand the production draw order to (local-site, atlas-vertex) per corner."
  (let* ((corners (loop for (star first count) in runs
                        sum (* count (aref (aref ranges star) 1))))
         (out (make-array (* 2 corners) :element-type '(unsigned-byte 32)))
         (cursor 0))
    (dolist (run runs)
      (destructuring-bind (star first count) run
        (let ((range (aref ranges star)))
          (dotimes (instance count)
            (dotimes (index (aref range 1))
              (setf (aref out cursor) (+ first instance)
                    (aref out (1+ cursor))
                    (aref atlas-indices (+ (aref range 0) index)))
              (incf cursor 2))))))
    out))

(defun merge-stream (sites appearances lights words normals reference)
  "Merge one resident only. Every output pair reuses an original pair.
Return an expanded stream and numeric merge counters. Varying-light triangles
are copied literally. Hole/ambiguous groups retain original triangles and
protect their vertices. Unmatched edges retain all endpoints at resident seams."
  (let* ((start (get-internal-real-time))
         (corners (/ (length reference) 2))
         (positions (make-array corners))
         (attributes (make-array corners))
         (groups (make-hash-table :test #'equal))
         (point-refs (make-hash-table :test #'equal))
         (boundaries (make-hash-table :test #'eq))
         (protected (make-hash-table :test #'equal))
         (edge-counts (make-hash-table :test #'equal))
         (out (make-array 0 :element-type '(unsigned-byte 32)
                            :adjustable t :fill-pointer 0))
         (eligible 0) (merged-count 0) (fallback-groups 0))
    (labels ((emit (id)
               (vector-push-extend (aref reference (* 2 id)) out)
               (vector-push-extend (aref reference (1+ (* 2 id))) out))
             (stats (fallback)
               (list :eligible-triangles eligible :group-count (hash-table-count groups)
                     :merged-groups (if fallback 0 merged-count)
                     :fallback-groups fallback-groups
                     :resident-fallbacks (if fallback 1 0)
                     :merge-ms (milliseconds-since start))))
      (dotimes (i corners)
        (setf (values (aref positions i) (aref attributes i))
              (corner-data sites appearances lights words normals
                           (aref reference (* 2 i))
                           (aref reference (1+ (* 2 i))))))
      (loop for offset from 0 below corners by 3
            for ids = (list offset (1+ offset) (+ offset 2))
            for points = (mapcar (lambda (id) (aref positions id)) ids)
            for attrs = (mapcar (lambda (id) (aref attributes id)) ids)
            do (if (and (equal (first attrs) (second attrs))
                        (equal (first attrs) (third attrs)))
                   (let* ((normal (r::star-triangle-normal-key points))
                          (key (list normal (point-dot normal (first points))
                                     (first attrs))))
                     (incf eligible)
                     (push points (gethash key groups))
                     (loop for p in points for id in ids
                           do (setf (gethash (list key p) point-refs) id)))
                   (loop for id in ids for p in points
                         do (setf (gethash p protected) t)
                            (emit id))))
      (maphash
       (lambda (key triangles)
         (let ((loops (group-boundaries triangles (first key))))
           (if loops
               (progn (setf (gethash key boundaries) loops)
                      (protect-boundary-corners loops protected edge-counts))
               (progn
                 (incf fallback-groups)
                 (dolist (tri triangles)
                   (dolist (p tri) (setf (gethash p protected) t)))))))
       groups)
      (maphash
       (lambda (edge count)
         (unless (and (= count 1) (= (gethash (reverse edge) edge-counts 0) 1))
           (dolist (p edge) (setf (gethash p protected) t))))
       edge-counts)
      (maphash
       (lambda (key triangles)
         (let* ((loops (gethash key boundaries))
                (merged
                  (loop for boundary in loops append
                    (let ((result
                            (triangulate-boundary
                             (remove-if-not (lambda (p) (gethash p protected)) boundary)
                             (first key))))
                      ;; Never fall back just one group after its neighbours
                      ;; have removed shared points: that would make T-junctions.
                      (unless result
                        (return-from merge-stream (values (copy-seq reference) (stats t))))
                      result))))
           (when (and merged (> (length merged) (length triangles)))
             (return-from merge-stream (values (copy-seq reference) (stats t))))
           (when (and merged (< (length merged) (length triangles)))
             (incf merged-count))
           (dolist (tri (or merged triangles))
             (dolist (p tri) (emit (gethash (list key p) point-refs))))))
       groups)
      (values (u32-array out) (stats nil)))))

(defun build-geometry (populations)
  "Return independent CPU numeric arrays for the production BEVEL atlas T.
POPULATIONS is a sequence of render-populations, retained in supplied order;
each population's sites/appearances/lights are sorted together by low W byte.
:REFERENCE and :MERGED are NONINDEXED interleaved u32 pairs (global-site-index,
atlas-vertex-index), including every triangle corner. Resident FIRST/COUNT
values are CORNER offsets, not word or byte offsets. Atlas records/normals
are four words/floats each. :RESIDENTS retains population identity; current
render-populations have no bounds accessor, so bounds are deliberately omitted
(the renderer owns potentially fixture-expanded bounds).

:STATS contains numeric totals and :RESIDENTS numeric per-resident plists,
also present as each resident's :STATS. Bytes are actual returned numeric-array
payload (4 * LENGTH), not Lisp object overhead, allocation high-water marks,
GPU minimum sizes or indices. Shared atlas bytes count once in totals only."
  (let* ((start (get-internal-real-time))
         (populations (coerce populations 'list))
         (site-count (loop for p in populations sum (r::render-population-site-count p)))
         (sites (make-array (* 4 site-count) :element-type '(unsigned-byte 32)))
         (appearances (make-array (* 2 site-count) :element-type '(unsigned-byte 32)))
         (lights (make-array (* 4 site-count) :element-type '(unsigned-byte 32)))
         (reference-parts nil) (merged-parts nil) (residents nil)
         (site-first 0) (reference-first 0) (merged-first 0))
    (assert (<= site-count (expt 2 32)))
    (multiple-value-bind (words normals atlas shadow-words shadow-indices ranges)
        (r::star-patch-tables t)
      (declare (ignore shadow-words shadow-indices))
      (let ((atlas-ms (milliseconds-since start)))
        (dolist (population populations)
          (let* ((resident-start (get-internal-real-time))
                 (count (r::render-population-site-count population))
                 (original-sites (r::render-population-instance-words population))
                 (original-app (r::render-population-appearance-words population))
                 (original-light (r::render-population-light-words population)))
            (assert (= (* 4 count) (length original-sites) (length original-light)))
            (assert (= (* 2 count) (length original-app)))
            (multiple-value-bind (local-sites app runs light)
                (r::sort-sites-by-star original-sites original-app original-light)
              (let* ((sort-ms (milliseconds-since resident-start))
                     (reference (reference-stream runs atlas ranges)))
                (multiple-value-bind (merged merge-stats)
                    (merge-stream local-sites app light words normals reference)
                  (let* ((reference-count (/ (length reference) 2))
                         (merged-count (/ (length merged) 2))
                         (stream-bytes (* 40 count))
                         (reference-bytes (* 4 (length reference)))
                         (merged-bytes (* 4 (length merged)))
                         (stats
                           (append
                            (list :site-count count :reference-corners reference-count
                                  :merged-corners merged-count
                                  :reference-triangles (/ reference-count 3)
                                  :merged-triangles (/ merged-count 3)
                                  :sites-bytes (* 16 count) :appearances-bytes (* 8 count)
                                  :lights-bytes (* 16 count) :stream-bytes stream-bytes
                                  :reference-bytes reference-bytes :merged-bytes merged-bytes
                                  :numeric-array-bytes (+ stream-bytes reference-bytes merged-bytes)
                                  :reference-total-bytes (+ stream-bytes reference-bytes)
                                  :merged-total-bytes (+ stream-bytes merged-bytes)
                                  :sort-ms sort-ms)
                            merge-stats)))
                    (replace sites local-sites :start1 (* 4 site-first))
                    (replace appearances app :start1 (* 2 site-first))
                    (replace lights light :start1 (* 4 site-first))
                    (dolist (stream (list reference merged))
                      (loop for i from 0 below (length stream) by 2
                            do (incf (aref stream i) site-first)))
                    (push reference reference-parts)
                    (push merged merged-parts)
                    (setf (getf stats :build-ms) (milliseconds-since resident-start))
                    (push (list :population population
                                :reference-first reference-first :reference-count reference-count
                                :merged-first merged-first :merged-count merged-count
                                :stats stats)
                          residents)
                    (incf site-first count)
                    (incf reference-first reference-count)
                    (incf merged-first merged-count)))))))
        (setf residents (nreverse residents))
        (let* ((reference (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                                (nreverse reference-parts)))
               (merged (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                             (nreverse merged-parts)))
               (resident-stats (mapcar (lambda (r) (getf r :stats)) residents))
               (stats (list :resident-count (length residents) :residents resident-stats
                            :atlas-ms atlas-ms :vertices-bytes (* 4 (length words))
                            :normals-bytes (* 4 (length normals))
                            :shared-bytes (* 4 (+ (length words) (length normals))))))
          (dolist (key '(:site-count :reference-corners :merged-corners :reference-triangles
                         :merged-triangles :sites-bytes :appearances-bytes :lights-bytes
                         :stream-bytes :reference-bytes :merged-bytes :numeric-array-bytes
                         :reference-total-bytes :merged-total-bytes :sort-ms :merge-ms
                         :eligible-triangles :group-count :merged-groups :fallback-groups
                         :resident-fallbacks))
            (setf (getf stats key) (loop for s in resident-stats sum (getf s key))))
          (dolist (key '(:numeric-array-bytes :reference-total-bytes :merged-total-bytes))
            (incf (getf stats key) (getf stats :shared-bytes)))
          (setf (getf stats :build-ms) (milliseconds-since start))
          (list :sites sites :appearances appearances :lights lights
                :vertices words :normals normals :reference reference :merged merged
                :residents residents :stats stats))))))
