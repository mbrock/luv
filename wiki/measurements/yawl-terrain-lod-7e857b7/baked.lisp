;;;; Immutable CPU-predecoded control. Load after GEOMETRY.LISP and LUFT/RENDERER.
;;;; No GPU work, production mutation, or dependency on the benchmark harness.
(in-package #:terrain-lod)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(build-baked baked-vertex-specification)))

(defun baked-code (word field)
  (ldb (apply #'byte (rest (luft.terrain::terrain-descriptor-byte field))) word))

(defun baked-field (word field)
  (/ (float (baked-code word field) 1.0)
     (luft.terrain::terrain-descriptor-divisor field)))

(defun baked-tone (word)
  (mapcar (lambda (field) (baked-field word field)) '(:red :green :blue)))

(defun baked-attribute-rows (attributes descriptors normals)
  "Decode CORNER-DATA's exact key to seven vec4 rows, without vertex light."
  (destructuring-bind (normal-index codes occupied empty packed-light edges) attributes
    (declare (ignore packed-light))
    (let* ((normal (loop for axis below 3 collect (aref normals (+ (* 4 normal-index) axis))))
           (side (cond ((> (third normal) 0.35) 0) ((< (third normal) -0.35) 2) (t 1)))
           (chosen (or (find-if #'plusp codes) 0))
           (base (* 8 chosen))
           (surface (aref descriptors (+ base 3)))
           (pattern (aref descriptors (+ base 4)))
           (accent (aref descriptors (+ base 6)))
           (convexity (max -1.0 (min 1.0 (* 4.0 (- 0.5 (/ (float occupied 1.0)
                                                          (max 1.0 (float (+ occupied empty) 1.0))))))))
           (tone (list 0.0 0.0 0.0))
           (weight 0.0))
      ;; Never sort or collapse repeated material codes: production adds in
      ;; lane order, and chooses the first sampled nonzero descriptor.
      (dolist (code codes)
        (when (plusp code)
          (setf tone (mapcar #'+ tone (baked-tone (aref descriptors (+ (* 8 code) side)))))
          (incf weight 1.0)))
      (append normal (list 0.0)
              (mapcar (lambda (x) (/ x (max 1.0 weight))) tone) (list 0.0)
              (list (float (baked-code surface :relief-code) 1.0)
                    (baked-field surface :strength) (baked-field surface :roughness)
                    (* convexity (baked-field surface :wear))
                    (float (baked-code pattern :pattern-code) 1.0)
                    (float (baked-code pattern :courses) 1.0)
                    (* (/ (luft.terrain::terrain-descriptor-divisor :aspect))
                       (float (baked-code pattern :aspect) 1.0))
                    (baked-field pattern :mortar-width))
              (baked-tone (aref descriptors (+ base 5)))
              (list (baked-field pattern :variance))
              (baked-tone (if (and (= (baked-code surface :relief-code) 1)
                                   (zerop (baked-code pattern :pattern-code)))
                              (aref descriptors base) accent))
              (list (baked-field accent :emission))
              (loop for shift from 0 below 8 by 2
                    collect (case (ldb (byte 2 shift) edges) (1 1.0) (2 -1.0) (t 0.0)))))))

(defun build-baked (geometry descriptors)
  "Bake immutable GEOMETRY with one shared u32 DESCRIPTORS palette.
The caller must verify every population uses that palette. Rebuild after ANY
geometry, appearance, light, edge, normal, descriptor, or decode-ABI change.
There is no dynamic palette update, LOD blending, or cache invalidation here.

Return :REFERENCE-VERTICES/:MERGED-VERTICES, simple u32 arrays of uvec4
(world-tick-X world-tick-Y world-tick-Z attribute-index | packed-light<<16),
and :REFERENCE-INDICES/:MERGED-INDICES, simple u32 arrays with ONE index per
original stream corner, preserving triangle order and winding. Resident
FIRST/COUNT values from GEOMETRY remain valid index ranges; use base vertex 0.
Identical position/attribute/light vertices share within each entire stream.
:ATTRIBUTES is a shared simple f32 array: seven vec4 per attribute, ordered
normal/pad, tone/pad, surface, pattern, mortar, accent, edge-shaping.
:STATS reports counts, build milliseconds, and actual numeric-array bytes.

Ticks must fit u32. Reject positions where float(ticks)/8 differs from the
original shader's float(site)+(float(record)-8)/8 (large-world rounding).
Reject 65536 or more distinct attributes. Attribute equality is bit-exact f32,
independent of light, including signed zero. No input arrays are modified."
  (check-type descriptors (simple-array (unsigned-byte 32) (*)))
  (assert (and (plusp (length descriptors)) (zerop (mod (length descriptors) 8))))
  (let* ((start (get-internal-real-time))
         (sites (getf geometry :sites)) (appearances (getf geometry :appearances))
         (lights (getf geometry :lights)) (words (getf geometry :vertices))
         (normals (getf geometry :normals))
         (attributes (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
         (attribute-keys (make-hash-table :test #'equal))
         (decode-cache (make-hash-table :test #'equal)))
    (labels ((attribute-index (key)
               (let ((decode-key (append (subseq key 0 4) (list (sixth key)))))
                 (or (gethash decode-key decode-cache)
                     (setf (gethash decode-key decode-cache)
                           (let* ((rows (baked-attribute-rows key descriptors normals))
                                  (bits (mapcar #'sb-kernel:single-float-bits rows)))
                             (or (gethash bits attribute-keys)
                                 (let ((index (/ (length attributes) 28)))
                                   (assert (< (1+ index) 65536) () "Baked attributes exceed the 16-bit experiment limit.")
                                   (dolist (x rows) (vector-push-extend x attributes))
                                   (setf (gethash bits attribute-keys) index))))))))
             (bake-stream (stream)
               (assert (zerop (mod (length stream) 6)))
               (let ((vertices (make-array 0 :element-type '(unsigned-byte 32)
                                            :adjustable t :fill-pointer 0))
                     (indices (make-array (/ (length stream) 2) :element-type '(unsigned-byte 32)))
                     (seen (make-hash-table :test #'equal)))
                 (dotimes (corner (length indices))
                   (let ((site (aref stream (* 2 corner)))
                         (vertex (aref stream (1+ (* 2 corner)))))
                     (multiple-value-bind (position key)
                         (corner-data sites appearances lights words normals site vertex)
                       (loop for tick in position for axis from 0 do
                         (assert (typep tick '(unsigned-byte 32)))
                         (assert (= (/ (float tick 1.0) 8.0)
                                    (+ (float (aref sites (+ (* 4 site) axis)) 1.0)
                                       (/ (- (float (aref words (+ (* 4 vertex) axis)) 1.0) 8.0) 8.0)))))
                       (let* ((record (append position (list (logior (attribute-index key)
                                                                    (ash (fifth key) 16)))))
                              (index (or (gethash record seen)
                                         (let ((index (/ (length vertices) 4)))
                                           (assert (typep index '(unsigned-byte 32)))
                                           (dolist (word record) (vector-push-extend word vertices))
                                           (setf (gethash record seen) index)))))
                         (setf (aref indices corner) index)))))
                 (values (u32-array vertices) indices))))
      (multiple-value-bind (reference-vertices reference-indices) (bake-stream (getf geometry :reference))
        (multiple-value-bind (merged-vertices merged-indices) (bake-stream (getf geometry :merged))
          (let* ((attributes (make-array (length attributes) :element-type 'single-float
                                                           :initial-contents attributes))
                 (attribute-bytes (* 4 (length attributes)))
                 (rv (* 4 (length reference-vertices))) (ri (* 4 (length reference-indices)))
                 (mv (* 4 (length merged-vertices))) (mi (* 4 (length merged-indices))))
            (list :reference-vertices reference-vertices :reference-indices reference-indices
                  :merged-vertices merged-vertices :merged-indices merged-indices :attributes attributes
                  :stats (list :attribute-count (/ (length attributes) 28)
                               :reference-vertex-count (/ (length reference-vertices) 4)
                               :merged-vertex-count (/ (length merged-vertices) 4)
                               :reference-index-count (length reference-indices)
                               :merged-index-count (length merged-indices)
                               :reference-vertices-bytes rv :reference-indices-bytes ri
                               :merged-vertices-bytes mv :merged-indices-bytes mi
                               :attributes-bytes attribute-bytes
                               :reference-total-bytes (+ rv ri attribute-bytes)
                               :merged-total-bytes (+ mv mi attribute-bytes)
                               :numeric-array-bytes (+ rv ri mv mi attribute-bytes)
                               :build-ms (milliseconds-since start)))))))))

(defun baked-vertex-specification ()
  "Production vertex contract, but pull predecoded 16-byte indexed vertices.
Bind :BAKED-VERTICES at 0 (uvec4), :BAKED-ATTRIBUTES at 1 (vec4), and the
unchanged :CAMERA-STATE prefix23 at 2. All other terrain vertex resources are
removed. Original camera, jitter, previous clip, shadow, and packed-light
expressions are reused verbatim. This does not replace any production shader."
  (let* ((source (q:shader-object-source-form (s:terrain-vertex-specification)))
         (options (copy-tree (third source)))
         (block (copy-tree (fourth source)))
         (bindings (second block))
         (outputs (cddr block))
         (camera (assoc 's::camera-state (getf options :resources)))
         (tail (member 's::divisor bindings :key #'first))
         (edge (find 's::edge-shaping-output outputs :key #'second)))
    (assert (and (= (length (cdddr source)) 1) (eq (first block) 'let*) edge camera))
    (assert (equal (getf (cddr camera) :members) (s::scene-uniform-prefix 23)))
    (assert (equal (mapcar #'first tail) '(s::divisor s::clip s::previous s::light s::jitter)))
    (labels ((row (index)
               `(q:buffer-element baked-attributes (+ attribute-base (q:uint ,(float index 1.0)))))
             (typed-row (name index &optional xyz)
               (let* ((assignment (find name outputs :key #'third))
                      (interface (assoc (second assignment) (getf options :outputs)))
                      (metadata (cddr interface))
                      (components (getf metadata :components))
                      (quantity (getf metadata :quantity)))
                 (assert (and assignment interface (or components quantity)))
                 (list name
                       `(q:assume-quantity ,(if xyz `(q:swizzle ,(row index) :xyz) (row index))
                         ,@(if components (list :components components)
                               (list :quantity quantity :unit (getf metadata :unit))))))))
      (setf (getf options :resources)
            (list '(baked-vertices :storage-buffer :binding 0 :element :uvec4)
                  '(baked-attributes :storage-buffer :binding 1 :element :vec4) camera)
            (third edge) (row 6))
      (q:parse-shader-specification
       'baked-vertex options
       `((let* ((baked-record (q:buffer-element baked-vertices s::vertex-index))
                (attribute-base (* (ldb (byte 16 0) (q:swizzle baked-record :w)) (q:uint 7.0)))
                (s::world (q:assume-quantity
                           (/ (q:vec3 (q:float (q:swizzle baked-record :x))
                                      (q:float (q:swizzle baked-record :y))
                                      (q:float (q:swizzle baked-record :z))) 8.0)
                           :quantity luft.quantities:world-position :unit luft.quantities:cell))
                ,(typed-row 's::normal 0 t)
                (s::material-tone (q:swizzle ,(row 1) :xyz))
                ,(typed-row 's::material-surface 2)
                ,(typed-row 's::material-pattern 3)
                ,(typed-row 's::material-mortar 4)
                ,(typed-row 's::material-accent 5)
                (s::packed-light (ldb (byte 16 16) (q:swizzle baked-record :w)))
                ,(copy-tree (assoc 's::voxel-light bindings))
                ,@tail)
           ,@outputs))))))
