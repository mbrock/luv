;;;; Real terrain, exact visibility followed by pixel-owned material evaluation.
;;;; Disposable experiment; no production shader replacements.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:terrain-lod)
    (defpackage #:terrain-lod
      (:use #:cl)
      (:local-nicknames (#:r #:luft.render) (#:s #:luft.render.shaders)
                        (#:q #:luv.shader)))))
(in-package #:terrain-lod)

(defun reference-world (reference)
  `(let* ((ref ,reference)
          (record (q:buffer-element s::terrain-vertices (q:swizzle ref :y)))
          (site (q:buffer-element s::sites (q:swizzle ref :x))))
     (q:representation
      (s::star-record-world record
                           (q:vec3 (q:float (q:swizzle site :x))
                                   (q:float (q:swizzle site :y))
                                   (q:float (q:swizzle site :z)))))))

(defun reference-light (reference)
  ;; Reuse the actual production decode, not a second hand-maintained ABI.
  (let* ((source (q:shader-object-source-form (s:terrain-vertex-specification)))
         (bindings (second (fourth source)))
         (first (position 's::lights bindings :key #'first))
         (last (position 's::voxel-light bindings :key #'first)))
    (assert (and first last))
    `(let* ((ref ,reference)
            (s::instance-index (q:swizzle ref :x))
            (s::record (q:buffer-element s::terrain-vertices (q:swizzle ref :y)))
            ,@(subseq bindings first (1+ last)))
       (q:representation s::voxel-light))))

(defun direct-vertex-specification ()
  (let* ((source (q:shader-object-source-form (s:terrain-vertex-specification)))
         (options (copy-tree (third source)))
         (body (copy-tree (cdddr source))))
    (setf body (subst '(q:swizzle reference :x) 's::instance-index
                     (subst '(q:swizzle reference :y) 's::vertex-index body)))
    (push '(reference (q:buffer-element references s::vertex-index))
          (second (first body)))
    (setf (getf options :resources)
          (append (getf options :resources)
                  '((references :storage-buffer :binding 9 :element :uvec2))))
    (q:parse-shader-specification 'direct-vertex options body)))

(q:define-shader visibility-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((position :vec4 :built-in :position)
               (identifier :uint :location 0 :interpolation :flat))
     :resources ((s::sites :storage-buffer :binding 0 :element :uvec4)
                 (s::terrain-vertices :storage-buffer :binding 1 :element :uvec4)
                 (s::camera-state :uniform-block :binding 2
                  :members #.(s::scene-uniform-prefix 23))
                 (references :storage-buffer :binding 9 :element :uvec2)))
  (let* ((reference (q:buffer-element references vertex-index))
         (site (q:buffer-element s::sites (q:swizzle reference :x)))
         (record (q:buffer-element s::terrain-vertices (q:swizzle reference :y)))
         (world (s::star-record-world
                 record (q:vec3 (q:float (q:swizzle site :x))
                                (q:float (q:swizzle site :y))
                                (q:float (q:swizzle site :z)))))
         (clip (s::mesh-view-clip world s::camera-position s::camera-right s::camera-up
                                 s::camera-forward s::camera-projection
                                 (q:swizzle (q:representation s::render-parameters) :z)))
         (jitter (q:representation (q:swizzle s::temporal-parameters :xy))))
    (q:set-output position
                  (q:vec4 (+ (q:swizzle clip :x) (* (q:swizzle jitter :x) (q:swizzle clip :w)))
                          (+ (q:swizzle clip :y) (* (q:swizzle jitter :y) (q:swizzle clip :w)))
                          (q:swizzle clip :z) (q:swizzle clip :w)))
    (q:set-output identifier (+ (/ vertex-index (q:uint 3.0)) (q:uint 1.0)))))

(q:define-shader visibility-fragment-specification
    (:stage :fragment
     :inputs ((identifier :uint :location 0 :interpolation :flat))
     :outputs ((color :vec4 :location 0)))
  ;; RGBA8 identity transfer stores a full uint without float-ID rounding.
  (q:set-output color
                (/ (q:vec4 (q:float (ldb (byte 8 0) identifier))
                           (q:float (ldb (byte 8 8) identifier))
                           (q:float (ldb (byte 8 16) identifier))
                           (q:float (ldb (byte 8 24) identifier)))
                   255.0)))

(defun deferred-fragment-specification (fragment)
  "Reconstruct production fragment inputs at the winning triangle's sample.
The visibility buffer retains exact geometry. Only covered pixels execute
the full material shader, rather than partial quads of every tiny facet.
Derivative quads now span adjacent surfaces: this is a measured difference,
not a claim of identical procedural filtering. Temporal rendering is disabled."
  (let* ((vertex-source (q:shader-object-source-form (s:terrain-vertex-specification)))
         (vertex-options (third vertex-source))
         (vertex-body (fourth vertex-source))
         (bindings (copy-tree (second vertex-body)))
         (source (q:shader-object-source-form fragment))
         (options (copy-tree (third source)))
         (inputs (getf options :inputs))
         (outputs (getf vertex-options :outputs))
         (assignments (cddr vertex-body)))
    (setf (second (assoc 's::world bindings))
          '(q:assume-quantity hit-world :quantity luft.quantities:world-position
                                       :unit luft.quantities:cell))
    (setf (second (assoc 's::voxel-light bindings))
          `(q:assume-quantity
            (+ (* (- 1.0 (+ weight-b weight-c)) ,(reference-light 'ref-a))
               (* weight-b ,(reference-light 'ref-b))
               (* weight-c ,(reference-light 'ref-c)))
            :components ,(luft.terrain::terrain-varying-components :light)))
    (setf (getf options :inputs) '((pixel :vec4 :built-in :frag-coord))
          (getf options :resources)
          (append (remove 's::camera-state (getf vertex-options :resources) :key #'first)
                  (getf options :resources)
                  '((references :storage-buffer :binding 9 :element :uvec2)
                    (visibility-ids :texture-2d :binding 10 :sample-transfer :identity))))
    (q:parse-shader-specification
     'deferred-fragment options
     `((let* ((encoded (q:texel-load visibility-ids
                                    (q:uvec2 (q:uint (q:swizzle pixel :x))
                                             (q:uint (q:swizzle pixel :y)))))
              (bytes (q:uvec4 (q:uint (+ (* (q:swizzle encoded :x) 255.0) 0.5))
                              (q:uint (+ (* (q:swizzle encoded :y) 255.0) 0.5))
                              (q:uint (+ (* (q:swizzle encoded :z) 255.0) 0.5))
                              (q:uint (+ (* (q:swizzle encoded :w) 255.0) 0.5))))
              (id (+ (q:swizzle bytes :x) (* (q:swizzle bytes :y) (q:uint 256.0))
                     (* (q:swizzle bytes :z) (q:uint 65536.0))
                     (* (q:swizzle bytes :w) (q:uint 16777216.0)))))
         (when (= id (q:uint 0.0)) (q:discard))
         (let* ((base (* (- id (q:uint 1.0)) (q:uint 3.0)))
                (ref-a (q:buffer-element references base))
                (ref-b (q:buffer-element references (+ base (q:uint 1.0))))
                (ref-c (q:buffer-element references (+ base (q:uint 2.0))))
                (a ,(reference-world 'ref-a))
                (b ,(reference-world 'ref-b))
                (c ,(reference-world 'ref-c))
                (eye (q:swizzle (q:representation s::camera-position) :xyz))
                (ndc (- (* (q:swizzle pixel :xy)
                           (q:swizzle (q:representation s::inspection-parameters) :zw) 2.0)
                        (q:vec2 1.0 1.0)))
                (offset (+ (* (/ (q:swizzle ndc :x) (q:swizzle s::camera-projection :x))
                              (q:swizzle (q:representation s::camera-right) :xyz))
                           (* (/ (- (q:swizzle ndc :y)) (q:swizzle s::camera-projection :y))
                              (q:swizzle (q:representation s::camera-up) :xyz))))
                (perspective (> (q:swizzle (q:representation s::render-parameters) :z) 0.5))
                (origin (if perspective eye (+ eye offset)))
                (direction (+ (q:swizzle (q:representation s::camera-forward) :xyz)
                              (if perspective offset (q:vec3 0.0 0.0 0.0))))
                (ab (- b a)) (ac (- c a))
                (normal (s::cross-product ab ac))
                (distance (/ (q:dot normal (- a origin)) (q:dot normal direction)))
                (hit-world (+ origin (* distance direction)))
                (ap (- hit-world a))
                (d00 (q:dot ab ab)) (d01 (q:dot ab ac)) (d11 (q:dot ac ac))
                (d20 (q:dot ap ab)) (d21 (q:dot ap ac))
                (denominator (- (* d00 d11) (* d01 d01)))
                (weight-b (/ (- (* d11 d20) (* d01 d21)) denominator))
                (weight-c (/ (- (* d00 d21) (* d01 d20)) denominator))
                (s::instance-index (q:swizzle ref-a :x))
                (s::vertex-index (q:swizzle ref-a :y))
                ,@bindings)
           (let* ,(loop for input in inputs
                        for location = (getf (cddr input) :location)
                        for output = (find location outputs
                                           :key (lambda (out) (getf (cddr out) :location)))
                        for assignment = (find (first output) assignments :key #'second)
                        do (assert assignment)
                        collect (list (first input) (third assignment)))
             ,@(cdddr source))))))))
