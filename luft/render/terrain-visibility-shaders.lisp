(in-package #:luft.render.shaders)

;;; Visibility is an exact address into the existing resident/site/atlas
;;; representation, not another mesh. Zero is reserved only in the primitive
;;; attachment: site zero is a perfectly ordinary resident-local site.

(defconstant +terrain-visibility-triangles-per-resident+ 65536)
(defconstant +terrain-visibility-resident-stride+ (* 3 65536))
(defconstant +terrain-visibility-resident-limit+ 21844)

(define-shader-function terrain-visibility-encode (identifier)
  (/ (vec4 (float (ldb (byte 8 0) identifier))
           (float (ldb (byte 8 8) identifier))
           (float (ldb (byte 8 16) identifier))
           (float (ldb (byte 8 24) identifier)))
     255.0))

(define-shader-function terrain-visibility-decode (encoded)
  (let* ((bytes (uvec4 (uint (+ (* (swizzle encoded :x) 255.0) 0.5))
                       (uint (+ (* (swizzle encoded :y) 255.0) 0.5))
                       (uint (+ (* (swizzle encoded :z) 255.0) 0.5))
                       (uint (+ (* (swizzle encoded :w) 255.0) 0.5)))))
    (+ (swizzle bytes :x) (* (swizzle bytes :y) (uint 256.0))
       (* (swizzle bytes :z) (uint 65536.0))
       (* (swizzle bytes :w) (uint 16777216.0)))))

(define-live-shader terrain-visibility-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position :invariant t)
               (site-identifier :uint :location 0 :interpolation :flat)
               (primitive-identifier :uint :location 1 :interpolation :flat))
     :resources ((sites :storage-buffer :binding 0 :element :uvec4)
                 (terrain-vertices :storage-buffer :binding 1 :element :uvec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))
                 (terrain-indices :storage-buffer :binding 9 :element :uint)))
  (let* ((primitive (/ vertex-index (uint 3.0)))
         (corner (- vertex-index (* primitive (uint 3.0))))
         (index (+ (* (ldb (byte 16 0) primitive) (uint 3.0)) corner))
         (site (buffer-element sites instance-index))
         (record (buffer-element terrain-vertices (buffer-element terrain-indices index)))
         (world (star-record-world
                 record (vec3 (float (swizzle site :x)) (float (swizzle site :y))
                              (float (swizzle site :z)))))
         (clip (mesh-view-clip world camera-position camera-right camera-up
                               camera-forward camera-projection
                               (swizzle (representation render-parameters) :z)))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle clip :x) (* (swizzle jitter :x) (swizzle clip :w)))
                      (+ (swizzle clip :y) (* (swizzle jitter :y) (swizzle clip :w)))
                      (swizzle clip :z) (swizzle clip :w)))
    (set-output site-identifier instance-index)
    (set-output primitive-identifier primitive)))

(define-live-shader terrain-visibility-fragment-specification
    (:stage :fragment
     :inputs ((site-identifier :uint :location 0 :interpolation :flat)
              (primitive-identifier :uint :location 1 :interpolation :flat))
     :outputs ((site-output :vec4 :location 0)
               (primitive-output :vec4 :location 1)))
  (let* ()
    (set-output site-output (terrain-visibility-encode site-identifier))
    (set-output primitive-output (terrain-visibility-encode primitive-identifier))))

(define-live-shader terrain-visibility-material-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (resident-identifier :uint :location 0 :interpolation :flat)))
  (let* ((index (float vertex-index))
         (x (if (< index 0.5) -1.0 (if (< index 1.5) 3.0 -1.0)))
         (y (if (< index 1.5) -1.0 3.0)))
    (set-output clip-position (vec4 x y 0.0 1.0))
    (set-output resident-identifier instance-index)))

(define-shader-function terrain-visibility-plane-point
    (pixel inverse-size jitter a plane eye right up forward projection divisor)
  "Intersect a pixel-centre ray with this triangle's extended plane.
Undo the actual raster jitter, but leave the production motion clips unjittered.
The homogeneous divisor is the same MIX as MESH-VIEW-CLIP, not a mode guess."
  (let* ((ndc (- (- (* pixel inverse-size 2.0) (vec2 1.0 1.0)) jitter))
         (offset (- (* right (/ (swizzle ndc :x) (swizzle projection :x)))
                    (* up (/ (swizzle ndc :y) (swizzle projection :y)))))
         (origin (+ eye (* offset (- 1.0 divisor))))
         (direction (+ forward (* offset divisor)))
         (distance (/ (dot plane (- a origin)) (dot plane direction))))
    (+ origin (* direction distance))))

(defun terrain-visibility-world-form (index)
  `(representation
    (star-record-world
     (buffer-element terrain-vertices ,index)
     (vec3 (float (swizzle site :x)) (float (swizzle site :y))
           (float (swizzle site :z))))))

(defun terrain-visibility-light-form (index vertex-bindings)
  ;; The light packing is owned by the forward vertex shader, including its
  ;; heterogeneous quantity boundary. Do not maintain a second decode here.
  (let ((start (position 'lights vertex-bindings :key #'first))
        (end (position 'voxel-light vertex-bindings :key #'first)))
    (assert (and start end (<= start end)))
    `(let* ((record (buffer-element terrain-vertices ,index))
            ,@(subseq vertex-bindings start (1+ end)))
       (representation voxel-light))))

(defun terrain-visibility-binding-slice (bindings result)
  "The ordered LET* dependencies of RESULT, without unrelated shading work."
  (let ((needed (make-hash-table :test #'eq)))
    (labels ((visit (form)
               (cond ((consp form) (mapc #'visit form))
                     ((and (symbolp form) (not (gethash form needed)))
                      (let ((binding (assoc form bindings)))
                        (when binding
                          (setf (gethash form needed) t)
                          (visit (second binding))))))))
      (visit result))
    (remove-if-not (lambda (binding) (gethash (first binding) needed)) bindings)))

(defun terrain-visibility-surface-source (form dx dy hx hy)
  "Specialize only derivative-bearing calls, preserving their exact formulas.

Typed shader functions have closed lexical environments. Inline just the
derivative-bearing call chain as lexical LET*s so the explicit plane gradients
also reach nested host treatments (notably Luvcraft's material character).
Everything else remains an ordinary typed function call. Expand abstractions
now, under the caller's host bindings, rather than recycling a stale AST.
Reject new derivative uses until their same-surface meaning is specified."
  (let ((cache (make-hash-table :test #'eq)))
    (labels
        ((rewrite (source owner)
           (cond
             ((atom source) (values source nil))
             ((member (first source) '(derivative-x derivative-y))
              (let ((x-p (eq (first source) 'derivative-x)))
                (cond
                  ((and (member owner '(material-pixel-footprint relief-normal rounded-cube-normal))
                        (equal (rest source) '(world)))
                   (values (if x-p dx dy) t))
                  ((and (eq owner 'relief-normal) (equal (rest source) '(height)))
                   (values (if x-p hx hy) t))
                  (t (error "No same-surface derivative contract for ~S in ~S" source owner)))))
             (t
              (let* ((operator (first source))
                     (definition (and (symbolp operator)
                                      (shader-function-definition-for operator))))
                (multiple-value-bind (children changed-p) (rewrite-list (rest source) owner)
                  (if definition
                      (let ((entry (or (gethash operator cache)
                                       (setf (gethash operator cache)
                                             (multiple-value-list
                                              (rewrite
                                               (expand-shader-source-form
                                                (first (shader-function-body definition)))
                                               operator))))))
                        (if (second entry)
                            (let ((temporaries (loop for argument in children
                                                    collect (gensym "ARGUMENT-"))))
                              (values
                               `(let* (,@(mapcar #'list temporaries children)
                                       ,@(mapcar #'list (shader-function-parameters definition)
                                                 temporaries))
                                  ,(first entry))
                               t))
                            (values (cons operator children) changed-p)))
                      ;; A binding list is not a call: its first element also
                      ;; needs walking (e.g. the first LET* initializer).
                      (multiple-value-bind (head head-changed-p) (rewrite operator owner)
                        (values (cons head children) (or changed-p head-changed-p)))))))))
         (rewrite-list (forms owner)
           (let ((changed-p nil))
             (values
              (mapcar (lambda (child)
                        (multiple-value-bind (rewritten changed) (rewrite child owner)
                          (when changed (setf changed-p t))
                          rewritten))
                      forms)
              changed-p))))
      (rewrite (expand-shader-source-form form) nil))))

(defun terrain-visibility-material-fragment-specification
    (&optional (fragment (star-fragment-specification)))
  "Shade the winning original triangle, with explicit same-plane quad samples.

Three production height evaluations replace hardware height derivatives: the
current sample and its horizontal and vertical partners, never the diagonal. This
is deliberately not a cheap cross-surface derivative or averaged normal. It
costs more ALU than forward shading per surviving fragment; visibility's gain
must come from eliminating hidden/partial-facet material invocations. Single
sample only: the host retains the visibility depth and uses forward for MSAA."
  (let* ((vertex-source (shader-object-source-form (terrain-vertex-specification)))
         (vertex-options (third vertex-source))
         (vertex-body (fourth vertex-source))
         (vertex-bindings (copy-tree (second vertex-body)))
         (source (shader-object-source-form fragment))
         (options (copy-tree (third source)))
         (inputs (getf options :inputs))
         (outputs (getf vertex-options :outputs))
         (assignments (cddr vertex-body))
         (fragment-body (copy-tree (fourth source)))
         (fragment-bindings (second fragment-body))
         (height-bindings (copy-tree (terrain-visibility-binding-slice fragment-bindings 'height)))
         ;; Uninterned names cannot be captured by a production function's
         ;; parameters or local bindings when the derivative chain is inlined.
         (dx (gensym "PLANE-DX-")) (dy (gensym "PLANE-DY-"))
         (hx (gensym "HEIGHT-DX-")) (hy (gensym "HEIGHT-DY-")))
    (assert (and (eq (first fragment-body) 'let*) (assoc 'height height-bindings)
                 (assoc 'world height-bindings)))
    (setf (second (assoc 'world vertex-bindings))
          '(assume-quantity hit-world :quantity quantities:world-position :unit quantities:cell)
          (second (assoc 'voxel-light vertex-bindings))
          `(assume-quantity
            (+ (* (- 1.0 (+ weight-b weight-c))
                  ,(terrain-visibility-light-form 'index-a (second vertex-body)))
               (* weight-b ,(terrain-visibility-light-form 'index-b (second vertex-body)))
               (* weight-c ,(terrain-visibility-light-form 'index-c (second vertex-body))))
            :components ,(luft.terrain::terrain-varying-components :light))
          (second (assoc 'height fragment-bindings))
          'height-here
          (getf options :inputs)
          '((pixel :vec4 :built-in :frag-coord)
            (resident-identifier :uint :location 0 :interpolation :flat))
          (getf options :resources)
          (append (remove 'camera-state (getf vertex-options :resources) :key #'first)
                  (getf options :resources)
                  '((terrain-indices :storage-buffer :binding 9 :element :uint)
                    (visibility-sites :texture-2d :binding 10 :sample-transfer :identity)
                    (visibility-primitives :texture-2d :binding 11 :sample-transfer :identity))))
    (labels ((plane-point (offset)
               `(terrain-visibility-plane-point
                 (+ quad-origin ,offset)
                 (swizzle (representation inspection-parameters) :zw)
                 (representation (swizzle temporal-parameters :xy))
                 a plane (swizzle (representation camera-position) :xyz)
                 (swizzle (representation camera-right) :xyz)
                 (swizzle (representation camera-up) :xyz)
                 (swizzle (representation camera-forward) :xyz)
                 camera-projection (swizzle (representation render-parameters) :z)))
             (height-at (point horizontal vertical)
               (let ((bindings (copy-tree height-bindings)))
                 (setf (second (assoc 'world bindings)) point)
                 `(let* ((,dx ,horizontal) (,dy ,vertical))
                    ,(terrain-visibility-surface-source
                      `(let* ,bindings height) dx dy hx hy)))))
      (parse-shader-specification
       'terrain-visibility-material-fragment options
       `((let* ((coordinate (uvec2 (uint (swizzle pixel :x)) (uint (swizzle pixel :y))))
                (primitive (terrain-visibility-decode
                            (texel-load visibility-primitives coordinate))))
           ;; Do not read any resident-local buffer until ownership is known.
           (when (or (= primitive (uint 0.0))
                     (/= (ldb (byte 16 16) primitive) resident-identifier))
             (discard))
           (let* ((instance-index (terrain-visibility-decode
                                   (texel-load visibility-sites coordinate)))
                  (base (* (ldb (byte 16 0) primitive) (uint 3.0)))
                  (index-a (buffer-element terrain-indices base))
                  (index-b (buffer-element terrain-indices (+ base (uint 1.0))))
                  (index-c (buffer-element terrain-indices (+ base (uint 2.0))))
                  (site (buffer-element sites instance-index))
                  (a ,(terrain-visibility-world-form 'index-a))
                  (b ,(terrain-visibility-world-form 'index-b))
                  (c ,(terrain-visibility-world-form 'index-c))
                  (ab (- b a)) (ac (- c a))
                  (plane (cross-product ab ac))
                  (quad-origin (+ (* (floor (* (swizzle pixel :xy) 0.5)) 2.0)
                                  (vec2 0.5 0.5)))
                  (right-column (> (- (swizzle pixel :x) (swizzle quad-origin :x)) 0.5))
                  (bottom-row (> (- (swizzle pixel :y) (swizzle quad-origin :y)) 0.5))
                  (point-00 ,(plane-point '(vec2 0.0 0.0)))
                  (point-10 ,(plane-point '(vec2 1.0 0.0)))
                  (point-01 ,(plane-point '(vec2 0.0 1.0)))
                  (point-11 ,(plane-point '(vec2 1.0 1.0)))
                  (dx-top (- point-10 point-00)) (dx-bottom (- point-11 point-01))
                  (dy-left (- point-01 point-00)) (dy-right (- point-11 point-10))
                  (,dx (if bottom-row dx-bottom dx-top))
                  (,dy (if right-column dy-right dy-left))
                  (hit-world (if bottom-row (if right-column point-11 point-01)
                                 (if right-column point-10 point-00)))
                  (ap (- hit-world a))
                  (denominator (dot plane plane))
                  (weight-b (/ (dot (cross-product ap ac) plane) denominator))
                  (weight-c (/ (dot (cross-product ab ap) plane) denominator))
                  (vertex-index index-a)
                  ,@vertex-bindings)
             (let* ,(loop for input in inputs
                          for location = (getf (cddr input) :location)
                          for output = (find location outputs
                                             :key (lambda (out) (getf (cddr out) :location)))
                          for assignment = (find (first output) assignments :key #'second)
                          do (assert assignment)
                          collect (list (first input) (third assignment)))
               (let* ((height-here ,(height-at 'hit-world dx dy))
                      (height-horizontal
                        ,(height-at '(if bottom-row (if right-column point-01 point-11)
                                         (if right-column point-00 point-10))
                                    dx '(if right-column dy-left dy-right)))
                      (height-vertical
                        ,(height-at '(if bottom-row (if right-column point-10 point-00)
                                         (if right-column point-11 point-01))
                                    '(if bottom-row dx-top dx-bottom) dy))
                      (,hx (if right-column (- height-here height-horizontal)
                               (- height-horizontal height-here)))
                      (,hy (if bottom-row (- height-here height-vertical)
                               (- height-vertical height-here))))
                 ,(terrain-visibility-surface-source fragment-body dx dy hx hy))))))))))
