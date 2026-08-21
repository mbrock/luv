;;; The face-record vertex stage: LUFT:REALIZE-FACE-PATCH on the GPU.
;;;
;;; The host uploads the dense face-record array exactly as
;;; LUFT:MATERIALIZE-SURFACE authors it -- four u32 words per exposed face:
;;; the decorated oriented site in two words, the shape word, and a
;;; reserved zero -- plus the permanent 54-entry positive winding template.
;;; A draw of 54 * FACE-COUNT vertices is issued.  Each invocation finds its
;;; face and its slot in the template, and realizes one of the sixteen patch
;;; points from the face site, the shape word, and the chamfer width alone.
;;;
;;; Nothing else is bound.  No occupancy, no per-vertex classification, no
;;; star gathers: the CPU authored every discrete geometric decision once,
;;; into the shape word, and the vertex stage only decodes it.

(in-package #:luft.render.shaders)

(defconstant +frame-binding+ 0)
(defconstant +faces-binding+ 1)
(defconstant +template-binding+ 2)

(defconstant +patch-vertices-per-face+ 54
  "Eighteen triangles, fifty-four template indices, per face patch.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *frame-uniform-members*
    '((camera-vector :vec4)     ; camera position, w unused
      (right-vector :vec4)      ; camera basis
      (up-vector :vec4)
      (forward-vector :vec4)
      (projection-vector :vec4) ; x scale, y scale, z scale, z offset
      (sun-vector :vec4)        ; direction toward the sun, w ambient
      (shape-vector :vec4))))   ; chamfer width, exposure, domain x/y period

(define-shader-function view-clip (point camera right up forward projection)
  "Homogeneous clip position of the world POINT.

Clip Y is framebuffer-oriented like every luv camera graph; the Metal
lowering owns the target's flip."
  (let* ((relative (- point camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward)))
    (vec4 (* view-x (swizzle projection :x))
          (- (* view-y (swizzle projection :y)))
          (+ (* view-z (swizzle projection :z)) (swizzle projection :w))
          view-z)))

(define-shader-function grid-lambda (index w)
  "The local grid coordinate of line INDEX (a float 0..3): 0, w, 1-w, 1."
  (if (< index 0.5)
      0.0
      (if (< index 1.5)
          w
          (if (< index 2.5) (- 1.0 w) 1.0))))

(define-shader-function boundary-line-p (index)
  "One when grid line INDEX (a float 0..3) is a patch boundary line."
  (if (< index 0.5) 1.0 (if (> index 2.5) 1.0 0.0)))

(define-shader-function cross3 (a b)
  (vec3 (- (* (swizzle a :y) (swizzle b :z)) (* (swizzle a :z) (swizzle b :y)))
        (- (* (swizzle a :z) (swizzle b :x)) (* (swizzle a :x) (swizzle b :z)))
        (- (* (swizzle a :x) (swizzle b :y)) (* (swizzle a :y) (swizzle b :x)))))

;;; Field positions inside the packed site, split across the two record
;;; words.  Site bits 0..31 live in word 0 and bits 32..59 in word 1:
;;;
;;;     word 0:  extent 0..2, polarity 3, X 4..27, Y low nibble 28..31
;;;     word 1:  Y high twenty bits 0..19, Z 20..27, stock nibble 28..31
;;;
;;; The wire record is two u32 words precisely so this stage never needs a
;;; 64-bit integer.

(define-shader patch-vertex-shader
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (faces :storage-buffer :binding #.+faces-binding+
                        :element :uint)
                 (winding :storage-buffer :binding #.+template-binding+
                           :element :uint))
     :outputs ((position :vec4 :built-in :position)
               (world :vec3 :location 0)
               (face-normal :vec3 :location 1)
               (stock-lane :float :location 2)))
  (let* ((face-index (/ vertex-index (uint 54.0)))
         (slot (mod vertex-index (uint 54.0)))
         ;; The permanent topology: template slot -> local point 0..15.
         (local (buffer-element winding slot))
         (i (float (/ local (uint 4.0))))
         (j (float (mod local (uint 4.0))))
         ;; The face record.
         (base (* face-index (uint 4.0)))
         (word0 (buffer-element faces base))
         (word1 (buffer-element faces (+ base (uint 1.0))))
         (shape (buffer-element faces (+ base (uint 2.0))))
         ;; The decorated oriented site.
         (extent (ldb (byte 3 0) word0))
         (negative-p (= (ldb (byte 1 3) word0) (uint 1.0)))
         (anchor (vec3 (float (ldb (byte 24 4) word0))
                       (+ (float (ldb (byte 4 28) word0))
                          (* 16.0 (float (ldb (byte 20 0) word1))))
                       (float (ldb (byte 8 20) word1))))
         (stock (float (ldb (byte 4 28) word1)))
         ;; The face basis: tangents in increasing axis order, canonical
         ;; normal n = u x v, oriented by the site's polarity.
         (yz-p (= extent (uint 6.0)))
         (xz-p (= extent (uint 5.0)))
         (u-axis (if yz-p (vec3 0.0 1.0 0.0) (vec3 1.0 0.0 0.0)))
         (v-axis (if xz-p
                     (vec3 0.0 0.0 1.0)
                     (if yz-p (vec3 0.0 0.0 1.0) (vec3 0.0 1.0 0.0))))
         (canonical (if yz-p
                        (vec3 1.0 0.0 0.0)
                        (if xz-p (vec3 0.0 -1.0 0.0) (vec3 0.0 0.0 1.0))))
         (polarity (if negative-p -1.0 1.0))
         (outward (* canonical polarity))
         ;; The flat local grid point.
         (w (swizzle shape-vector :x))
         (lu (grid-lambda i w))
         (lv (grid-lambda j w))
         (flat (+ anchor (+ (* u-axis lu) (* v-axis lv))))
         ;; Which kind of point this is: both indices extreme is a corner,
         ;; one is an edge point, neither is interior.
         (u-boundary (boundary-line-p i))
         (v-boundary (boundary-line-p j))
         (corner-p (> (* u-boundary v-boundary) 0.5))
         (interior-p (< (+ u-boundary v-boundary) 0.5))
         ;; The edge decode: the two-bit class at this incidence, the
         ;; face's outward normal n, and the outward in-plane tangent t
         ;; give q as balanced -> 0, convex -> -n-t, concave -> +n-t.
         (edge-code (if (> u-boundary 0.5)
                        (if (< i 0.5)
                            (ldb (byte 2 0) shape)     ; u-low
                            (ldb (byte 2 2) shape))    ; u-high
                        (if (< j 0.5)
                            (ldb (byte 2 4) shape)     ; v-low
                            (ldb (byte 2 6) shape))))  ; v-high
         (tangent (if (> u-boundary 0.5)
                      (if (< i 0.5) (- u-axis) u-axis)
                      (if (< j 0.5) (- v-axis) v-axis)))
         (edge-q (if (= edge-code (uint 1.0))
                     (- (- outward) tangent)
                     (if (= edge-code (uint 2.0))
                         (- outward tangent)
                         (vec3 0.0 0.0 0.0))))
         (edge-displacement (* edge-q (* w 0.5)))
         ;; The corner decode: six bits at this corner's field, a ternary
         ;; direction code and the two-thirds centroid-reach flag.
         (corner-code (if (> i 2.5)
                          (if (> j 2.5)
                              (ldb (byte 6 26) shape)  ; (u-high, v-high)
                              (ldb (byte 6 20) shape)) ; (u-high, v-low)
                          (if (> j 2.5)
                              (ldb (byte 6 14) shape)  ; (u-low, v-high)
                              (ldb (byte 6 8) shape)))); (u-low, v-low)
         (direction (ldb (byte 5 0) corner-code))
         (two-thirds-p (= (ldb (byte 1 5) corner-code) (uint 1.0)))
         (corner-q (vec3 (- (float (mod direction (uint 3.0))) 1.0)
                         (- (float (mod (/ direction (uint 3.0)) (uint 3.0)))
                            1.0)
                         (- (float (/ direction (uint 9.0))) 1.0)))
         (reach (if two-thirds-p #.(float 2/3 1.0) 0.5))
         (corner-displacement (* corner-q (* w reach)))
         (displacement (if interior-p
                           (vec3 0.0 0.0 0.0)
                           (if corner-p
                               corner-displacement
                               edge-displacement)))
         (point (+ flat displacement))
         (clip (view-clip point (swizzle camera-vector :xyz)
                          (swizzle right-vector :xyz)
                          (swizzle up-vector :xyz)
                          (swizzle forward-vector :xyz)
                          projection-vector)))
    (set-output position clip)
    (set-output world point)
    (set-output face-normal outward)
    (set-output stock-lane stock)))

;;; The fragment stage: faceted normals from screen derivatives, oriented by
;;; the face's outward normal, under one sun and a flat ambient.

(define-shader patch-fragment-shader
    (:stage :fragment
     :inputs ((world :vec3 :location 0)
              (face-normal :vec3 :location 1)
              (stock-lane :float :location 2))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*))
     :outputs ((color :vec4 :location 0)))
  (let* ((facet (cross3 (derivative-x world) (derivative-y world)))
         (unit (normalize facet))
         ;; Screen derivatives fix the facet plane but not its side; the
         ;; face's outward normal does.
         (normal (* unit (signum (dot unit face-normal))))
         (sun (swizzle sun-vector :xyz))
         (ambient (swizzle sun-vector :w))
         (diffuse (max (dot normal sun) 0.0))
         ;; A quiet ramp of stock tones, warm to cool.
         (tint (clamp (* stock-lane #.(float 1/15 1.0)) 0.0 1.0))
         (albedo (mix (vec3 0.80 0.76 0.70) (vec3 0.42 0.52 0.72) tint))
         ;; A faint sky tilt so the vertical axis reads without a sun.
         (sky-fill (* 0.18 (max (swizzle normal :z) 0.0)))
         (exposure (swizzle shape-vector :y))
         (light (* (+ ambient (+ diffuse sky-fill)) exposure)))
    (set-output color (vec4 (* albedo light) 1.0))))
