;;; The surface of a solid world, drawn as packed sites.
;;;
;;; The host uploads two arrays and one small uniform block:
;;;
;;;   terms   (unsigned-byte 64)  the surface chain, one packed term per face
;;;   bricks  vec4                a bounding sphere per +BRICK-SIZE+ terms
;;;   frame   uniform block       camera basis, projection, sun, sky
;;;
;;; The task stage owns one brick per workgroup and decides, uniformly, whether
;;; the brick's sphere meets the view frustum; if so it emits one mesh
;;; workgroup and hands it the brick index.  Each mesh lane then unpacks one
;;; term straight from the u64 array: anchor, extent, and sign.  The extent
;;; names the face's two spanning axes in canonical order, so their cross
;;; product is the canonical orientation, and the chain sign times that
;;; orientation is the outward normal.  A lane whose face is absent, or which
;;; faces away from the camera, collapses its quad to a point.  #VAABY9

(in-package #:luft.render.shaders)

(defconstant +brick-size+ 64
  "Terms per task workgroup and faces per mesh workgroup: 256 vertices.")
(defconstant +frame-binding+ 0)
(defconstant +terms-binding+ 1)
(defconstant +bricks-binding+ 2)

;;; Every stage declares the same frame block at the same binding: identical
;;; member order and offsets are the ABI, written once and spliced at read
;;; time.  The members are deliberately unannotated raw lanes.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *frame-uniform-members*
    '((camera-vector :vec4)      ; camera position, w unused
      (right-vector :vec4)       ; camera basis
      (up-vector :vec4)
      (forward-vector :vec4)
      (projection-vector :vec4)  ; x scale, y scale, z scale, z offset
      (sun-vector :vec4)         ; direction toward the sun, ambient light
      (sky-vector :vec4))))      ; sky colour, fog distance

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

(define-task-payload surface-brick-payload
  (brick-index :uint))

(define-shader surface-task-shader
    (:stage :task
     :workgroup-size (1 1 1)
     :payload surface-brick-payload
     :inputs ((lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (bricks :storage-buffer :binding #.+bricks-binding+
                         :element :vec4)))
  (let* ((brick (swizzle group :x))
         (sphere (buffer-element bricks brick))
         (center (swizzle sphere :xyz))
         (radius (swizzle sphere :w))
         (relative (- center (swizzle camera-vector :xyz)))
         (view-x (dot relative (swizzle right-vector :xyz)))
         (view-y (dot relative (swizzle up-vector :xyz)))
         (view-z (dot relative (swizzle forward-vector :xyz)))
         ;; A conservative frustum test: the sphere must reach past the eye
         ;; plane and lie within the widening view pyramid at its far reach.
         (reach (+ view-z radius))
         (x-limit (+ (/ reach (swizzle projection-vector :x)) radius))
         (y-limit (+ (/ reach (swizzle projection-vector :y)) radius))
         (mesh-count
           (if (> reach 0.0)
               (if (< (abs view-x) x-limit)
                   (if (< (abs view-y) y-limit) (uint 1.0) (uint 0.0))
                   (uint 0.0))
               (uint 0.0))))
    (set-payload brick-index brick)
    (emit-mesh-workgroups (uvec3 mesh-count (uint 1.0) (uint 1.0)))))

(define-shader surface-mesh-shader
    (:stage :mesh
     :workgroup-size (#.+brick-size+ 1 1)
     :payload surface-brick-payload
     :inputs ((lane :uint :built-in :local-invocation-index))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (terms :storage-buffer :binding #.+terms-binding+
                        :element :uint64))
     :mesh-output
     (:topology :triangles
      :max-vertices #.(* 4 +brick-size+)
      :max-primitives #.(* 2 +brick-size+)
      :vertex ((position :vec4 :built-in :position)
               (normal :vec3 :location 0)
               (world :vec3 :location 1)
               (uv :vec2 :location 2))))
  (let* ((term (buffer-element
                terms (+ (* brick-index (uint +brick-size+)) lane)))
         ;; The packed site: extent in the low bits, then X, Y, Z anchors;
         ;; the chain's sign rides above the site in bit 60.
         (extent (uint (ldb (byte luft:+extent-bits+ 0) term)))
         (anchor
           (vec3 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                          luft:+x-shift+)
                                    term)))
                 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                          luft:+y-shift+)
                                    term)))
                 (float (uint (ldb (byte luft:+vertical-coordinate-bits+
                                          luft:+z-shift+)
                                    term)))))
         (negative-p (= (uint (ldb (byte 1 luft:+term-sign-bit+) term))
                        (uint 1.0)))
         (present-p (> extent (uint 0.0)))
         (yz-face-p (= extent (uint luft:+yz-face-extent+)))
         (xz-face-p (= extent (uint luft:+xz-face-extent+)))
         (xy-face-p (= extent (uint luft:+xy-face-extent+)))
         ;; The two spanning axes in canonical X<Y<Z order, and their cross
         ;; product: the face's canonical orientation, which is also the
         ;; incidence sign convention of the boundary operator.
         (edge-a (if yz-face-p (vec3 0.0 1.0 0.0) (vec3 1.0 0.0 0.0)))
         (edge-b (if xy-face-p (vec3 0.0 1.0 0.0) (vec3 0.0 0.0 1.0)))
         (canonical (if yz-face-p
                        (vec3 1.0 0.0 0.0)
                        (if xz-face-p (vec3 0.0 -1.0 0.0) (vec3 0.0 0.0 1.0))))
         (orientation (if negative-p -1.0 1.0))
         (normal (* canonical orientation))
         (camera (swizzle camera-vector :xyz))
         (facing-p (> (dot normal (- camera anchor)) 0.0))
         ;; Absent or back-facing lanes emit a point instead of a quad.
         (scale (if present-p (if facing-p 1.0 0.0) 0.0))
         (span-a (* edge-a scale))
         (span-b (* edge-b scale))
         (corner-0 anchor)
         (corner-1 (+ anchor span-a))
         (corner-2 (+ corner-1 span-b))
         (corner-3 (+ anchor span-b))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (clip-0 (view-clip corner-0 camera right up forward projection-vector))
         (clip-1 (view-clip corner-1 camera right up forward projection-vector))
         (clip-2 (view-clip corner-2 camera right up forward projection-vector))
         (clip-3 (view-clip corner-3 camera right up forward projection-vector))
         (vertex-0 (* lane (uint 4.0)))
         (vertex-1 (+ vertex-0 (uint 1.0)))
         (vertex-2 (+ vertex-0 (uint 2.0)))
         (vertex-3 (+ vertex-0 (uint 3.0)))
         (primitive-0 (* lane (uint 2.0)))
         (primitive-1 (+ primitive-0 (uint 1.0)))
         ;; Counter-clockwise from the outward side: reverse the loop when
         ;; the sign flips the canonical orientation.
         (second-0 (if negative-p vertex-2 vertex-1))
         (third-0 (if negative-p vertex-1 vertex-2))
         (second-1 (if negative-p vertex-3 vertex-2))
         (third-1 (if negative-p vertex-2 vertex-3)))
    (set-mesh-output-counts (uint #.(* 4 +brick-size+))
                            (uint #.(* 2 +brick-size+)))
    (set-mesh-vertex vertex-0
                     (position clip-0) (normal normal)
                     (world corner-0) (uv (vec2 0.0 0.0)))
    (set-mesh-vertex vertex-1
                     (position clip-1) (normal normal)
                     (world corner-1) (uv (vec2 1.0 0.0)))
    (set-mesh-vertex vertex-2
                     (position clip-2) (normal normal)
                     (world corner-2) (uv (vec2 1.0 1.0)))
    (set-mesh-vertex vertex-3
                     (position clip-3) (normal normal)
                     (world corner-3) (uv (vec2 0.0 1.0)))
    (set-mesh-primitive primitive-0 (uvec3 vertex-0 second-0 third-0))
    (set-mesh-primitive primitive-1 (uvec3 vertex-0 second-1 third-1))))

(define-shader surface-fragment-shader
    (:stage :fragment
     :inputs ((normal :vec3 :location 0)
              (world :vec3 :location 1)
              (uv :vec2 :location 2))
     :outputs ((color :vec4 :location 0))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)))
  (let* ((sun (swizzle sun-vector :xyz))
         (ambient (swizzle sun-vector :w))
         (diffuse (max (dot normal sun) 0.0))
         (light (+ ambient (* diffuse (- 1.0 ambient))))
         (upness (swizzle normal :z))
         (top (vec3 0.40 0.60 0.28))
         (side (vec3 0.58 0.50 0.40))
         (bottom (vec3 0.28 0.26 0.25))
         (base (if (> upness 0.5) top (if (< upness -0.5) bottom side)))
         ;; Textureless, but not featureless: a soft line where faces meet.
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (edge (min (min u (- 1.0 u)) (min v (- 1.0 v))))
         (line (mix 0.70 1.0 (smoothstep 0.0 0.06 edge)))
         (shaded (* base (* light line)))
         (camera (swizzle camera-vector :xyz))
         (delta (- world camera))
         (distance (sqrt (dot delta delta)))
         (fog-far (swizzle sky-vector :w))
         (fog (smoothstep (* 0.45 fog-far) fog-far distance))
         (final (mix shaded (swizzle sky-vector :xyz) fog)))
    (set-output color (vec4 final 1.0))))

(defun frame-uniform-block ()
  "The frame uniform block as declared by the mesh stage."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources (surface-mesh-shader))))
