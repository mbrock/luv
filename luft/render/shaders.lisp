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

(defconstant +brick-size+ 7
  "Terms per task workgroup and faces per mesh workgroup.

Seven faces subdivided into 6x6 grids are 252 vertices, just under the mesh
threadgroup limit of 256; the flat pipeline uses the same bricks at four
vertices a face.")
(defconstant +frame-binding+ 0)
(defconstant +terms-binding+ 1)
(defconstant +bricks-binding+ 2)
(defconstant +cells-binding+ 3)

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
      (sky-vector :vec4)         ; sky colour, fog distance
      (domain-vector :vec4))))   ; x period, y period, bevel radius, unused

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


;;; ------------------------------------------------------------------------
;;; Bevelled and filleted faces
;;;
;;; The rounding of a crease belongs to the crease's site, not to either face
;;; beside it.  An edge site has four cells around it, a vertex site eight; the
;;; solidity of that star classifies the crease, and one rule moves the shared
;;; point toward the star's minority (the lone solid cell of a convex crease,
;;; the lone air cell of a concave one) by r(sqrt(m) - 1) along the minority
;;; direction, m being its nonzero axes.  Every face incident to the site
;;; computes the same point from the same star, so the subdivided faces meet
;;; exactly.  The rule reproduces the fillet cylinder point at edges and the
;;; sphere point at corners, and vanishes on flat and saddle stars.

(define-shader-function site-centre (minority radius)
  "The centre of the rounding sphere or cylinder a site's star selects,
relative to the site: RADIUS along each nonzero axis of MINORITY."
  (* (clamp minority (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)) radius))

(define-shader-function rounded-point (point centre minority radius)
  "Project POINT onto the sphere or cylinder of RADIUS about CENTRE, or
return it unchanged when the MINORITY is zero and the site is flat."
  (let* ((offset (- point centre))
         (length (sqrt (dot offset offset))))
    (if (> (dot minority minority) 0.5)
        (+ centre (* offset (/ radius length)))
        point)))

(define-shader-function rounded-normal (point centre minority normal)
  "The rounded surface normal at POINT: away from CENTRE on a convex star,
toward it on a concave one, and NORMAL where the star is flat."
  (let* ((offset (- point centre))
         (length (sqrt (dot offset offset)))
         (sign (if (> (dot minority normal) 0.0) -1.0 1.0)))
    (if (> (dot minority minority) 0.5)
        (* offset (/ sign length))
        normal)))

(define-shader-function edge-minority (solid-c solid-e normal tangent)
  "The minority direction of an edge star, or zero when it is flat or saddle.

The face's own solid cell and air cell are known; SOLID-C and SOLID-E are the
solidity of the two cells across the edge in TANGENT direction, beside the
solid cell and the air cell respectively.  A convex crease moves into the
solid, a concave crease out of it, and both move inward along the face."
  (if (< (abs (- solid-c solid-e)) 0.5)
      (- (* normal (- (* 2.0 solid-c) 1.0)) tangent)
      (vec3 0.0 0.0 0.0)))

(define-shader-function vertex-minority
    (normal u v solid-cu solid-eu solid-cv solid-ev solid-cuv solid-euv)
  "The minority direction of a vertex star from the six unknown solidities.

The face's solid cell sits at (-N,-U,-V) and its air cell at (+N,-U,-V);
the six arguments are the cells across U, across V, and across both, each
beside the solid cell and beside the air cell."
  (let* ((count (+ 1.0 solid-cu solid-eu solid-cv solid-ev solid-cuv
                   solid-euv))
         (sum-n (+ -1.0 (- solid-cu) solid-eu (- solid-cv) solid-ev
                   (- solid-cuv) solid-euv))
         (sum-u (+ -1.0 solid-cu solid-eu (- solid-cv) (- solid-ev)
                   solid-cuv solid-euv))
         (sum-v (+ -1.0 (- solid-cu) (- solid-eu) solid-cv solid-ev
                   solid-cuv solid-euv))
         (solid-sum (+ (* normal sum-n) (* u sum-u) (* v sum-v))))
    (if (< count 3.5)
        solid-sum
        (if (> count 4.5)
            (- solid-sum)
            (vec3 0.0 0.0 0.0)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +bevel-rings+ 2
    "Subdivision rings inside the rounding radius: one is a chamfer, two a
fillet.  The grid has 2*(rings+1) points per side.")

  (defun bevel-grid-side ()
    (* 2 (1+ +bevel-rings+)))

  (defun bevel-grid-index (i j)
    (+ (* i (bevel-grid-side)) j))

  (defun bevel-grid-parameter (k)
    "The in-plane parameter of grid line K: 0, then RADIUS in RINGS steps,
mirrored from the far side."
    (let ((side (bevel-grid-side)))
      (cond ((= k 0) 0.0)
            ((= k (1- side)) 1.0)
            ((<= k +bevel-rings+)
             `(* radius ,(/ (float k) +bevel-rings+)))
            (t
             `(- 1.0 (* radius ,(/ (float (- side 1 k)) +bevel-rings+)))))))

  (defun cell-solid-bindings (name position-form)
    "LET* bindings that read the solidity of the cell at POSITION-FORM."
    (let ((x0 (intern (format nil "~A-X0" name)))
          (y0 (intern (format nil "~A-Y0" name)))
          (x (intern (format nil "~A-X" name)))
          (y (intern (format nil "~A-Y" name)))
          (z (intern (format nil "~A-Z" name)))
          (index (intern (format nil "~A-INDEX" name)))
          (word (intern (format nil "~A-WORD" name))))
      `((,x0 (swizzle ,position-form :x))
        (,y0 (swizzle ,position-form :y))
        (,x (if (< ,x0 0.0) (+ ,x0 period-x)
                (if (< ,x0 period-x) ,x0 (- ,x0 period-x))))
        (,y (if (< ,y0 0.0) (+ ,y0 period-y)
                (if (< ,y0 period-y) ,y0 (- ,y0 period-y))))
        (,z (max 0.0 (min (swizzle ,position-form :z) 255.0)))
        (,index (uint (+ ,x (* period-x (+ ,y (* period-y ,z))))))
        (,word (buffer-element cells (/ ,index (uint 32.0))))
        (,name (float (ldb (byte 1 (mod ,index (uint 32.0))) ,word))))))

  (defun bevel-corner-bindings ()
    "Per corner: the clamped minority Q, the sphere centre, and whether the
star is a pure single-cell corner or a genuine three-axis one."
    (loop for (u-side v-side) in '(("NEG" "NEG") ("NEG" "POS")
                                   ("POS" "NEG") ("POS" "POS"))
          for corner = (intern (format nil "CORNER-~A-~A" u-side v-side))
          for minority = (intern (format nil "~A-MINORITY" corner))
          for q = (intern (format nil "~A-Q" corner))
          append
          `((,q (clamp ,minority (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)))
            (,(intern (format nil "~A-CENTRE" corner))
             (+ anchor
                (* edge-a ,(if (string= u-side "NEG") 0.0 1.0))
                (* edge-b ,(if (string= v-side "NEG") 0.0 1.0))
                (* ,q radius)))
            (,(intern (format nil "~A-PURE" corner))
             (if (> (dot ,minority ,minority) 0.5)
                 (< (dot ,minority ,minority) 3.5)
                 (> 0.0 1.0)))
            (,(intern (format nil "~A-THREE" corner))
             (> (dot ,q ,q) 2.5)))))

  (defun bevel-point-bindings (i j)
    "Bindings placing grid point I,J on the rounded surface.

Every point projects onto the sphere or cylinder its nearest crease selects.
Vertices use their own star.  Other points start from the nearest creased
edge's cylinder, or stay flat, and blend toward the sphere of the nearest
vertex by their distance to it whenever that vertex is a genuine three-axis
corner; a pure corner star, one solid or one air cell, is the sphere
outright, which is exactly the rounded box.  Inner points stay on the plane."
    (let* ((side (bevel-grid-side))
           (last (1- side))
           (half (floor side 2))
           (name (format nil "~D~D" i j))
           (flat (intern (format nil "FLAT-~A" name)))
           (point (intern (format nil "POINT-~A" name)))
           (normal (intern (format nil "NORMAL-~A" name)))
           (clip (intern (format nil "CLIP-~A" name)))
           (s (bevel-grid-parameter i))
           (t* (bevel-grid-parameter j))
           (u-side (if (< i half) "NEG" "POS"))
           (v-side (if (< j half) "NEG" "POS"))
           ;; Ring steps from the nearest vertex along each axis, or NIL when
           ;; the point is beyond the radius along that axis.
           (u-steps (cond ((< i +bevel-rings+) i)
                          ((> i (- last +bevel-rings+)) (- last i))))
           (v-steps (cond ((< j +bevel-rings+) j)
                          ((> j (- last +bevel-rings+)) (- last j))))
           (u-edge (and u-steps (intern (format nil "EDGE-U-~A" u-side))))
           (v-edge (and v-steps (intern (format nil "EDGE-V-~A" v-side))))
           (corner (intern (format nil "CORNER-~A-~A" u-side v-side)))
           (u-minority (and u-edge (intern (format nil "~A-MINORITY" u-edge))))
           (v-minority (and v-edge (intern (format nil "~A-MINORITY" v-edge))))
           (corner-minority (intern (format nil "~A-MINORITY" corner)))
           (corner-centre (intern (format nil "~A-CENTRE" corner)))
           (corner-pure (intern (format nil "~A-PURE" corner)))
           (corner-three (intern (format nil "~A-THREE" corner)))
           (u-foot `(+ anchor (* edge-a ,(if (string= u-side "NEG") 0.0 1.0))
                       (* edge-b ,t*)))
           (v-foot `(+ anchor (* edge-a ,s)
                       (* edge-b ,(if (string= v-side "NEG") 0.0 1.0))))
           (base-minority (intern (format nil "BASE-MINORITY-~A" name)))
           (base-centre (intern (format nil "BASE-CENTRE-~A" name)))
           (base-point (intern (format nil "BASE-POINT-~A" name)))
           (base-normal (intern (format nil "BASE-NORMAL-~A" name)))
           (sphere-point (intern (format nil "SPHERE-POINT-~A" name)))
           (sphere-normal (intern (format nil "SPHERE-NORMAL-~A" name)))
           (weight (intern (format nil "WEIGHT-~A" name)))
           (mixed-normal (intern (format nil "MIXED-NORMAL-~A" name)))
           (flat-form `(+ anchor (* edge-a ,s) (* edge-b ,t*))))
      (append
       `((,flat ,flat-form))
       (cond
         ;; A shared vertex: its own star, and nothing else.
         ((and (member i (list 0 last)) (member j (list 0 last)))
          `((,point (+ anchor
                       (* (- (rounded-point ,flat ,corner-centre
                                            ,corner-minority radius)
                             anchor)
                          scale)))
            (,normal (rounded-normal ,flat ,corner-centre ,corner-minority
                                     normal))))
         ;; Inner points stay on the plane.
         ((and (null u-steps) (null v-steps))
          `((,point (+ anchor (* (- ,flat anchor) scale)))
            (,normal normal)))
         (t
          (let* ((on-u-boundary (member i (list 0 last)))
                 (on-v-boundary (member j (list 0 last)))
                 ;; The base projection: the point's own edge when it lies on
                 ;; one, else the nearer creased edge's cylinder, else flat.
                 (base
                   (cond
                     (on-u-boundary `((,base-minority ,u-minority)
                                      (,base-centre
                                       (+ ,flat (site-centre ,u-minority
                                                             radius)))))
                     (on-v-boundary `((,base-minority ,v-minority)
                                      (,base-centre
                                       (+ ,flat (site-centre ,v-minority
                                                             radius)))))
                     ((and u-edge v-edge)
                      (let ((u-first (<= u-steps v-steps)))
                        `((,base-minority
                           (if (> (dot ,u-minority ,u-minority) 0.5)
                               (if (> (dot ,v-minority ,v-minority) 0.5)
                                   ,(if u-first u-minority v-minority)
                                   ,u-minority)
                               ,v-minority))
                          (,base-centre
                           (if (> (dot ,u-minority ,u-minority) 0.5)
                               (if (> (dot ,v-minority ,v-minority) 0.5)
                                   (+ ,(if u-first u-foot v-foot)
                                      (site-centre ,(if u-first u-minority
                                                        v-minority)
                                                   radius))
                                   (+ ,u-foot (site-centre ,u-minority radius)))
                               (+ ,v-foot (site-centre ,v-minority radius)))))))
                     (u-edge `((,base-minority ,u-minority)
                               (,base-centre
                                (+ ,u-foot (site-centre ,u-minority radius)))))
                     (t `((,base-minority ,v-minority)
                          (,base-centre
                           (+ ,v-foot (site-centre ,v-minority radius)))))))
                 ;; Distance to the nearest vertex in ring steps: along the
                 ;; edge for boundary points, the larger axis otherwise.
                 (steps (cond (on-u-boundary (or v-steps +bevel-rings+))
                              (on-v-boundary (or u-steps +bevel-rings+))
                              (t (max (or u-steps +bevel-rings+)
                                      (or v-steps +bevel-rings+)))))
                 (falloff (max 0.0 (- 1.0 (/ (float steps) +bevel-rings+)))))
            `(,@base
              (,base-point (rounded-point ,flat ,base-centre ,base-minority
                                          radius))
              (,base-normal (rounded-normal ,flat ,base-centre ,base-minority
                                            normal))
              (,sphere-point (rounded-point ,flat ,corner-centre
                                            ,corner-minority radius))
              (,sphere-normal (rounded-normal ,flat ,corner-centre
                                              ,corner-minority normal))
              (,weight (if ,corner-pure 1.0
                           (if ,corner-three ,falloff 0.0)))
              (,point (+ anchor
                         (* (- (mix ,base-point ,sphere-point ,weight) anchor)
                            scale)))
              (,mixed-normal (mix ,base-normal ,sphere-normal ,weight))
              (,normal (normalize ,mixed-normal))))))
       `((,clip (view-clip ,point camera right up forward
                           projection-vector))))))

  (defun bevel-mesh-shader-definition ()
    "The BEVEL-MESH-SHADER definition: sixteen gathers, eight site rules, a
grid of rounded points, and their triangles per face."
    (let* ((side (bevel-grid-side))
           (points (* side side))
           (primitives (* 2 (1- side) (1- side)))
           (gathers
             ;; name, solid-side-p, du, dv
             '((solid-cu-pos t 1 0) (solid-eu-pos nil 1 0)
               (solid-cu-neg t -1 0) (solid-eu-neg nil -1 0)
               (solid-cv-pos t 0 1) (solid-ev-pos nil 0 1)
               (solid-cv-neg t 0 -1) (solid-ev-neg nil 0 -1)
               (solid-c-pos-pos t 1 1) (solid-e-pos-pos nil 1 1)
               (solid-c-pos-neg t 1 -1) (solid-e-pos-neg nil 1 -1)
               (solid-c-neg-pos t -1 1) (solid-e-neg-pos nil -1 1)
               (solid-c-neg-neg t -1 -1) (solid-e-neg-neg nil -1 -1)))
           (point-bindings '())
           (vertex-statements '())
           (primitive-statements '()))
      (flet ((offset-form (du dv)
               (cond ((and (/= du 0) (/= dv 0))
                      `(+ (* edge-a ,(float du)) (* edge-b ,(float dv))))
                     ((/= du 0) `(* edge-a ,(float du)))
                     (t `(* edge-b ,(float dv))))))
        (dotimes (i side)
          (dotimes (j side)
            (let ((name (format nil "~D~D" i j)))
              (setf point-bindings
                    (append point-bindings (bevel-point-bindings i j)))
              (setf vertex-statements
                    (append
                     vertex-statements
                     `((set-mesh-vertex
                        (+ vertex-base (uint ,(float (bevel-grid-index i j))))
                        (position ,(intern (format nil "CLIP-~A" name)))
                        (normal ,(intern (format nil "NORMAL-~A" name)))
                        (world ,(intern (format nil "POINT-~A" name)))
                        (uv (vec2 ,(bevel-grid-parameter i)
                                  ,(bevel-grid-parameter j))))))))))
        (dotimes (i (1- side))
          (dotimes (j (1- side))
            (let ((g00 (float (bevel-grid-index i j)))
                  (g10 (float (bevel-grid-index (1+ i) j)))
                  (g11 (float (bevel-grid-index (1+ i) (1+ j))))
                  (g01 (float (bevel-grid-index i (1+ j))))
                  (primitive (float (* 2 (+ (* i (1- side)) j)))))
              (setf primitive-statements
                    (append
                     primitive-statements
                     `((set-mesh-primitive
                        (+ primitive-base (uint ,primitive))
                        (uvec3 (+ vertex-base (uint ,g00))
                               (+ vertex-base (if negative-p (uint ,g11)
                                                  (uint ,g10)))
                               (+ vertex-base (if negative-p (uint ,g10)
                                                  (uint ,g11)))))
                       (set-mesh-primitive
                        (+ primitive-base (uint ,(1+ primitive)))
                        (uvec3 (+ vertex-base (uint ,g00))
                               (+ vertex-base (if negative-p (uint ,g01)
                                                  (uint ,g11)))
                               (+ vertex-base (if negative-p (uint ,g11)
                                                  (uint ,g01)))))))))))
        `(define-shader bevel-mesh-shader
             (:stage :mesh
              :workgroup-size (,+brick-size+ 1 1)
              :payload surface-brick-payload
              :inputs ((lane :uint :built-in :local-invocation-index))
              :resources ((frame :uniform-block :binding ,+frame-binding+
                                 :members ,*frame-uniform-members*)
                          (terms :storage-buffer :binding ,+terms-binding+
                                 :element :uint64)
                          (cells :storage-buffer :binding ,+cells-binding+
                                 :element :uint))
              :mesh-output
              (:topology :triangles
               :max-vertices ,(* points +brick-size+)
               :max-primitives ,(* primitives +brick-size+)
               :vertex ((position :vec4 :built-in :position)
                        (normal :vec3 :location 0)
                        (world :vec3 :location 1)
                        (uv :vec2 :location 2))))
           (let* ((term (buffer-element
                         terms (+ (* brick-index (uint ,+brick-size+)) lane)))
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
                  (edge-a (if yz-face-p (vec3 0.0 1.0 0.0) (vec3 1.0 0.0 0.0)))
                  (edge-b (if xy-face-p (vec3 0.0 1.0 0.0) (vec3 0.0 0.0 1.0)))
                  (canonical (if yz-face-p
                                 (vec3 1.0 0.0 0.0)
                                 (if xz-face-p
                                     (vec3 0.0 -1.0 0.0)
                                     (vec3 0.0 0.0 1.0))))
                  (orientation (if negative-p -1.0 1.0))
                  (normal (* canonical orientation))
                  (axis (abs canonical))
                  (camera (swizzle camera-vector :xyz))
                  ;; A rounded boundary point's normal tilts up to about 55
                  ;; degrees from the face normal, so only faces turned well
                  ;; past grazing can be dropped: those whose centre lies more
                  ;; than 145 degrees from the camera direction.
                  (toward (- camera (+ anchor (* (+ edge-a edge-b) 0.5))))
                  (toward-normal (dot normal toward))
                  (facing-p (if (> toward-normal 0.0)
                                (> 1.0 0.0)
                                (< (* toward-normal toward-normal)
                                   (* 0.72 (dot toward toward)))))
                  (scale (if present-p (if facing-p 1.0 0.0) 0.0))
                  (right (swizzle right-vector :xyz))
                  (up (swizzle up-vector :xyz))
                  (forward (swizzle forward-vector :xyz))
                  (period-x (swizzle domain-vector :x))
                  (period-y (swizzle domain-vector :y))
                  (radius (swizzle domain-vector :z))
                  ;; The face's own solid cell and air cell anchors.
                  (positive-p (> (dot normal axis) 0.0))
                  (solid-anchor (- anchor (* axis (if positive-p 1.0 0.0))))
                  (air-anchor (- anchor (* axis (if positive-p 0.0 1.0))))
                  ,@(loop for (name solid-p du dv) in gathers
                          append (cell-solid-bindings
                                  name
                                  `(+ ,(if solid-p 'solid-anchor 'air-anchor)
                                      ,(offset-form du dv))))
                  ;; Site rules: four edges, four corners.
                  (edge-u-pos-minority
                    (edge-minority solid-cu-pos solid-eu-pos normal edge-a))
                  (edge-u-neg-minority
                    (edge-minority solid-cu-neg solid-eu-neg normal (- edge-a)))
                  (edge-v-pos-minority
                    (edge-minority solid-cv-pos solid-ev-pos normal edge-b))
                  (edge-v-neg-minority
                    (edge-minority solid-cv-neg solid-ev-neg normal (- edge-b)))
                  (corner-pos-pos-minority
                    (vertex-minority normal edge-a edge-b
                                     solid-cu-pos solid-eu-pos
                                     solid-cv-pos solid-ev-pos
                                     solid-c-pos-pos solid-e-pos-pos))
                  (corner-pos-neg-minority
                    (vertex-minority normal edge-a (- edge-b)
                                     solid-cu-pos solid-eu-pos
                                     solid-cv-neg solid-ev-neg
                                     solid-c-pos-neg solid-e-pos-neg))
                  (corner-neg-pos-minority
                    (vertex-minority normal (- edge-a) edge-b
                                     solid-cu-neg solid-eu-neg
                                     solid-cv-pos solid-ev-pos
                                     solid-c-neg-pos solid-e-neg-pos))
                  (corner-neg-neg-minority
                    (vertex-minority normal (- edge-a) (- edge-b)
                                     solid-cu-neg solid-eu-neg
                                     solid-cv-neg solid-ev-neg
                                     solid-c-neg-neg solid-e-neg-neg))
                  ,@(bevel-corner-bindings)
                  ,@point-bindings
                  (vertex-base (* lane (uint ,(float points))))
                  (primitive-base (* lane (uint ,(float primitives)))))
             (set-mesh-output-counts (uint ,(float (* points +brick-size+)))
                                     (uint ,(float (* primitives +brick-size+))))
             ,@vertex-statements
             ,@primitive-statements))))))

#.(bevel-mesh-shader-definition)

(defun frame-uniform-block ()
  "The frame uniform block as declared by the mesh stage."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources (surface-mesh-shader))))
