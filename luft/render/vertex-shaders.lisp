;;; The surface drawn by ordinary vertex shaders pulling packed sites.
;;;
;;; Mesh shaders are a young extension and not every driver that advertises
;;; them will run them; this is the same atelier on the oldest pipeline there
;;; is.  Nothing changes in the representation: the host still uploads the
;;; surface chain as one (unsigned-byte 64) site per face and builds no
;;; vertex.  A draw of K vertices per face is issued with no vertex buffer at
;;; all, and each vertex shader invocation divides its built-in index by K to
;;; find its site and takes the remainder to learn which corner of which
;;; triangle it is.  The fragment stages are the very same ones the mesh path
;;; feeds: NORMAL, WORLD, UV, and for the chamfer FACE-NORMAL, at the same
;;; locations.  #AGVXGM #VAABY9 #NV25VM
;;;
;;; A vertex cannot be skipped the way a mesh lane's output count is uniform,
;;; so an absent or back-facing face collapses every one of its vertices onto
;;; the anchor, where the rasterizer drops the degenerate triangle for free.

(in-package #:luft.render.shaders)

;;; ------------------------------------------------------------------------
;;; From a vertex index to a quad corner
;;;
;;; Six vertices draw one quad as two triangles around corner 0:
;;;
;;;     (0 1 2) (0 2 3)        positive polarity, counter-clockwise outward
;;;     (0 2 1) (0 3 2)        negative polarity, the same loop reversed
;;;
;;; Corners run anchor, +A, +A+B, +B, so corner C spans A when C is 1 or 2
;;; and spans B when C is 2 or 3.  The reversal that polarity asks for is
;;; the same swap the mesh shader makes between its second and third
;;; primitive vertices.

(define-shader-function quad-corner (vertex-in-quad negative-p)
  "The corner (0..3) that vertex VERTEX-IN-QUAD (0..5) of a quad draws,
with the loop reversed when NEGATIVE-P."
  (let* ((triangle (float (/ vertex-in-quad (uint 3.0))))
         (slot (float (mod vertex-in-quad (uint 3.0)))))
    (if (< slot 0.5)
        0.0
        (if negative-p
            (- (+ 3.0 triangle) slot)
            (+ slot triangle)))))

(define-shader-function corner-spans-a (corner)
  "One when CORNER lies along the face's first spanning axis."
  (if (> corner 0.5) (if (< corner 2.5) 1.0 0.0) 0.0))

(define-shader-function corner-spans-b (corner)
  "One when CORNER lies along the face's second spanning axis."
  (if (> corner 1.5) 1.0 0.0))

;;; ------------------------------------------------------------------------
;;; Flat faces: six vertices a site

(defgeneric surface-vertices-per-face (style)
  (:documentation
   "How many vertices the vertex-pulling shader of STYLE draws per site.
The renderer multiplies this by the site count to size its draw."))

(defmethod surface-vertices-per-face ((style (eql :flat))) 6)

(define-shader surface-vertex-shader
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (sites :storage-buffer :binding #.+sites-binding+
                        :element :uint64))
     :outputs ((position :vec4 :built-in :position)
               (normal :vec3 :location 0)
               (world :vec3 :location 1)
               (uv :vec2 :location 2)))
  (let* ((site (buffer-element sites (/ vertex-index (uint 6.0))))
         (vertex-in-quad (mod vertex-index (uint 6.0)))
         ;; The signed site: XYZ extent in the low three bits, polarity in the
         ;; fourth, then X, Y, Z anchors.
         (extent (uint (ldb (byte luft:+extent-bits+ 0) site)))
         (anchor
           (vec3 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                          luft:+x-shift+)
                                    site)))
                 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                          luft:+y-shift+)
                                    site)))
                 (float (uint (ldb (byte luft:+vertical-coordinate-bits+
                                          luft:+z-shift+)
                                    site)))))
         (negative-p (= (uint (ldb (byte 1 luft:+site-sign-bit+) site))
                        (uint 1.0)))
         (present-p (> extent (uint 0.0)))
         (yz-face-p (= extent (uint luft:+yz-face-extent+)))
         (xz-face-p (= extent (uint luft:+xz-face-extent+)))
         (xy-face-p (= extent (uint luft:+xy-face-extent+)))
         ;; The two spanning axes in canonical X<Y<Z order, and their cross
         ;; product: the face's canonical orientation, which is also the
         ;; orientation convention of the boundary operator.
         (edge-a (if yz-face-p (vec3 0.0 1.0 0.0) (vec3 1.0 0.0 0.0)))
         (edge-b (if xy-face-p (vec3 0.0 1.0 0.0) (vec3 0.0 0.0 1.0)))
         (canonical (if yz-face-p
                        (vec3 1.0 0.0 0.0)
                        (if xz-face-p (vec3 0.0 -1.0 0.0) (vec3 0.0 0.0 1.0))))
         (orientation (if negative-p -1.0 1.0))
         (normal (* canonical orientation))
         (camera (swizzle camera-vector :xyz))
         (facing-p (> (dot normal (- camera anchor)) 0.0))
         ;; Absent or back-facing faces collapse onto their anchor.
         (scale (if present-p (if facing-p 1.0 0.0) 0.0))
         (corner (quad-corner vertex-in-quad negative-p))
         (a (corner-spans-a corner))
         (b (corner-spans-b corner))
         (point (+ anchor (* (+ (* edge-a a) (* edge-b b)) scale)))
         (clip (view-clip point camera
                          (swizzle right-vector :xyz)
                          (swizzle up-vector :xyz)
                          (swizzle forward-vector :xyz)
                          projection-vector)))
    (set-output position clip)
    (set-output normal normal)
    (set-output world point)
    (set-output uv (vec2 a b))))

;;; ------------------------------------------------------------------------
;;; Chamfered faces: a 4x4 point grid, fifty-four vertices a site
;;;
;;; The mesh shader of #RUAWR5 generates all sixteen grid points of a face in
;;; one lane from sixteen gathered cells.  A vertex is one grid point, so it
;;; needs only the star of its own nearest corner: six cells beside the
;;; face's solid and air cells, across U, across V, and across both, toward
;;; the corner's side.  The same site rules then place it -- a shared point
;;; moves half the chamfer width toward its star's minority -- and because
;;; every face incident to a site gathers the same star, the facets still
;;; meet exactly.  The grid quads split along the diagonal toward the nearest
;;; corner, exactly as the mesh shader's do, so a corner square is two flat
;;; chamfer pieces.

(defmethod surface-vertices-per-face ((style (eql :chamfer))) 54)
(defmethod surface-vertices-per-face ((style (eql :paper))) 54)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun chamfer-vertex-shader-definition ()
    "The chamfer vertex shader, spliced around six cell gathers."
    `(define-shader chamfer-vertex-shader
         (:stage :vertex
          :inputs ((vertex-index :uint :built-in :vertex-index))
          :resources ((frame :uniform-block :binding ,+frame-binding+
                             :members ,*frame-uniform-members*)
                      (sites :storage-buffer :binding ,+sites-binding+
                             :element :uint64)
                      (cells :storage-buffer :binding ,+cells-binding+
                             :element :uint))
          :outputs ((position :vec4 :built-in :position)
                    (normal :vec3 :location 0)
                    (world :vec3 :location 1)
                    (uv :vec2 :location 2)
                    (face-normal :vec3 :location 3)))
       (let* ((site (buffer-element sites (/ vertex-index (uint 54.0))))
              (vertex-in-face (mod vertex-index (uint 54.0)))
              ;; Which of the nine grid quads, and which vertex of its six.
              (quad (/ vertex-in-face (uint 6.0)))
              (vertex-in-quad (mod vertex-in-face (uint 6.0)))
              (quad-i (float (/ quad (uint 3.0))))
              (quad-j (float (mod quad (uint 3.0))))
              (extent (uint (ldb (byte luft:+extent-bits+ 0) site)))
              (anchor
                (vec3 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                               luft:+x-shift+)
                                         site)))
                      (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                               luft:+y-shift+)
                                         site)))
                      (float (uint (ldb (byte luft:+vertical-coordinate-bits+
                                               luft:+z-shift+)
                                         site)))))
              (negative-p (= (uint (ldb (byte 1 luft:+site-sign-bit+) site))
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
              ;; A chamfered point's normal tilts up to 45 degrees from the
              ;; face normal, so only faces turned well past grazing can be
              ;; dropped: the same relaxed test as the mesh shader's.
              (toward (- camera (+ anchor (* (+ edge-a edge-b) 0.5))))
              (toward-normal (dot normal toward))
              (facing-p (if (> toward-normal 0.0)
                            (> 1.0 0.0)
                            (< (* toward-normal toward-normal)
                               (* 0.72 (dot toward toward)))))
              (scale (if present-p (if facing-p 1.0 0.0) 0.0))
              (period-x (swizzle domain-vector :x))
              (period-y (swizzle domain-vector :y))
              (radius (swizzle domain-vector :z))
              ;; The quad's diagonal points at the nearest corner of the
              ;; face: rotate the corner loop by one when it does not, so
              ;; that (p q r)(p r s) splits a b c d along b-d instead of a-c.
              (toward-corner-p (if (< quad-i 1.5)
                                   (< quad-j 1.5)
                                   (> quad-j 1.5)))
              (loop-corner (quad-corner vertex-in-quad negative-p))
              (corner (if toward-corner-p
                          loop-corner
                          (if (> loop-corner 2.5) 0.0 (+ loop-corner 1.0))))
              ;; The grid point this vertex is, 0..3 along each axis.
              (grid-i (+ quad-i (corner-spans-a corner)))
              (grid-j (+ quad-j (corner-spans-b corner)))
              ;; Its in-plane parameters: 0, the width, one less the width, 1.
              (s (if (< grid-i 0.5) 0.0
                     (if (< grid-i 1.5) radius
                         (if (< grid-i 2.5) (- 1.0 radius) 1.0))))
              (t* (if (< grid-j 0.5) 0.0
                      (if (< grid-j 1.5) radius
                          (if (< grid-j 2.5) (- 1.0 radius) 1.0))))
              (flat (+ anchor (* edge-a s) (* edge-b t*)))
              ;; The nearest corner's side along each axis, and whether the
              ;; point lies on the face's boundary along each.
              (su (if (< grid-i 1.5) -1.0 1.0))
              (sv (if (< grid-j 1.5) -1.0 1.0))
              (on-u-p (if (< grid-i 0.5) (> 1.0 0.0) (> grid-i 2.5)))
              (on-v-p (if (< grid-j 0.5) (> 1.0 0.0) (> grid-j 2.5)))
              (u (* edge-a su))
              (v (* edge-b sv))
              ;; The face's own solid cell and air cell anchors, and the
              ;; six cells of the nearest corner's star beside them.
              (positive-p (> (dot normal axis) 0.0))
              (solid-anchor (- anchor (* axis (if positive-p 1.0 0.0))))
              (air-anchor (- anchor (* axis (if positive-p 0.0 1.0))))
              ,@(cell-solid-bindings 'solid-cu '(+ solid-anchor u))
              ,@(cell-solid-bindings 'solid-eu '(+ air-anchor u))
              ,@(cell-solid-bindings 'solid-cv '(+ solid-anchor v))
              ,@(cell-solid-bindings 'solid-ev '(+ air-anchor v))
              ,@(cell-solid-bindings 'solid-cuv '(+ solid-anchor (+ u v)))
              ,@(cell-solid-bindings 'solid-euv '(+ air-anchor (+ u v)))
              ;; Site rules: the corner's own star when the point is the
              ;; corner, an edge's star along a boundary, nothing inside.
              (u-minority (edge-minority solid-cu solid-eu normal u))
              (v-minority (edge-minority solid-cv solid-ev normal v))
              (corner-minority
                (vertex-minority normal u v
                                 solid-cu solid-eu solid-cv solid-ev
                                 solid-cuv solid-euv))
              (minority (if on-u-p
                            (if on-v-p corner-minority u-minority)
                            (if on-v-p v-minority (vec3 0.0 0.0 0.0))))
              (point (+ anchor
                        (* (- (chamfer-point flat minority radius) anchor)
                           scale)))
              (point-normal (chamfer-normal minority normal))
              (clip (view-clip point camera
                               (swizzle right-vector :xyz)
                               (swizzle up-vector :xyz)
                               (swizzle forward-vector :xyz)
                               projection-vector)))
         (set-output position clip)
         (set-output normal point-normal)
         (set-output world point)
         (set-output uv (vec2 s t*))
         (set-output face-normal normal)))))

#.(chamfer-vertex-shader-definition)

;;; ------------------------------------------------------------------------
;;; The screen: one triangle behind everything, from three vertex indices

(define-shader sky-vertex-shader
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((position :vec4 :built-in :position)
               (ndc :vec2 :location 0)))
  ;; The oversized triangle covering clip space: (-1,-1), (3,-1), (-1,3).
  ;; Depth sits just inside the far plane so the pass may run first and the
  ;; world still draws over it under an ordinary less-than test.
  (let* ((index (float vertex-index))
         (x (if (< index 0.5) -1.0 (if (< index 1.5) 3.0 -1.0)))
         (y (if (< index 1.5) -1.0 3.0)))
    (set-output position (vec4 x y 0.99999 1.0))
    (set-output ndc (vec2 x y))))
