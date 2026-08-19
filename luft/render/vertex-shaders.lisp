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

;;; Shaped faces: a point grid per site, one vertex per grid point
;;;
;;; The mesh shaders of #6TEFOS and #RUAWR5 generate every grid point of a
;;; face in one lane from sixteen gathered cells.  A vertex is one grid
;;; point, so it needs only the star of its own nearest corner: six cells
;;; beside the face's solid and air cells, across U, across V, and across
;;; both, toward that corner's side.  The same site rules then place it, and
;;; because every face incident to a site gathers the same star, the shaped
;;; faces still meet exactly.  The grid quads split along the diagonal toward
;;; the nearest corner, exactly as the mesh shader's do, so a corner square
;;; is two flat chamfer pieces.
;;;
;;; One definition serves both rules.  The chamfer is one ring: a 4x4 grid,
;;; nine quads, fifty-four vertices a site, and only shared points move, by
;;; half the width toward their star's minority.  The rounding is two rings:
;;; a 6x6 grid, twenty-five quads, a hundred and fifty vertices a site; the
;;; shared points project onto their site's sphere or cylinder, the ring
;;; points onto the nearest crease's cylinder blended toward the corner's
;;; sphere, as BEVEL-POINT-BINDINGS does for the mesh lane, but chosen at run
;;; time from the vertex's own grid position instead of generated per point.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun grid-vertices-per-face (rings)
    "Six vertices for each quad of the grid RINGS rings make."
    (let ((side (let ((*bevel-rings* rings)) (bevel-grid-side))))
      (* 6 (1- side) (1- side))))

  (defun grid-parameter-form (index rings)
    "A form giving the in-plane parameter of grid line INDEX (a float
0..SIDE-1 at run time): 0, then RADIUS in RINGS steps, mirrored from the
far side, then 1."
    (let ((side (let ((*bevel-rings* rings)) (bevel-grid-side))))
      (labels ((chain (k)
                 (if (= k (1- side))
                     1.0
                     `(if (< ,index ,(+ k 0.5))
                          ,(let ((*bevel-rings* rings))
                             (bevel-grid-parameter k))
                          ,(chain (1+ k))))))
        (chain 0))))

  (defun grid-vertex-shader-definition (name rings rule)
    "A vertex shader definition named NAME: the grid of RINGS rings, each
vertex placed by RULE, :CHAMFER or :ROUND, from its nearest corner's star."
    (let* ((side (let ((*bevel-rings* rings)) (bevel-grid-side)))
           (quads (1- side))
           (last (float (1- side)))
           (half (float (floor side 2)))
           (vertices (grid-vertices-per-face rings)))
      `(define-shader ,name
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
                      ,@(when (eq rule :chamfer)
                          '((face-normal :vec3 :location 3)))))
         (let* ((site (buffer-element
                       sites (/ vertex-index (uint ,(float vertices)))))
                (vertex-in-face (mod vertex-index (uint ,(float vertices))))
                ;; Which grid quad, and which vertex of its six.
                (quad (/ vertex-in-face (uint 6.0)))
                (vertex-in-quad (mod vertex-in-face (uint 6.0)))
                (quad-i (float (/ quad (uint ,(float quads)))))
                (quad-j (float (mod quad (uint ,(float quads)))))
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
                ;; A shaped point's normal tilts well away from the face
                ;; normal, so only faces turned well past grazing can be
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
                ;; that (p q r)(p r s) splits a b c d along b-d, not a-c.
                (toward-corner-p (if (< quad-i ,(- half 0.5))
                                     (< quad-j ,(- half 0.5))
                                     (> quad-j ,(- half 0.5))))
                (loop-corner (quad-corner vertex-in-quad negative-p))
                (corner (if toward-corner-p
                            loop-corner
                            (if (> loop-corner 2.5) 0.0 (+ loop-corner 1.0))))
                ;; The grid point this vertex is, 0..SIDE-1 along each axis,
                ;; and its in-plane parameters.
                (grid-i (+ quad-i (corner-spans-a corner)))
                (grid-j (+ quad-j (corner-spans-b corner)))
                (s ,(grid-parameter-form 'grid-i rings))
                (t* ,(grid-parameter-form 'grid-j rings))
                (flat (+ anchor (* edge-a s) (* edge-b t*)))
                ;; The nearest corner's side along each axis, whether the
                ;; point lies on the face's boundary along each, and how
                ;; many ring steps it is from that corner along each.
                (su (if (< grid-i ,(- half 0.5)) -1.0 1.0))
                (sv (if (< grid-j ,(- half 0.5)) -1.0 1.0))
                (on-u-p (if (< grid-i 0.5) (> 1.0 0.0) (> grid-i ,(- last 0.5))))
                (on-v-p (if (< grid-j 0.5) (> 1.0 0.0) (> grid-j ,(- last 0.5))))
                (u-steps (min grid-i (- ,last grid-i)))
                (v-steps (min grid-j (- ,last grid-j)))
                (u-near-p (< u-steps ,(- rings 0.5)))
                (v-near-p (< v-steps ,(- rings 0.5)))
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
                ;; Site rules: the two edges meeting at the corner, and the
                ;; corner itself.
                (u-minority (edge-minority solid-cu solid-eu normal u))
                (v-minority (edge-minority solid-cv solid-ev normal v))
                (corner-minority
                  (vertex-minority normal u v
                                   solid-cu solid-eu solid-cv solid-ev
                                   solid-cuv solid-euv))
                ,@(ecase rule
                    (:chamfer
                     ;; Woodworking: shared points move to the meeting of
                     ;; flat facets, and nothing else moves.
                     `((minority (if on-u-p
                                     (if on-v-p corner-minority u-minority)
                                     (if on-v-p v-minority (vec3 0.0 0.0 0.0))))
                       (shaped (chamfer-point flat minority radius))
                       (shaped-normal (chamfer-normal minority normal))))
                    (:round
                     ;; Every point projects onto the sphere or cylinder its
                     ;; nearest crease selects: a vertex onto its own star's,
                     ;; an inner point nowhere, and the rest onto the nearer
                     ;; creased edge's cylinder (their own edge's when on a
                     ;; boundary), blended toward the corner's sphere by
                     ;; their distance when that corner is a genuine
                     ;; three-axis star, or the sphere outright when the
                     ;; corner is a pure single-cell one.
                     `((corner-q (clamp corner-minority
                                        (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)))
                       (corner-foot (+ anchor
                                       (* edge-a (if (< su 0.0) 0.0 1.0))
                                       (* edge-b (if (< sv 0.0) 0.0 1.0))))
                       (corner-centre (+ corner-foot (* corner-q radius)))
                       (corner-weight (dot corner-minority corner-minority))
                       (corner-pure-p (if (> corner-weight 0.5)
                                          (< corner-weight 3.5)
                                          (> 0.0 1.0)))
                       (corner-three-p (> (dot corner-q corner-q) 2.5))
                       (u-creased-p (> (dot u-minority u-minority) 0.5))
                       (v-creased-p (> (dot v-minority v-minority) 0.5))
                       (u-foot (+ anchor
                                  (* edge-a (if (< su 0.0) 0.0 1.0))
                                  (* edge-b t*)))
                       (v-foot (+ anchor
                                  (* edge-a s)
                                  (* edge-b (if (< sv 0.0) 0.0 1.0))))
                       ;; The base projection's edge: the point's own when it
                       ;; lies on one, else the nearer creased edge.
                       (use-u-p (if on-u-p
                                    (> 1.0 0.0)
                                    (if on-v-p
                                        (> 0.0 1.0)
                                        (if u-near-p
                                            (if v-near-p
                                                (if u-creased-p
                                                    (if v-creased-p
                                                        (<= u-steps v-steps)
                                                        (> 1.0 0.0))
                                                    (> 0.0 1.0))
                                                (> 1.0 0.0))
                                            (> 0.0 1.0)))))
                       (base-minority (if use-u-p u-minority v-minority))
                       (base-foot (if on-u-p flat
                                      (if on-v-p flat
                                          (if use-u-p u-foot v-foot))))
                       (base-centre (+ base-foot
                                       (site-centre base-minority radius)))
                       (base-point (rounded-point flat base-centre
                                                  base-minority radius))
                       (base-normal (rounded-normal flat base-centre
                                                    base-minority normal))
                       (sphere-point (rounded-point flat corner-centre
                                                    corner-minority radius))
                       (sphere-normal (rounded-normal flat corner-centre
                                                      corner-minority normal))
                       ;; Distance to the corner in ring steps: along the
                       ;; edge for boundary points, the larger axis
                       ;; otherwise, and the full ring count past the rings.
                       (u-reach (if u-near-p u-steps ,(float rings)))
                       (v-reach (if v-near-p v-steps ,(float rings)))
                       (steps (if on-u-p v-reach
                                  (if on-v-p u-reach (max u-reach v-reach))))
                       (falloff (max 0.0 (- 1.0 (/ steps ,(float rings)))))
                       (weight (if corner-pure-p 1.0
                                   (if corner-three-p falloff 0.0)))
                       (mixed-point (mix base-point sphere-point weight))
                       (mixed-normal (normalize (mix base-normal sphere-normal
                                                     weight)))
                       (corner-p (if on-u-p on-v-p (> 0.0 1.0)))
                       (inner-p (if u-near-p (> 0.0 1.0)
                                    (if v-near-p (> 0.0 1.0) (> 1.0 0.0))))
                       (shaped (if corner-p sphere-point
                                   (if inner-p flat mixed-point)))
                       (shaped-normal (if corner-p sphere-normal
                                          (if inner-p normal mixed-normal))))))
                (point (+ anchor (* (- shaped anchor) scale)))
                (clip (view-clip point camera
                                 (swizzle right-vector :xyz)
                                 (swizzle up-vector :xyz)
                                 (swizzle forward-vector :xyz)
                                 projection-vector)))
           (set-output position clip)
           (set-output normal shaped-normal)
           (set-output world point)
           (set-output uv (vec2 s t*))
           ,@(when (eq rule :chamfer)
               '((set-output face-normal normal))))))))

(defmethod surface-vertices-per-face ((style (eql :chamfer)))
  (grid-vertices-per-face 1))

(defmethod surface-vertices-per-face ((style (eql :paper)))
  (grid-vertices-per-face 1))

(defmethod surface-vertices-per-face ((style (eql :bevel)))
  (grid-vertices-per-face 2))

#.(grid-vertex-shader-definition 'chamfer-vertex-shader 1 :chamfer)

#.(grid-vertex-shader-definition 'bevel-vertex-shader 2 :round)

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
