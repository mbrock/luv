;;; The first genuinely volumetric body for an embodied agent: a little
;;; fellow assembled out of signed distance fields.
;;;
;;; A camera-facing square is only the conservative rasterization proxy.  Each
;;; fragment sends its world-space camera ray through the signed distance
;;; field and shades the point where sphere tracing converges.  The proxy's
;;; depth remains an approximation for now; the shape and its lighting are
;;; genuinely three-dimensional.
;;;
;;; The figure is built in its own upright frame: feet at local y zero, +y up,
;;; +z toward whoever is looking.  The gnome therefore always turns to face
;;; the camera, which is both the cheapest orientation and the correct one for
;;; someone whose whole job is to be addressed.  Every part is a small closed
;;; form -- a round cone for the robe and the hat, ellipsoids for the beard
;;; and the brim, spheres for head, nose, mittens and boots -- and the parts
;;; are welded with a polynomial smooth minimum so the joins read as one body
;;; rather than as a pile of primitives.  The same five part functions answer
;;; twice: once for the surface the ray finds, once for the colour it wears.

(in-package #:luvcraft.shaders)

;;; ---------------------------------------------------------------------
;;; Distance-field vocabulary

(define-shader-function gnome-length (vector)
  "Euclidean length, floored away from the origin's derivative singularity."
  (sqrt (max (dot vector vector) 1e-12)))

(define-shader-function gnome-smooth-union
    (first-distance second-distance radius)
  "The polynomial smooth minimum: a union with a fillet of RADIUS.

This is what makes a nose grow out of a face instead of intersecting it."
  (let* ((blend (clamp (+ 0.5 (* 0.5 (/ (- second-distance first-distance)
                                        radius)))
                       0.0 1.0)))
    (- (mix second-distance first-distance blend)
       (* radius (* blend (- 1.0 blend))))))

(define-shader-function gnome-sphere-distance (point center radius)
  "Signed distance to a sphere."
  (- (gnome-length (- point center)) radius))

(define-shader-function gnome-ellipsoid-distance (point center radii)
  "The usual bounded approximation to an ellipsoid's signed distance.

Exact would need a quartic root; this under-estimates, which is all sphere
tracing asks of it."
  (let* ((offset (/ (- point center) radii))
         (outer (gnome-length offset))
         (gradient (gnome-length (/ offset radii))))
    (/ (* outer (- outer 1.0)) (max gradient 1e-6))))

(define-shader-function gnome-round-cone-distance
    (point base-height top-height base-radius top-radius)
  "A cone capped by a sphere at each end: the shape of a robe, and of a hat.

The lateral surface is tangent to both caps, so the silhouette has no crease
where the cone meets its rounding."
  (let* ((height (max (- top-height base-height) 1e-4))
         (radial (gnome-length (vec2 (swizzle point :x) (swizzle point :z))))
         (axial (- (swizzle point :y) base-height))
         (slope (clamp (/ (- base-radius top-radius) height) -0.999 0.999))
         (run (sqrt (max (- 1.0 (* slope slope)) 1e-6)))
         (side (+ (* radial (- slope)) (* axial run))))
    (if (< side 0.0)
        (- (gnome-length (vec2 radial axial)) base-radius)
        (if (> side (* run height))
            (- (gnome-length (vec2 radial (- axial height))) top-radius)
            (- (+ (* radial run) (* axial slope)) base-radius)))))

;;; ---------------------------------------------------------------------
;;; The parts of a gnome
;;;
;;; Five functions, each answering for one material.  They are called by the
;;; marcher to find the surface and again at the hit point to decide which
;;; material owns it, so shape and colour can never drift apart.

(define-shader-function gnome-hat-point (point)
  "The hat's own frame: a domain warp that leans the cone's tip forward.

A straight cone is a traffic cone.  The lean is what makes it a hat someone
is wearing, and it costs one smoothstep."
  (let* ((rise (smoothstep 1.02 1.70 (swizzle point :y)))
         (lean (* rise rise)))
    (vec3 (- (swizzle point :x) (* 0.055 lean))
          (swizzle point :y)
          (- (swizzle point :z) (* 0.27 lean)))))

(define-shader-function gnome-hat-distance (point)
  "The tall cone and the brim it sits on.

The brim rides high, near the crown rather than at eye level.  A brim low
enough to be practical would put his whole face in shadow, and a gnome whose
eyes you cannot find is not a friendly little fellow but a bollard."
  (let* ((cone (gnome-round-cone-distance
                (gnome-hat-point point) 1.04 1.70 0.300 0.018))
         (brim (gnome-ellipsoid-distance
                point (vec3 0.0 1.06 0.0) (vec3 0.335 0.042 0.335))))
    (gnome-smooth-union cone brim 0.045)))

(define-shader-function gnome-skin-distance (point)
  "Head and nose.

Most of the head is under the hat or behind the beard; the nose is the part
of him you actually see, so it is a frank sphere."
  (let* ((head (gnome-sphere-distance point (vec3 0.0 0.90 0.0) 0.25))
         (nose (gnome-sphere-distance point (vec3 0.0 0.855 0.225) 0.085)))
    (gnome-smooth-union head nose 0.045)))

(define-shader-function gnome-beard-distance (point)
  "A broad beard with a blunt point: the mass that gives him his gravity.

Its top edge is the lower bound of his face, so it sits just under the nose:
enough beard to be venerable, not so much that there is nobody behind it."
  (let* ((mass (gnome-ellipsoid-distance
                point (vec3 0.0 0.605 0.04) (vec3 0.26 0.23 0.235)))
         (tip (gnome-sphere-distance point (vec3 0.0 0.435 0.115) 0.095))
         (moustache (gnome-ellipsoid-distance
                     point (vec3 0.0 0.775 0.155) (vec3 0.155 0.045 0.145))))
    (gnome-smooth-union (gnome-smooth-union mass tip 0.11) moustache 0.05)))

(define-shader-function gnome-robe-distance (point)
  "The robe, flaring to a hem that rests on the ground, and two mitten hands.

The hem stops just above his boots.  An earlier version ran the cone's lower
cap down past the feet, and the terrain cut it off in a dead straight line:
he looked less like he was standing on the world than pasted onto it."
  (let* ((robe (gnome-round-cone-distance point 0.30 0.80 0.30 0.21))
         (mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (mitten (gnome-sphere-distance mirrored
                                        (vec3 0.30 0.50 0.115) 0.09)))
    (gnome-smooth-union robe mitten 0.05)))

(define-shader-function gnome-boot-distance (point)
  "Two small boots, toes forward, mirrored across the body's plane."
  (let* ((mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z))))
    (gnome-ellipsoid-distance mirrored (vec3 0.13 0.055 0.115)
                              (vec3 0.095 0.055 0.145))))

(define-shader-function gnome-distance (point)
  "The whole fellow: five parts welded with fillets of different sizes.

The robe-to-head weld is generous because a gnome has no neck to speak of;
the hat sits on the brim with almost no fillet at all, because a hat is worn
rather than grown."
  (let* ((robe (gnome-robe-distance point))
         (skin (gnome-skin-distance point))
         (beard (gnome-beard-distance point))
         (hat (gnome-hat-distance point))
         (boots (gnome-boot-distance point)))
    (gnome-smooth-union
     (gnome-smooth-union
      (gnome-smooth-union (gnome-smooth-union robe skin 0.085) beard 0.045)
      hat 0.02)
     boots 0.03)))

(define-shader-function gnome-normal (point)
  "The gradient by the tetrahedron trick: four samples rather than six."
  (let* ((reach 0.0016)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (gnome-distance (+ point (* a reach))))
           (* b (gnome-distance (+ point (* b reach)))))
        (+ (* c (gnome-distance (+ point (* c reach))))
           (* d (gnome-distance (+ point (* d reach)))))))))

;;; ---------------------------------------------------------------------
;;; What he is made of

(define-shader-function gnome-face-detail (point albedo skin-weight)
  "Eyes, brows and a little colour in the cheeks, painted on the skin only.

The features are placed by *direction* from the centre of the head rather
than by distance to a point in space.  Placing them by point failed: the
welded surface stands a fillet's width outside the head sphere the markers
were measured against, so every feature sat just under the skin and the
gnome came out blank-faced.  A direction does not care how far out the
surface ended up.

The eyes are small and dark and the brows are heavy.  Resisting the urge to
enlarge the eyes is the whole difference between a friendly little fellow
and a cartoon: he should look like he is thinking about you, not begging for
something."
  (let* ((mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (direction (normalize (- mirrored (vec3 0.0 0.90 0.0))))
         (eye (gnome-length (- direction (normalize (vec3 0.52 0.04 0.85)))))
         (pupil (- 1.0 (smoothstep 0.100 0.135 eye)))
         (socket (- 1.0 (smoothstep 0.135 0.310 eye)))
         (glint (- 1.0 (smoothstep 0.035 0.060
                                   (gnome-length
                                    (- direction
                                       (normalize (vec3 0.47 0.12 0.87)))))))
         ;; The brow is measured in a squashed metric: wide, low and shallow,
         ;; which is a brow, where an unsquashed disc would be a wart.
         (brow (- 1.0 (smoothstep 0.115 0.190
                                  (gnome-length
                                   (* (- direction
                                         (normalize (vec3 0.53 0.21 0.82)))
                                      (vec3 0.45 1.9 0.9))))))
         (cheek (- 1.0 (smoothstep 0.180 0.400
                                   (gnome-length
                                    (- direction
                                       (normalize (vec3 0.80 -0.22 0.56)))))))
         (shaded (mix albedo (* albedo (vec3 0.62 0.55 0.52))
                      (* socket (* skin-weight 0.32))))
         (blushed (mix shaded (vec3 0.82 0.40 0.33)
                       (* cheek (* skin-weight 0.30))))
         (eyed (mix blushed (vec3 0.035 0.030 0.042)
                    (* pupil skin-weight)))
         (browed (mix eyed (vec3 0.80 0.79 0.76)
                      (* brow (* skin-weight 0.9))))
         (lit (mix browed (vec3 0.95 0.95 0.95)
                   (* glint (* skin-weight 0.9)))))
    lit))

(define-shader-function gnome-albedo (point)
  "The nearest part decides the colour.

A muted, slightly earthy palette on purpose: a scarlet hat over a slate robe
reads as someone who has been standing in a garden for a long time, where
saturated primaries would read as a toy."
  (let* ((robe (gnome-robe-distance point))
         (skin (gnome-skin-distance point))
         (beard (gnome-beard-distance point))
         (hat (gnome-hat-distance point))
         (boots (gnome-boot-distance point))
         ;; Three grains, each sampled in a domain stretched along the way
         ;; that material actually runs: the beard downward in strands, the
         ;; robe in a coarse weave, the hat in fine felt.  Without them
         ;; everything the sphere tracer returns is the same flawless vinyl,
         ;; and a gnome who has stood in a garden for two hundred years
         ;; should not look like he came out of a mould this morning.
         (strand (- (lattice-noise (* point (vec3 34.0 7.0 34.0))) 0.5))
         (weave (- (lattice-noise (* point (vec3 15.0 15.0 15.0))) 0.5))
         (felt (- (lattice-noise (* point (vec3 24.0 24.0 24.0))) 0.5))
         (robe-color (* (vec3 0.055 0.145 0.245) (+ 1.0 (* weave 0.30))))
         (skin-color (vec3 0.72 0.47 0.35))
         (beard-color (* (vec3 0.80 0.79 0.755) (+ 1.0 (* strand 0.22))))
         (hat-color (* (vec3 0.52 0.075 0.085) (+ 1.0 (* felt 0.20))))
         (boot-color (* (vec3 0.105 0.075 0.055) (+ 1.0 (* weave 0.35))))
         ;; A running nearest-so-far, folded by hand: STEP answers one when
         ;; the candidate is at least as near, and MIX takes it.
         (near-skin (step skin robe))
         (color-1 (mix robe-color skin-color near-skin))
         (distance-1 (min robe skin))
         (near-beard (step beard distance-1))
         (color-2 (mix color-1 beard-color near-beard))
         (distance-2 (min distance-1 beard))
         (near-hat (step hat distance-2))
         (color-3 (mix color-2 hat-color near-hat))
         (distance-3 (min distance-2 hat))
         (near-boots (step boots distance-3))
         (color-4 (mix color-3 boot-color near-boots))
         ;; How much of this point is face, for the details painted on it.
         (skin-weight
           (- 1.0 (smoothstep 0.0 0.035 (- skin (min distance-3 boots))))))
    (gnome-face-detail point color-4 skin-weight)))

;;; ---------------------------------------------------------------------
;;; The pipeline stages

(define-shader-method shader-specification-for
    gnome-sdf-vertex-specification
    ((role (eql :gnome-sdf)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (sphere-center-radius :vec4 :location 1))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position :vec3 :location 0)
               (sphere-output :vec4 :location 1))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle sphere-center-radius :xyz))
         (radius (swizzle sphere-center-radius :w))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (world-position
           (+ center (+ (* right (* corner-x radius))
                        (* up (* corner-y radius)))))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (z-scale (representation (swizzle projection-vector :z)))
         (z-offset (representation (swizzle projection-vector :w))))
    (set-output clip-position
                (vec4 (* view-x x-scale)
                      (- (* view-y y-scale))
                      (+ (* view-z z-scale) z-offset)
                      view-z))
    (set-output proxy-world-position world-position)
    (set-output sphere-output sphere-center-radius)))

(define-shader-method shader-specification-for
    gnome-sdf-fragment-specification
    ((role (eql :gnome-sdf)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (sphere-input :vec4 :location 1))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (center (swizzle sphere-input :xyz))
         (radius (swizzle sphere-input :w))
         (ray (normalize (- proxy-world-position camera)))
         ;; The figure's own upright frame: world up stays up, and +z points
         ;; along the ground toward the eye, so he faces whoever addresses
         ;; him.  RIGHT is the cross product of world up with FACING, written
         ;; out by hand.
         (toward-eye (- camera center))
         (facing (normalize (vec3 (swizzle toward-eye :x)
                                  0.0
                                  (swizzle toward-eye :z))))
         (sideways (vec3 (swizzle facing :z) 0.0 (- (swizzle facing :x))))
         ;; The gnome stands with his feet at local y zero; the instance
         ;; centre is the middle of his bounding sphere, 0.85 above them.
         (local-camera (vec3 (dot toward-eye sideways)
                             (+ (swizzle toward-eye :y) 0.85)
                             (dot toward-eye facing)))
         (local-ray (vec3 (dot ray sideways)
                          (swizzle ray :y)
                          (dot ray facing)))
         ;; Enter and leave through the bounding sphere analytically, so the
         ;; fixed step count is spent on the body rather than on the approach.
         (to-center (- local-camera (vec3 0.0 0.85 0.0)))
         (half-way (- (dot to-center local-ray)))
         (gap (- (dot to-center to-center) (* radius radius)))
         (discriminant (- (* half-way half-way) gap))
         (span (sqrt (max discriminant 0.0)))
         (entry (max (- half-way span) 0.0))
         (exit (+ half-way span))
         (travel
           (counted-fold (march 64.0 ray-distance entry)
             (let* ((point (+ local-camera (* local-ray ray-distance)))
                    (distance (gnome-distance point)))
               (if (< distance 0.0009)
                   ray-distance
                   (if (> ray-distance exit)
                       ray-distance
                       ;; The hat's domain warp makes the field slightly
                       ;; non-metric; a shortened step keeps the march honest.
                       (+ ray-distance (max (* distance 0.85) 0.0009)))))))
         (point (+ local-camera (* local-ray travel)))
         (surface-distance (gnome-distance point))
         (coverage (* (- 1.0 (step 0.0035 surface-distance))
                      (- 1.0 (step 0.0 (- discriminant)))))
         (local-normal (gnome-normal point))
         (normal (+ (* sideways (swizzle local-normal :x))
                    (+ (vec3 0.0 (swizzle local-normal :y) 0.0)
                       (* facing (swizzle local-normal :z)))))
         (albedo (gnome-albedo point))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         ;; Wrapped diffuse: light bends a little past the terminator, which
         ;; is what keeps a small round person from looking like a billiard
         ;; ball with one hard shadow across the face.
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.35) 1.35)))
         ;; The sky above contributes more to what points up at it.
         (sky (+ 0.55 (* 0.45 (swizzle normal :y))))
         ;; A crude ambient occlusion: what is low and inward is darker.
         (occlusion (clamp (+ 0.35 (* 0.65 (smoothstep -0.15 0.35
                                                       (swizzle point :y))))
                           0.35 1.0))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         (rim (expt (- 1.0 view-facing) 3.5))
         (halfway (normalize (- sun-direction ray)))
         (specular (* 0.12 (expt (max 0.0 (dot normal halfway)) 28.0)))
         (illumination
           (+ (* ambient (* sky occlusion))
              (* sun-color (+ 0.12 (* wrapped 1.15)))))
         (radiance
           (+ (+ (* albedo illumination)
                 (* sun-color (* specular coverage)))
              ;; The rim is the mystery: a thin warm outline that separates
              ;; him from whatever he is standing in front of.
              (* (vec3 0.95 0.72 0.44) (* rim 0.30)))))
    ;; The scene target uses premultiplied alpha.  Misses leave no rectangular
    ;; trace of the conservative billboard.
    (set-output color-output (vec4 (* radiance coverage) coverage))))
