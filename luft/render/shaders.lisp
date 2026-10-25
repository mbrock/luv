(in-package #:luft.render.shaders)

;;; Site-stream rendering. One UVec4 instance selects a lattice base and a
;;; canonical fixed-stride template; the template vertex is a small exact
;;; offset plus geometric attributes. The CPU classifies sites and the vertex
;;; shader realizes the renderer-global triangle and quad populations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *screen-ambient-occlusion-strength* 0.38)
  (defvar *screen-ambient-occlusion-radius* 0.95)
  (defvar *ambient-pigment-strength* 0.82)
  (defvar *voxel-light-gain* 0.7
    "Radiance of full terrain voxel light on a surface of unit tone."))

(luv.knobs:define-knob voxel-light-gain
    (:group :grading :class 'luv.knobs:rebuild-knob
     :quantity (:quantity quantities:scene-radiance :unit :one)
     :minimum 0.0 :maximum 3.0 :step 0.05)
  *voxel-light-gain*)

(define-shader-function unpack-terrain-tone (packed)
  "Decode one RGB8 terrain descriptor into scene-linear colour."
  (vec3 (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :red) packed))
           #.(luft.terrain::terrain-descriptor-divisor :red))
        (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :green) packed))
           #.(luft.terrain::terrain-descriptor-divisor :green))
        (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :blue) packed))
           #.(luft.terrain::terrain-descriptor-divisor :blue))))

(define-shader-function terrain-material-sample
    (code selected descriptor normal)
  "Return one selected tone and its unit weight, or the additive identity."
  (let* ((upness (swizzle (representation normal) :z))
         (packed
           (if (> upness 0.35) (swizzle descriptor :x)
               (if (< upness -0.35) (swizzle descriptor :z)
                   (swizzle descriptor :y)))))
    (if (> selected (uint 0.0))
        (if (> code (uint 0.0))
            (vec4 (unpack-terrain-tone packed) 1.0)
            (vec4 0.0 0.0 0.0 0.0))
        (vec4 0.0 0.0 0.0 0.0))))

;;; Material surfaces (#HYBFRT).  A descriptor's fourth word names a relief
;;; field, its strength, a roughness, and how strongly the material answers
;;; the stars' edges.  The vertex stage picks the word of the first material
;;; it samples and reads the edge's convexity from the record's occupied and
;;; empty cell masks; the fragment stage cuts the relief in world space, so it
;;; runs across cell boundaries instead of repeating on every face.

(define-shader-function terrain-code-pick (current code selected)
  "Keep CURRENT once chosen, else CODE when it is a sampled material."
  (if (> current (uint 0.0))
      current
      (if (> selected (uint 0.0)) code (uint 0.0))))

(define-shader-function cell-mask-count (mask)
  "The number of cells named by an eight-bit sample MASK."
  (float (+ (ldb (byte 1 0) mask) (ldb (byte 1 1) mask)
            (ldb (byte 1 2) mask) (ldb (byte 1 3) mask)
            (ldb (byte 1 4) mask) (ldb (byte 1 5) mask)
            (ldb (byte 1 6) mask) (ldb (byte 1 7) mask))))

(define-shader-function terrain-surface-vector (word occupied empty)
  "Unpack surface WORD: relief code, strength, roughness, and signed wear.

The wear's sign is the edge's: a face has as many occupied as empty cells, a
convex edge or corner fewer occupied, and a concave one more."
  (let* ((occupied-count (cell-mask-count occupied))
         (incident (max 1.0 (+ occupied-count (cell-mask-count empty))))
         (convexity (clamp (* 4.0 (- 0.5 (/ occupied-count incident))) -1.0 1.0)))
    (assume-quantity
     (vec4 (float (ldb #.(luft.terrain::terrain-descriptor-byte :relief-code) word))
           (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :strength) word))
              #.(luft.terrain::terrain-descriptor-divisor :strength))
           (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :roughness) word))
              #.(luft.terrain::terrain-descriptor-divisor :roughness))
           (* convexity (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :wear) word))
                           #.(luft.terrain::terrain-descriptor-divisor :wear))))
     :components #.(luft.terrain::terrain-varying-components :surface))))

(define-shader-function paper-hash (site)
  (let* ((scattered (fract (* site 0.1031)))
         (shift (dot scattered
                     (+ (swizzle scattered :zyx)
                        (vec3 31.32 31.32 31.32))))
         (folded (+ scattered (vec3 shift shift shift))))
    (fract (* (+ (swizzle folded :x) (swizzle folded :y))
              (swizzle folded :z)))))

(define-shader-function paper-noise (point)
  (let* ((lattice (floor point))
         (offset (fract point))
         (weight (* offset (* offset (- (vec3 3.0 3.0 3.0)
                                        (* offset 2.0)))))
         (u (swizzle weight :x))
         (v (swizzle weight :y))
         (w (swizzle weight :z))
         (near-low (mix (paper-hash lattice)
                        (paper-hash (+ lattice (vec3 1.0 0.0 0.0))) u))
         (near-high (mix (paper-hash (+ lattice (vec3 0.0 1.0 0.0)))
                         (paper-hash (+ lattice (vec3 1.0 1.0 0.0))) u))
         (far-low (mix (paper-hash (+ lattice (vec3 0.0 0.0 1.0)))
                       (paper-hash (+ lattice (vec3 1.0 0.0 1.0))) u))
         (far-high (mix (paper-hash (+ lattice (vec3 0.0 1.0 1.0)))
                        (paper-hash (+ lattice (vec3 1.0 1.0 1.0))) u)))
    (mix (mix near-low near-high v) (mix far-low far-high v) w)))

(define-shader-function terrain-pattern-vector (word)
  "Unpack pattern WORD: shape, rows per cell, element length in rows, mortar."
  (assume-quantity
   (vec4 (float (ldb #.(luft.terrain::terrain-descriptor-byte :pattern-code) word))
         (float (ldb #.(luft.terrain::terrain-descriptor-byte :courses) word))
         (* #.(/ (luft.terrain::terrain-descriptor-divisor :aspect))
            (float (ldb #.(luft.terrain::terrain-descriptor-byte :aspect) word)))
         (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :mortar-width) word))
            #.(luft.terrain::terrain-descriptor-divisor :mortar-width)))
   :components #.(luft.terrain::terrain-varying-components :pattern)))

;;; Laid patterns.  Bricks, tiles, and shingles are laid out in the face's
;;; own world coordinates: along a wall and up it, or across a floor.  Rows
;;; therefore run the whole length of a wall and meet at its corners, and
;;; every element has its own identity to vary its tone by.  The shapes
;;; follow +MATERIAL-PATTERNS+: none, bond, grid, shingle, checker, and the
;;; face-aware shelf, window, timbered, and panel (FACE-LAID-PATTERN).

(define-shader-function laid-pattern (pattern normal world)
  "Return the element mask, the element's hash, its height ramp, and parity."
  (let* ((shape (swizzle pattern :x))
         (courses (max 1.0 (swizzle pattern :y)))
         (aspect (max 0.5 (swizzle pattern :z)))
         (mortar (* 0.5 (swizzle pattern :w)))
         (an (abs normal))
         (top (> (swizzle an :z) 0.7))
         (x-facing (> (swizzle an :x) (swizzle an :y)))
         (u (if top (swizzle world :x)
                (if x-facing (swizzle world :y) (swizzle world :x))))
         (v (if top (swizzle world :y) (swizzle world :z)))
         (w (if top (swizzle world :z)
                (if x-facing (swizzle world :x) (swizzle world :y))))
         (rowf (* v courses))
         (row (floor rowf))
         (fv (fract rowf))
         (parity (* 2.0 (fract (* row 0.5))))
         (shift (if (< shape 1.5) (* 0.5 parity)
                    (if (> shape 2.5) (if (< shape 3.5) (* 0.5 parity) 0.0) 0.0)))
         (uf (+ (/ (* u courses) aspect) shift))
         (col (floor uf))
         (fu (fract uf))
         (du (* aspect (min fu (- 1.0 fu))))
         (dv (min fv (- 1.0 fv)))
         (bevel 0.08)
         (joint (smoothstep mortar (+ mortar bevel) du))
         (bed (smoothstep mortar (+ mortar bevel) dv))
         (shingle (> shape 2.5))
         (checker (> shape 3.5))
         (mask (if checker (* joint bed)
                   (if shingle (* joint (smoothstep 0.0 (+ mortar bevel) fv))
                       (* joint bed))))
         (ramp (if checker 1.0 (if shingle (- 1.0 (* 0.5 fv)) 1.0)))
         (hash (paper-hash (vec3 col row (floor (+ w 0.5)))))
         (checked (* 2.0 (fract (* (+ row col) 0.5)))))
    (if (< shape 0.5)
        (vec4 1.0 0.5 1.0 0.0)
        (if (> shape 4.5)
            (face-laid-pattern pattern normal world)
            (vec4 mask hash ramp checked)))))

;;; Face-aware patterns lay a block's side faces as its front -- books on
;;; shelves, panes in a frame, posts and braces, a raised panel -- and its
;;; top and bottom as the plain wood they would be.  Each is laid per cell,
;;; like the block it draws, and returns its height, the element's own
;;; variation, the frame's weight, and the recess's.

(define-shader-function face-coordinates (normal world)
  "A face's along, up, and depth coordinates, as LAID-PATTERN lays them."
  (let* ((an (abs normal))
         (top (> (swizzle an :z) 0.7))
         (x-facing (> (swizzle an :x) (swizzle an :y))))
    (vec3 (if top (swizzle world :x)
              (if x-facing (swizzle world :y) (swizzle world :x)))
          (if top (swizzle world :y) (swizzle world :z))
          (if top (swizzle world :z)
              (if x-facing (swizzle world :x) (swizzle world :y))))))

(define-shader-function face-band (distance width soft)
  "One within WIDTH of an edge DISTANCE away, fading to zero over SOFT."
  (- 1.0 (smoothstep width (+ width soft) distance)))

(define-shader-function cell-edge-distance (face)
  "How far FACE's point is from the nearest edge of its cell's face."
  (let* ((fu (fract (swizzle face :x)))
         (fv (fract (swizzle face :y))))
    (min (min fu (- 1.0 fu)) (min fv (- 1.0 fv)))))

(define-shader-function board-face (face)
  "Boards four to a cell, with a groove between each."
  (let* ((rowf (* (swizzle face :y) 4.0))
         (fv (fract rowf))
         (groove (face-band (min fv (- 1.0 fv)) 0.03 0.03)))
    (vec4 (- 1.0 (* 0.7 groove))
          (paper-hash (vec3 (floor rowf) (floor (+ (swizzle face :z) 0.5)) 3.0))
          0.0 (* 0.6 groove))))

(define-shader-function shelf-face (pattern face)
  "A bookcase side: COURSES shelves of books, ASPECT to a row's height.

The variation is the book's hash, plus one where a gilt band crosses it."
  (let* ((rows (max 1.0 (swizzle pattern :y)))
         (books (* rows (max 0.5 (swizzle pattern :z))))
         (board (swizzle pattern :w))
         (u (swizzle face :x))
         (depth (floor (+ (swizzle face :z) 0.5)))
         (rowf (* (swizzle face :y) rows))
         (row (floor rowf))
         (fv (fract rowf))
         (cell-u (fract u))
         (upright (face-band (min cell-u (- 1.0 cell-u)) 0.03 0.015))
         (shelf (max (face-band fv board 0.04)
                     (face-band (- 1.0 fv) (* 0.4 board) 0.04)))
         (frame (max upright shelf))
         ;; A slot holds one book, or two thin ones.
         (slotf (* u books))
         (slot (floor slotf))
         (fs (fract slotf))
         (pair (if (< (paper-hash (vec3 slot row (+ depth 17.0))) 0.35) 2.0 1.0))
         (sub (floor (* fs pair)))
         (fb (fract (* fs pair)))
         (id (paper-hash (vec3 (+ (* slot 2.0) sub) row depth)))
         (clear (/ (- fv board) (- 1.0 (* 1.4 board))))
         (crown (+ 0.55 (* 0.4 (fract (* id 13.7)))))
         (above (smoothstep (- crown 0.02) (+ crown 0.02) clear))
         (seam (face-band (/ (min fb (- 1.0 fb)) (* books pair)) 0.004 0.004))
         (recess (* (- 1.0 frame) (max above seam)))
         (across (- (* 2.0 fb) 1.0))
         (spine (+ 0.55 (* 0.2 (- 1.0 (* across across)))))
         (gilt (if (< (abs (- clear (* 0.82 crown))) 0.025)
                   (if (> (fract (* id 5.3)) 0.4) 1.0 0.0)
                   0.0)))
    (vec4 (mix (mix spine 0.05 recess) 1.0 frame) (+ id gilt) frame recess)))

(define-shader-function window-face (pattern face)
  "COURSES by COURSES panes behind mullions in a grooved frame.

The variation is the pane's lamplight, brighter toward its lower edge."
  (let* ((panes (max 1.0 (swizzle pattern :y)))
         (width (swizzle pattern :w))
         (edge (cell-edge-distance face))
         (frame (face-band edge width 0.012))
         (groove (* frame (face-band (abs (- edge (* 0.5 width))) 0.01 0.008)))
         (pu (fract (* (swizzle face :x) panes)))
         (pv (fract (* (swizzle face :y) panes)))
         (mullion (face-band (/ (min (min pu (- 1.0 pu)) (min pv (- 1.0 pv)))
                                panes)
                             0.02 0.01))
         (bar (max frame mullion))
         (lamp (paper-hash (vec3 (floor (* (swizzle face :x) panes))
                                 (floor (* (swizzle face :y) panes))
                                 (+ (floor (+ (swizzle face :z) 0.5)) 5.0))))
         (light (clamp (+ 0.3 (* 0.5 (- 1.0 pv)) (* 0.4 (- lamp 0.5))) 0.0 1.0)))
    (vec4 (mix 0.2 (mix (mix 0.85 1.0 frame) 0.45 groove) bar) light bar groove)))

(define-shader-function timbered-face (pattern face)
  "Infill between posts at the cell's edges and a brace across it."
  (let* ((width (* 0.5 (swizzle pattern :w)))
         (fu (fract (swizzle face :x)))
         (fv (fract (swizzle face :y)))
         (post (face-band (cell-edge-distance face) width 0.015))
         (parity (fract (* 0.5 (+ (floor (swizzle face :x))
                                  (floor (swizzle face :y))))))
         (diagonal (if (> parity 0.25) (abs (- fu fv)) (abs (- (+ fu fv) 1.0))))
         (brace (face-band (* 0.7071 diagonal) width 0.015))
         (frame (max post brace)))
    (vec4 (mix (+ 0.35 (* 0.2 (paper-noise (* face 7.0)))) 0.95 frame)
          0.5 frame 0.0)))

(define-shader-function panel-face (pattern face)
  "A raised field inside a stile-and-rail frame, one to a cell."
  (let* ((width (swizzle pattern :w))
         (edge (cell-edge-distance face))
         (frame (face-band edge width 0.03))
         (bevel (smoothstep width (+ width 0.12) edge)))
    (vec4 (mix (mix 0.55 0.85 bevel) 0.95 frame)
          (paper-hash (vec3 (floor (swizzle face :x)) (floor (swizzle face :y))
                            (floor (+ (swizzle face :z) 0.5))))
          frame 0.0)))

(define-shader-function mineral-pattern (pattern normal world)
  "Face-local warped veins, or world-space dark mineral inclusions."
  (let* ((uv (* (fract (swizzle (face-coordinates normal world) :xy)) 16.0))
         (x (swizzle uv :x))
         (y (swizzle uv :y))
         (phase (+ (* 0.55 x) (* 0.85 y) (* 2.2 (sin (* 0.45 y)))
                   (* 1.3 (sin (* 0.6 x)))))
         (ink (if (< (swizzle pattern :x) 14.5)
                  (max 0.0 (- 1.0 (* 2.2 (abs (sin (* 0.5 phase))))))
                  (step 0.85 (paper-hash (floor (* world (* 4.75 (swizzle pattern :y)))))))))
    (vec4 1.0 ink 0.0 0.0)))

(define-shader-function face-laid-pattern (pattern normal world)
  "Return a face-aware pattern's height, variation, frame, and recess."
  (let* ((shape (swizzle pattern :x))
         (face (face-coordinates normal world))
         (top (> (abs (swizzle normal :z)) 0.7)))
    (if (> shape 8.5)
        (if (> shape 13.5) (mineral-pattern pattern normal world)
            (motif-face shape face top))
        (if top
            (if (< shape 5.5)
                (board-face face)
                (if (< shape 7.5) (vec4 1.0 0.5 1.0 0.0) (vec4 0.8 0.5 0.0 0.0)))
            (if (< shape 5.5)
                (shelf-face pattern face)
                (if (< shape 6.5)
                    (window-face pattern face)
                    (if (< shape 7.5)
                        (timbered-face pattern face)
                        (panel-face pattern face))))))))

(define-shader-function motif-face (shape face top)
  "Cell-sized motifs: height, accent mask, frame mask, and recess mask.
Only arithmetic; no extra noise or texture samples in these laid shapes."
  (let* ((uv (fract (swizzle face :xy)))
         (q (- uv (vec2 0.5 0.5)))
         (x (swizzle q :x)) (y (swizzle q :y))
         (edge (max (abs x) (abs y)))
         (frame (smoothstep 0.34 0.40 edge))
         (r2 (dot q q)))
    (if (< shape 9.5)
        ;; A recessed glass screen, broken rows of phosphor, and a bezel.
        (let* ((row (floor (* (swizzle uv :y) 12.0)))
               (ink (* (- 1.0 frame)
                       (if top 0.0 1.0)
                       (step 0.35 (fract (* (swizzle uv :y) 12.0)))
                       (step 0.3 (fract (+ (* (swizzle uv :x) 9.0)
                                          (* row 0.37)))))))
          (vec4 (+ 0.4 (* 0.5 frame)) ink frame 0.0))
        (if (< shape 10.5)
            ;; A metal flange: central hub, dark annulus, and four spokes.
            (let* ((hub (- 1.0 (smoothstep 0.012 0.025 r2)))
                   (rim (smoothstep 0.14 0.18 r2))
                   (spoke (- 1.0 (smoothstep 0.035 0.065
                                                  (min (abs x) (abs y)))))
                   (metal (* (- 1.0 (smoothstep 0.18 0.20 r2))
                             (max hub (max rim spoke)))))
              (vec4 (+ 0.35 (* 0.55 metal)) metal 0.0 0.0))
            (if (< shape 11.5)
                ;; A large woven lozenge survives after the nap filters out.
                (let* ((diamond (+ (abs x) (abs y)))
                       (ink (* (smoothstep 0.18 0.23 diamond)
                               (- 1.0 (smoothstep 0.34 0.39 diamond)))))
                  (vec4 0.85 ink (smoothstep 0.43 0.47 (abs x)) 0.0))
                (if (< shape 12.5)
                    ;; Two broad bindings across a bale, on every face.
                    (let* ((band (- 1.0 (smoothstep
                                        0.035 0.065 (abs (- (abs x) 0.25))))))
                      (vec4 (- 0.9 (* band 0.25)) 0.0 band 0.0))
                    ;; A stone basin: water/lava only inside its upper rim.
                    (vec4 (+ 0.35 (* 0.6 frame))
                          (if top (- 1.0 frame) 0.0)
                          (if top frame 1.0) 0.0)))))))

(define-shader-function book-tone (accent base hash)
  "A binding: ACCENT's hue turned by HASH, or BASE's leather."
  (let* ((k (floor (* hash 6.0))))
    (if (< k 0.5) accent
        (if (< k 1.5) (swizzle accent :yxz)
            (if (< k 2.5) (swizzle accent :zyx)
                (if (< k 3.5) (* (+ accent (swizzle accent :yxz)) 0.6)
                    (if (< k 4.5) (* (+ accent (swizzle accent :zyx)) 0.6)
                        (* base 0.55))))))))

(define-shader-function face-element-tone (shape top variation tone accent variance)
  "The tone of a face-aware pattern's element: book, glass, infill, or field."
  (let* ((own (fract variation))
         (jitter (+ 1.0 (* 2.0 variance (- own 0.5)))))
    (if (> shape 8.5)
        (mix tone accent (clamp variation 0.0 1.0))
        (if top
            (* tone jitter)
            (if (< shape 5.5)
                (mix (* (mix (book-tone accent tone own) tone 0.3)
                        (+ 1.0 (* 2.0 variance (- (fract (* own 7.31)) 0.5))))
                     (vec3 0.55 0.38 0.12)
                     (* 0.7 (floor variation)))
                (if (< shape 6.5)
                    ;; Glass is dark by day; its lamplight is emitted instead.
                    (* accent 0.3)
                    (if (< shape 7.5) accent (* tone jitter))))))))

(define-shader-function face-pattern-tone (pattern laid normal tone mortar accent)
  "The tone of a face-aware pattern: element, recess, and frame."
  (let* ((element (face-element-tone (swizzle pattern :x)
                                     (> (abs (swizzle normal :z)) 0.7)
                                     (swizzle laid :y) tone
                                     (swizzle accent :xyz) (swizzle mortar :w)))
         (frame-tone (swizzle mortar :xyz)))
    (mix (mix element (* frame-tone 0.2) (swizzle laid :w))
         frame-tone (swizzle laid :z))))

(define-shader-function face-pattern-glow (pattern laid normal accent fade)
  "A window's lamplight through its panes; a far one glows on average."
  (let* ((shape (swizzle pattern :x))
         (lit (if (> shape 8.5)
                  (if (< shape 9.5) (swizzle laid :y)
                      (if (> shape 12.5) (swizzle laid :y) 0.0))
                  (if (> shape 5.5)
                      (if (< shape 6.5)
                          (if (< (abs (swizzle normal :z)) 0.7) 1.0 0.0)
                          0.0)
                      0.0))))
    (* (swizzle accent :xyz)
       (* lit (swizzle accent :w)
          (mix 0.5 (* (- 1.0 (swizzle laid :z)) (+ 0.15 (* 1.1 (swizzle laid :y))))
               fade)))))

(define-shader-function pattern-fineness (pattern)
  "Rows per cell, or for shelves books per cell over two, to fade it by."
  (let* ((shape (swizzle pattern :x)))
    (if (> shape 4.5)
        (if (< shape 5.5)
            (* 0.5 (swizzle pattern :y) (swizzle pattern :z))
            (swizzle pattern :y))
        (swizzle pattern :y))))

(defvar *terrain-screen-filter-p* nil
  "Band-limit the game's procedural materials using actual raster footprints.")

(define-shader-function material-pixel-footprint (world)
  (let* ((dx (derivative-x world)) (dy (derivative-y world)))
    (sqrt (max (dot dx dx) (dot dy dy)))))

(define-shader-function material-detail-weight (world frequency)
  (- 1.0 (smoothstep 0.35 1.0 (* frequency (material-pixel-footprint world)))))

(define-shader-abstraction filtered-material-detail (weight world frequency)
  (if *terrain-screen-filter-p*
      `(* ,weight (material-detail-weight ,world ,frequency))
      weight))

(define-shader-function material-relief-frequency (code)
  (if (< code 1.5) 13.0
      (if (< code 2.5) 7.0
          (if (< code 3.5) 9.0
              (if (< code 4.5) 5.5
                  (if (< code 5.5) 14.0
                      (if (< code 8.5) 4.0
                          (if (< code 9.5) 8.0
                              (if (< code 11.5) 19.0 24.0)))))))))

(define-shader-function surface-height (code pattern point fade nap-fade laid)
  "The relief CODE at POINT, cut by PATTERN's elements, faded by FADE."
  (let* ((raw (mix 0.5 (material-relief-height code point)
                    (filtered-material-detail 1.0 point (material-relief-frequency code))))
         ;; Filter the woven nap and fine straw along with the laid joints.
         (relief (if (> code 8.5)
                     (if (< code 10.5) (mix 0.5 raw nap-fade) raw)
                     raw))
         (cut (* (swizzle laid :x)
                 (if (> (swizzle pattern :x) 4.5) 1.0 (swizzle laid :z))
                 (+ 0.75 (* 0.25 relief)))))
    (if (< (swizzle pattern :x) 0.5)
        relief
        (mix relief cut fade))))

(define-shader-function ridged-noise (point)
  (- 1.0 (abs (- (* (paper-noise point) 2.0) 1.0))))

(define-shader-function plank-height (point)
  "Board grooves every quarter cell, with a grain running along each board."
  (let* ((u (fract (* (+ (swizzle point :x) (swizzle point :y)) 4.0))))
    (* (smoothstep 0.0 0.1 u)
       (- 1.0 (smoothstep 0.9 1.0 u))
       (+ 0.75 (* 0.25 (paper-noise (* point (vec3 1.0 1.0 9.0))))))))

(define-shader-function thread-bump (offset)
  "A thread's rounded cross-section across OFFSET in [0, 1]."
  (* 4.0 offset (- 1.0 offset)))

(define-shader-function cloth-thread (across-a across-b along over)
  "A thread's height: round across ACROSS-A and ACROSS-B, lifted ALONG its
length where it rides over a crossing, by how far OVER it rides."
  (* (thread-bump across-a) (thread-bump across-b)
     (+ 0.55 (* 0.45 (thread-bump along)))
     (+ 0.7 (* 0.3 over))))

(define-shader-function cloth-height (point)
  "A woven cloth: eight threads a cell each way, over and under.

Threads run along all three axes; the lattice is offset half a thread so a
face's own axis sits at a crossing's crown, where its threads are lower than
the face's two, and drop out of the maximum.  Alternating crossings lift the
warp then the weft. Distant and grazing nap is filtered by the caller."
  (let* ((q (+ (* point 8.0) (vec3 0.5 0.5 0.5)))
         (cell (floor q))
         (offset (fract q))
         (x (swizzle offset :x))
         (y (swizzle offset :y))
         (z (swizzle offset :z))
         (over (* 2.0 (fract (* 0.5 (+ (swizzle cell :x) (swizzle cell :y)
                                     (swizzle cell :z))))))
         (along-x (cloth-thread y z x over))
         (along-y (cloth-thread x z y (- 1.0 over)))
         (along-z (cloth-thread x y z over)))
    (+ 0.1 (* 0.8 (max along-x (max along-y along-z))))))

(define-shader-function fiber-height (code point)
  "Straw's broken stems, cactus ribs, or brushed metal; one noise at most."
  (let* ((u (+ (swizzle point :x) (swizzle point :y)))
         (z (swizzle point :z)))
    (if (< code 10.5)
        (+ (* 0.7 (thread-bump (fract (+ (* u 17.0) (* z 1.3)))))
           (* 0.3 (paper-noise (* point (vec3 19.0 19.0 3.0)))))
        (if (< code 11.5)
            (+ (* 0.8 (thread-bump (fract (* u 5.0))))
               (* 0.2 (step 0.85 (fract (+ (* u 13.0) (* z 7.0))))))
            (+ 0.45 (* 0.1 (thread-bump (fract (* z 24.0)))))))))

(define-shader-function material-relief-height (code point)
  "The height, in [0, 1], of relief field CODE at world POINT.

The codes follow +MATERIAL-RELIEFS+: none, grass, stone, grain, wood,
leaves, planks, snow, gloss, cloth, straw, cactus, metal, faceted."
  (if (or (> code 12.5) (< code 0.5)) 0.5
      (if (< code 1.5)
          ;; Tufts: two scales of soft bumps, the finer one blade-sized.
          (+ (* 0.6 (paper-noise (* point 4.7)))
             (* 0.4 (paper-noise (* point 13.0))))
          (if (< code 2.5)
              ;; Fractured rock: ridged cracks over broad slabs.
              (+ (* 0.55 (ridged-noise (* point 1.9)))
                 (* 0.3 (paper-noise (* point 0.7)))
                 (* 0.15 (paper-noise (* point 7.0))))
              (if (< code 3.5)
                  ;; Grain: sand, dirt, gravel.
                  (+ (* 0.7 (paper-noise (* point 9.0)))
                     (* 0.3 (paper-noise (* point 2.3))))
                  (if (< code 4.5)
                      ;; Bark: ridges running up the trunk.
                      (ridged-noise (* point (vec3 5.5 5.5 0.6)))
                      (if (< code 5.5)
                          ;; Leaves: clumps with dark gaps between them.
                          (+ (* 0.65 (smoothstep 0.3 0.7 (paper-noise (* point 5.0))))
                             (* 0.35 (paper-noise (* point 14.0))))
                          (if (< code 6.5)
                              ;; Planks: board grooves every quarter cell.
                              (plank-height point)
                              (if (< code 7.5)
                                  ;; Snow: soft drifts.
                                  (+ (* 0.7 (paper-noise (* point 1.3)))
                                     (* 0.3 (paper-noise (* point 4.0))))
                                  (if (< code 8.5)
                                      ;; Gloss: nearly smooth.
                                      (paper-noise (* point 2.0))
                                      (if (< code 9.5)
                                          (cloth-height point)
                                          (fiber-height code point))))))))))))

(define-shader-function faceted-tone (tone normal world fade)
  "Bright diagonal facets and a rim on each cell face of a crystal."
  (let* ((uv (fract (swizzle (face-coordinates normal world) :xy)))
         (diagonal (min (abs (- (swizzle uv :x) (swizzle uv :y)))
                        (abs (- (+ (swizzle uv :x) (swizzle uv :y)) 1.0))))
         (facet (- 1.0 (smoothstep 0.0625 0.09375 diagonal)))
         (edge (min (min (swizzle uv :x) (- 1.0 (swizzle uv :x)))
                    (min (swizzle uv :y) (- 1.0 (swizzle uv :y)))))
         (rim (- 1.0 (smoothstep 0.03125 0.0625 edge))))
    (+ tone (* fade (+ (* (vec3 0.0 0.26 0.16) facet)
                       (* (vec3 0.065 0.18 0.08) rim))))))

(define-shader-function cross-product (a b)
  (- (* (swizzle a :yzx) (swizzle b :zxy))
     (* (swizzle a :zxy) (swizzle b :yzx))))

(define-shader-function relief-normal (normal world strength height)
  "Bend NORMAL by the screen-quad gradient of the shared surface HEIGHT."
  ;; Tone and normal share one procedural evaluation (#XDZ78R). Pixel-quad
  ;; differences replace fixed world-space offsets; this is not a band-limit.
  (let* ((dx (derivative-x world))
         (dy (derivative-y world))
         (hx (derivative-x height))
         (hy (derivative-y height))
         (rx (cross-product dy normal))
         (ry (cross-product normal dx))
         (det (dot dx rx)))
    ;; Keep the determinant's sign: mirrored screen bases must bend the
    ;; normal the same way. A degenerate projection has no usable gradient.
    (if (> (abs det) 0.000000000001)
        (normalize (- normal (* (+ (* rx hx) (* ry hy))
                                (/ (* strength 0.09) det))))
        normal)))

(define-shader-function material-specular (normal view sun roughness)
  "A rough dielectric's sun highlight, sharper as ROUGHNESS falls."
  (let* ((halfway (normalize (+ view sun)))
         (x (max 0.0 (dot normal halfway)))
         (x2 (* x x)) (x4 (* x2 x2)) (x8 (* x4 x4))
         (x16 (* x8 x8)) (x64 (* (* x16 x16) (* x16 x16)))
         (smooth (- 1.0 roughness)))
    (* (* smooth smooth) 0.5 (mix x8 x64 smooth)
       (max 0.0 (dot normal sun)))))

(define-shader-function scene-relative-luminance (radiance)
  "Reduce checked linear RGB radiance to its named relative luminance."
  (let* ((radiance
           (interpret radiance :quantity quantities:scene-radiance
                                :unit :one))
         (weights
           (assume-quantity (vec3 0.2126 0.7152 0.0722) :unit :one)))
    (interpret (dot radiance weights)
               :quantity quantities:scene-luminance :unit :one)))

(define-shader-function paper-tonemap (radiance)
  "Cross from scene radiance into bounded display-linear colour."
  (let* ((radiance
           (representation
            (interpret radiance :quantity quantities:scene-radiance
                                 :unit :one)))
         (numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    ;; The fitted polynomial is a transfer function, not homogeneous
    ;; radiometric arithmetic.  Its output starts a distinct colour space.
    (assume-quantity
     (clamp (/ numerator denominator)
            (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))
     :quantity quantities:presented-color :unit :one)))

(define-shader-function paper-grade (color)
  "Keep cool shade and warm paper luminous after highlight compression.

The grade works on a square-root (roughly perceptual) encoding of the
display-linear colour: an S-curve applied to linear values would crush every
shadow, while here it deepens the darks a little and lifts the lights evenly,
which is what makes a printed illustration read as solid."
  (let* ((color
           (representation
            (interpret color :quantity quantities:presented-color
                             :unit :one)))
         (luminance (dot color (vec3 0.2126 0.7152 0.0722)))
         ;; Cool shade, warm light: the two halves of a sunlit page.
         (temperature (smoothstep 0.06 0.62 luminance))
         (split-tone
           (mix (vec3 0.92 0.98 1.09) (vec3 1.06 1.01 0.92)
                temperature))
         (toned (* color split-tone))
         (toned-luminance (dot toned (vec3 0.2126 0.7152 0.0722)))
         (grey (vec3 toned-luminance toned-luminance toned-luminance))
         (saturated (max (+ grey (* (- toned grey) 1.10)) (vec3 0.0 0.0 0.0)))
         (perceptual (sqrt (min saturated (vec3 1.0 1.0 1.0))))
         (curved
           (* perceptual
              (* perceptual
                 (- (vec3 3.0 3.0 3.0) (* perceptual 2.0)))))
         (contrasted (mix perceptual curved 0.38))
         (black (vec3 0.0 0.0 0.0))
         (white (vec3 1.0 1.0 1.0)))
    (assume-quantity
     (clamp (* contrasted contrasted) black white)
     :quantity quantities:presented-color :unit :one)))

(defvar *presentation-grade-function* nil
  "Optional game grade of bounded display-linear colour; NIL keeps paper.")
(defvar *presentation-paper-p* t
  "Select the atelier's AO pigment and screen-locked paper grain at construction.")

(define-shader-abstraction presentation-grade (color)
  (if *presentation-grade-function*
      `(,*presentation-grade-function* ,color)
      `(paper-grade ,color)))

(define-shader-abstraction presentation-medium (paper neutral)
  (if *presentation-paper-p* paper neutral))

(define-shader-function mesh-view-clip
    (point position right up forward projection divisor)
  (let* ((relative (- point (swizzle position :xyz)))
         ;; The view coordinates are checked cell-valued projections.  The
         ;; following homogeneous row is deliberately representation: its Z
         ;; offset and W divisor change meaning with projection mode.
         (view-x (representation
                  (dot relative (swizzle right :xyz))))
         (view-y (representation
                  (dot relative (swizzle up :xyz))))
         (view-z (representation
                  (dot relative (swizzle forward :xyz)))))
    (vec4 (* view-x (swizzle projection :x))
          (- (* view-y (swizzle projection :y)))
          (+ (* view-z (swizzle projection :z))
             (swizzle projection :w))
          (mix 1.0 view-z divisor))))

(define-shader-function mesh-clip-uv (clip)
  (assume-quantity
   (+ (* (/ (swizzle clip :xy) (swizzle clip :w)) 0.5)
      (vec2 0.5 0.5))
   :quantity quantities:texture-coordinate :unit :one))

(define-shader-function mesh-temporal-motion (previous-clip current-clip)
  "Return the unjittered previous-minus-current motion MetalFX expects.

The scaler receives the current sampling offset independently through
JITTER-OFFSET-{X,Y}.  Its default contract consumes these vectors directly,
so adding either frame's Halton offset here would invent screen-wide motion
for completely static geometry."
  (representation
   (- (mesh-clip-uv previous-clip)
      (mesh-clip-uv current-clip))))

(define-shader-function light-clip-position
    (world-position row-x row-y row-z row-w)
  ;; A homogeneous projective row is representation, not four compatible
  ;; spatial quantities.  Erase the checked point exactly at that boundary.
  (let* ((point (vec4 (representation world-position) 1.0)))
    (vec4 (dot point row-x) (dot point row-y)
          (dot point row-z) (dot point row-w))))

;;; Shadow filtering.  The sun's orthographic depth map is compared over a
;;; small grid of hardware-filtered taps.  A constant or slope-scaled bias
;;; cannot serve a wide filter: a tap one texel toward the sun reads the depth
;;; of the receiver's own plane a little nearer the light, and a low sun turns
;;; that into acne stripes and a permanent half-shadow on every slope.  The
;;; receiver plane is known exactly, so each tap compares against the plane's
;;; own depth at that tap instead.

(define-shader-function receiver-depth-gradient (normal row-x row-y row-z)
  "Return d(shadow depth)/d(shadow uv) across the receiver plane of NORMAL.

The light rows are orthogonal, so a displacement in the plane is solved in
closed form from its two map coordinates."
  (let* ((rx (swizzle row-x :xyz))
         (ry (swizzle row-y :xyz))
         (rz (swizzle row-z :xyz))
         (along-z (dot normal rz))
         ;; Lit receivers face the light, so their forward component is
         ;; negative.  Grazing planes are clamped; their direct light is
         ;; already vanishing.
         (safe-z (min along-z (* -0.08 (sqrt (dot rz rz)))))
         (scale (/ (* -2.0 (dot rz rz)) safe-z))
         (gradient
           (vec2 (/ (* scale (dot normal rx)) (dot rx rx))
                 (/ (* scale (dot normal ry)) (dot ry ry)))))
    (clamp gradient (vec2 -4.0 -4.0) (vec2 4.0 4.0))))

(define-shader-function soft-shadow-visibility
    (shadow-map shadow-sampler shadow-sample normal sun shadow-control
     row-x row-y row-z)
  "Nine receiver-plane comparison taps forming one paper-soft sun shadow."
  ;; Comparison sampling combines a projective coordinate, plane bias, and
  ;; filter radius.  Their interface meanings are checked; the fixed
  ;; comparison-filter program is the explicit representation seam.
  (let* ((shadow-sample (representation shadow-sample))
         (uv (swizzle shadow-sample :xy))
         (depth (swizzle shadow-sample :z))
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (in-bounds
           (* (step 0.0 u) (step u 1.0)
              (step 0.0 v) (step v 1.0)
              (step 0.0 depth) (step depth 1.0)))
         (texel (swizzle shadow-control :xy))
         (gradient (receiver-depth-gradient normal row-x row-y row-z))
         ;; The hardware comparison filter spans one more texel than the tap
         ;; centre; allow the plane that much slack, plus the authored bias.
         (bias (+ (swizzle shadow-control :z)
                  (* (abs (swizzle gradient :x)) (swizzle texel :x))
                  (* (abs (swizzle gradient :y)) (swizzle texel :y))))
         (step-uv (* texel (swizzle shadow-control :w)))
         (sx (swizzle step-uv :x))
         (sy (swizzle step-uv :y))
         (gx (* (swizzle gradient :x) sx))
         (gy (* (swizzle gradient :y) sy))
         (reference (- depth bias))
         (visibility
           (/ (+ (sample-compare shadow-map shadow-sampler uv reference)
                 (sample-compare shadow-map shadow-sampler
                                 (+ uv (vec2 sx 0.0)) (+ reference gx))
                 (sample-compare shadow-map shadow-sampler
                                 (- uv (vec2 sx 0.0)) (- reference gx))
                 (sample-compare shadow-map shadow-sampler
                                 (+ uv (vec2 0.0 sy)) (+ reference gy))
                 (sample-compare shadow-map shadow-sampler
                                 (- uv (vec2 0.0 sy)) (- reference gy))
                 (sample-compare shadow-map shadow-sampler
                                 (+ uv (vec2 sx sy)) (+ reference (+ gx gy)))
                 (sample-compare shadow-map shadow-sampler
                                 (- uv (vec2 sx sy)) (- reference (+ gx gy)))
                 (sample-compare shadow-map shadow-sampler
                                 (+ uv (vec2 sx (- sy))) (+ reference (- gx gy)))
                 (sample-compare shadow-map shadow-sampler
                                 (- uv (vec2 sx (- sy))) (- reference (- gx gy))))
              9.0)))
    (mix 1.0 visibility in-bounds)))

;;; The atmosphere shared by sky and ground.  Distant terrain and the sky's
;;; own lower half must arrive at the same colour, or the edge of the
;;; resident world draws itself as a line.  Both ask AERIAL-PERSPECTIVE-COLOR.

(define-shader-function henyey-greenstein (cosine asymmetry)
  "The Henyey-Greenstein phase function for a haze of ASYMMETRY at COSINE."
  (let* ((squared (* asymmetry asymmetry))
         (denominator
           (max 0.0001 (- (+ 1.0 squared) (* 2.0 (* asymmetry cosine))))))
    (/ (- 1.0 squared) (* 12.566371 (* denominator (sqrt denominator))))))

(define-shader-function aerial-perspective-color
    (fog-color direction sun day-factor)
  "The colour something arbitrarily far away takes, seen along DIRECTION.

The day's haze colour, warmed and brightened where the ray runs toward the
sun through the same Henyey-Greenstein glow the sky's halo is made of: a
ridge against a low sun glows and one with the sun behind stays cool."
  (let* ((alignment (dot direction sun))
         (toward (max 0.0 alignment))
         (low-sun (* day-factor (- 1.0 (smoothstep 0.02 0.45 (swizzle sun :z)))))
         (glow (henyey-greenstein alignment 0.66))
         (warm (mix (vec3 1.08 0.80 0.56) (vec3 1.25 0.56 0.30) low-sun)))
    (+ (* fog-color (- 1.0 (* 0.18 (* low-sun toward))))
       (* warm (* day-factor
                  (+ (* glow 0.07)
                     (* (+ 0.04 (* 0.22 low-sun)) (* toward toward))))))))

(define-shader-function haze-amount (eye point density falloff)
  "Fraction of a ray from EYE to POINT lost to height-falling haze.

Density decays exponentially with world height, so valleys fill with air
and summits stand clear.  The optical depth is the closed-form integral of
that density along the straight ray."
  (let* ((delta (- point eye))
         (distance (sqrt (dot delta delta)))
         (eye-height (max 0.0 (swizzle eye :z)))
         (rise (* falloff (swizzle delta :z)))
         (base (exp (* (- falloff) eye-height)))
         (integral
           (if (> (abs rise) 0.001)
               (/ (- 1.0 (exp (- rise))) rise)
               (- 1.0 (* 0.5 rise))))
         (depth (* density (* distance (* base integral)))))
    (- 1.0 (exp (- depth)))))

(defvar *terrain-edge-shaping-p* nil
  "Round plain cube shading normals; the atelier and real bevels stay unchanged.")

(define-shader-function edge-class-value (code)
  (if (= code (uint 1.0)) 1.0 (if (= code (uint 2.0)) -1.0 0.0)))

(define-shader-function rounded-cube-normal (bent normal world edges distance)
  "A narrow normal-only rim on exposed edges, filtered before it becomes subpixel."
  (let* ((nx (abs (swizzle normal :x)))
         (ny (abs (swizzle normal :y)))
         (u (if (> nx 0.5) (vec3 0.0 1.0 0.0)
                (if (> ny 0.5) (vec3 0.0 0.0 1.0) (vec3 1.0 0.0 0.0))))
         (v (if (> nx 0.5) (vec3 0.0 0.0 1.0)
                (if (> ny 0.5) (vec3 1.0 0.0 0.0) (vec3 0.0 1.0 0.0))))
         (uv (fract (vec2 (dot world u) (dot world v))))
         (ramps (- (vec4 1.0 1.0 1.0 1.0)
                   (smoothstep (vec4 0.0 0.0 0.0 0.0) (vec4 0.10 0.10 0.10 0.10)
                               (vec4 (swizzle uv :x) (- 1.0 (swizzle uv :x))
                                     (swizzle uv :y) (- 1.0 (swizzle uv :y))))))
         (rim (* ramps edges))
         (lean (+ (* u (- (swizzle rim :y) (swizzle rim :x)))
                  (* v (- (swizzle rim :w) (swizzle rim :z)))))
         (dx (derivative-x world))
         (dy (derivative-y world))
         (footprint (sqrt (max (dot dx dx) (dot dy dy))))
         (fade (* (- 1.0 (smoothstep 0.04 0.16 footprint))
                  (- 1.0 (smoothstep 20.0 55.0 distance))))
         (rounded (normalize (+ normal (* lean 0.75)))))
    (normalize (+ bent (* (- rounded normal) fade)))))

(define-shader-abstraction terrain-edge-normal (bent normal world edges distance)
  (if *terrain-edge-shaping-p*
      `(rounded-cube-normal ,bent ,normal ,world ,edges ,distance)
      bent))

(defvar *terrain-material-character-function* nil
  "Optional game material treatment; NIL preserves the atelier's surface tones.")

(define-shader-abstraction terrain-material-character (tone relief world normal distance)
  (if *terrain-material-character-function*
      `(,*terrain-material-character-function* ,tone ,relief ,world ,normal ,distance)
      tone))

(defvar *surface-atmosphere-function* nil
  "Optional game atmosphere; NIL keeps the height-falling paper haze.")

(define-shader-abstraction atmospheric-surface
    (radiance world eye forward divisor character sun fog zenith parameters)
  `(,(or *surface-atmosphere-function* 'paper-atmospheric-surface)
    ,radiance ,world ,eye ,forward ,divisor ,character ,sun ,fog ,zenith ,parameters))

;;; Paper surfaces.  Lonely Mountains: Downhill reads as cut card under a
;;; strong soft sun: each facet one flat, slightly irregular tone; lit faces
;;; warm, shaded faces taking the sky's blue rather than going grey; and
;;; distance dissolving into air.  The two functions below are those terms,
;;; shared by terrain and torch bodies so both sit in the same light.

(define-shader-function paper-surface-radiance
    (tone normal world visibility sun sun-color sky ground)
  "Scene radiance of a flat paper facet of TONE under the sun and sky."
  (let* ((upness (swizzle normal :z))
         ;; Plain Lambert.  Wrapping it past the terminator would let the
         ;; shadow map's meaningless back-face samples show as stripes.
         (direct-shape (max 0.0 (dot normal sun)))
         (direct (* sun-color (* direct-shape visibility)))
         (ambient (mix ground sky (+ 0.5 (* 0.5 upness))))
         ;; One fixed hash per cell and orientation gives each facet its own
         ;; slightly different cut of the same paper; a broad fibre noise
         ;; keeps large faces from reading as flat vector fills.
         (cell (floor (- world (* normal 0.25))))
         (facet (- (paper-hash (+ cell (* normal 7.0))) 0.5))
         (fibre (- (paper-noise (* world 1.7)) 0.5))
         ;; Broad painted patches, a few dozen cells across, drift the tone
         ;; slightly warmer and lighter or cooler and deeper, the way a
         ;; hand-coloured hillside never holds one flat green.
         (patch (- (paper-noise (* world 0.045)) 0.5))
         (patch-tint (+ (vec3 1.0 1.0 1.0)
                        (* (vec3 0.22 0.14 -0.06) patch)))
         (albedo (* (* tone patch-tint)
                    (+ 1.0 (* 0.06 facet) (* 0.08 fibre)))))
    (* albedo (+ ambient direct))))

(define-shader-function paper-atmospheric-surface
    (radiance world eye forward divisor character sun fog zenith parameters)
  "Fade RADIANCE at WORLD into the aerial colour seen from the eye.

An isometric camera's position is not a place anyone stands, so its haze is
measured outward from the character it frames instead: the diorama around
the figure stays clear and only its far reaches recede."
  (let* ((viewer (mix (swizzle character :xyz) (swizzle eye :xyz) divisor))
         (delta (- world viewer))
         (distance (sqrt (dot delta delta)))
         (direction (/ delta (max distance 0.001)))
         (haze (haze-amount viewer world (swizzle fog :w)
                            (swizzle parameters :w)))
         ;; The last resident chunks must reach the sky's own ground colour
         ;; exactly, or the edge of the world draws a line on the horizon.
         (edge (* divisor (smoothstep 180.0 310.0 distance)))
         (amount (max haze edge))
         (air (aerial-perspective-color (swizzle fog :xyz) direction sun
                                        (swizzle zenith :w))))
    (mix radiance air amount)))

;;; A small, rounded traveler for the sanctuary bridge.  The proxy is only a
;;; conservative raster bound; every visible contour below comes from this
;;; analytic field.  LUFT is Z-up, unlike luvcraft's first-person figures.

(defvar *terrain-cloud-visibility-function* nil
  "Shader function selected while building a game's terrain specification.
Arguments are raw Z-up point, sun, elapsed seconds, cloudiness, source origin.
NIL leaves the atelier's direct light unchanged.")

(define-shader-abstraction terrain-cloud-visibility (point sun elapsed cloudiness origin)
  (if *terrain-cloud-visibility-function*
      `(,*terrain-cloud-visibility-function* ,point ,sun ,elapsed ,cloudiness ,origin)
      1.0))

(defvar *terrain-blocklight-function* nil
  "Optional game response to raw RGB blocklight and the shared light gain.")

(define-shader-abstraction terrain-blocklight (light gain)
  (if *terrain-blocklight-function*
      `(,*terrain-blocklight-function* ,light ,gain)
      `(* ,gain (* ,light ,light))))

(define-live-shader star-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
              (mesh-normal :vec3 :location 1 :interpolation :flat
                           :quantity quantities:world-orientation :unit :one)
              (current-clip :vec4 :location 2)
              (previous-clip :vec4 :location 3)
              (shadow-sample :vec3 :location 4
                             :quantity quantities:shadow-coordinate :unit :one)
              (material-tone :vec3 :location 5 :interpolation :flat)
              (material-surface :vec4 :location 6 :interpolation :flat
                                :components #.(luft.terrain::terrain-varying-components :surface))
              (material-pattern :vec4 :location 7 :interpolation :flat
                                :components #.(luft.terrain::terrain-varying-components :pattern))
              (material-mortar :vec4 :location 8 :interpolation :flat
                               :components #.(luft.terrain::terrain-varying-components :mortar))
              (material-accent :vec4 :location 9 :interpolation :flat
                               :components #.(luft.terrain::terrain-varying-components :accent))

              (edge-shaping :vec4 :location 11 :interpolation :flat)
              (voxel-light :vec4 :location 10
                           :components #.(luft.terrain::terrain-varying-components :light)))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 34))
                 (shadow-map :depth-texture-2d :binding 4)
                 (shadow-sampler :sampler :binding 5)))
  (let* ((normal (normalize (representation mesh-normal)))
         (world (representation world-position))
         ;; Response curves below intentionally operate on represented lanes;
         ;; their heterogeneous inputs have been checked at the stage seam.
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (visibility
           (* (soft-shadow-visibility
               shadow-map shadow-sampler shadow-sample normal sun
               (representation shadow-control)
               shadow-row-x shadow-row-y shadow-row-z)
              (terrain-cloud-visibility
               world sun (swizzle atmosphere-parameters :x)
               (swizzle (representation horizon-color-vector) :w)
               (swizzle (representation sun-color-vector) :w))))
         (relief (swizzle (representation material-surface) :x))
         (strength (swizzle (representation material-surface) :y))
         (roughness (swizzle (representation material-surface) :z))
         (wear (swizzle (representation material-surface) :w))
         (eye (representation camera-position))
         (forward (representation (swizzle camera-forward :xyz)))
         (divisor (swizzle (representation render-parameters) :z))
         ;; A laid pattern fades out where its joints would be finer than
         ;; pixels; an orthographic view keeps it.
         (to-eye (- (swizzle eye :xyz) world))
         (distance (mix 20.0 (sqrt (dot to-eye to-eye)) divisor))
         (fade (filtered-material-detail
                (- 1.0 (smoothstep 40.0 120.0
                                  (* distance
                                     (/ (pattern-fineness (representation material-pattern)) 4.0))))
                world (pattern-fineness (representation material-pattern))))
         ;; Fine nap filters independently: a tapestry's large figure and a
         ;; bale's bindings must survive after individual fibers disappear.
         (nap-fade (filtered-material-detail
                    (- 1.0 (smoothstep 12.0 40.0
                                      (/ distance (max 0.2 (abs (dot normal (normalize to-eye)))))))
                    world 24.0))
         (laid (laid-pattern (representation material-pattern) normal world))
         (height (surface-height relief (representation material-pattern) world fade nap-fade laid))
         (bent (terrain-edge-normal (relief-normal normal world strength height)
                                    normal world edge-shaping distance))
         (mortar-tone (swizzle (representation material-mortar) :xyz))
         (element-tone
           (* (if (> (swizzle laid :w) 0.5)
                  (if (> (swizzle (representation material-pattern) :x) 3.5)
                      mortar-tone material-tone)
                  (if (> (dot (swizzle (representation material-accent) :xyz) (vec3 1.0 1.0 1.0)) 0.001)
                      (if (< (swizzle (representation material-pattern) :x) 1.5)
                          (mix material-tone (swizzle (representation material-accent) :xyz) (swizzle laid :y))
                          (book-tone (swizzle (representation material-accent) :xyz) material-tone (swizzle laid :y)))
                      material-tone))
              (+ 1.0 (* 2.0 (swizzle (representation material-mortar) :w)
                        (- (swizzle laid :y) 0.5)))))
         (joint-tone (if (> (swizzle (representation material-pattern) :x) 3.5)
                         (* mortar-tone 0.6) mortar-tone))
         (laid-tone (mix joint-tone element-tone (swizzle laid :x)))
         (face-tone (face-pattern-tone (representation material-pattern) laid normal
                                       material-tone (representation material-mortar)
                                       (representation material-accent)))
         (base-tone (if (< (swizzle (representation material-pattern) :x) 0.5)
                        material-tone
                        (if (> (swizzle (representation material-pattern) :x) 4.5)
                            (mix material-tone face-tone fade)
                            (mix (mix material-tone mortar-tone
                                      (swizzle (representation material-pattern) :w))
                                 laid-tone fade))))
         ;; Hollows of the relief hold shade and dirt; convex edges are
         ;; worn lighter and concave ones gather darker (#HYBFRT).
         (fringe (if (< (abs (- relief 1.0)) 0.5)
                     (* (smoothstep 0.78 0.94
                                    (+ (fract (- (swizzle world :z) 0.001))
                                       (* 0.08 (- height 0.5))))
                        (- 1.0 (abs (swizzle normal :z))))
                     0.0))
         (surface-tone (if (> relief 12.5)
                           (faceted-tone base-tone normal world fade)
                           base-tone))
         (tone (terrain-material-character
                (* (mix surface-tone (swizzle (representation material-accent) :xyz) fringe)
                   (+ 1.0 (* strength 0.5 (- height 0.5)) (* 0.22 wear)))
                relief world normal distance))
         (view (normalize (mix (- forward) (- (swizzle eye :xyz) world)
                               divisor)))
         ;; Conductors use neutral ambient energy and a coloured sun lobe.
         ;; Two sky/ground colours are not an environment map: reflecting
         ;; their hue painted every iron face blue, including upward ones.
         (metal (if (> relief 11.5) (if (< relief 12.5) 1.0 0.0) 0.0))
         (sky (* (swizzle (representation voxel-light) :w)
                 (representation (swizzle sky-color-vector :xyz))))
         (ground (* (swizzle (representation voxel-light) :w)
                    (representation (swizzle ground-color-vector :xyz))))
         (environment (* (swizzle (representation voxel-light) :w) 0.38))
         (radiance
           (+ (paper-surface-radiance
               (* tone (- 1.0 (* metal 0.9))) bent world visibility sun sun-color
               sky ground)
              (* metal tone (* 1.8 environment))
              (* sun-color
                 (mix (vec3 1.0 1.0 1.0) (+ (vec3 0.2 0.2 0.2) (* tone 5.0)) metal)
                 (* visibility (material-specular bent view sun roughness)))
              (face-pattern-glow (representation material-pattern) laid normal
                                 (representation material-accent)
                                 fade)
              (if (< (swizzle (representation material-pattern) :x) 0.5)
                  (* tone (swizzle (representation material-accent) :w))
                  (vec3 0.0 0.0 0.0))
              ;; Terrain voxel light (#BQMYNU): torches, lanterns, crystals,
              ;; and lava, warm by night and lost in the sun by day.  The
              ;; square keeps a light's falloff from flattening into a wash.
              (* tone (terrain-blocklight (swizzle (representation voxel-light) :xyz)
                                         voxel-light-gain))))
         (final
           (atmospheric-surface
            radiance world
            (representation camera-position)
            forward
            (swizzle (representation render-parameters) :z)
            (representation character-parameters)
            sun (representation fog-color-vector)
            (representation zenith-color-vector)
            atmosphere-parameters)))
    (set-output color-output (vec4 final 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))

;;; Terrain by instanced vertex pulling.  Every star has its own indexed
;;; patch in two shared tables: the scene's, whose vertices are shared only
;;; where triangles agree on normal and material mask, and the shadow's,
;;; shared wherever positions meet.  Each chunk sorts its sites by star, and
;;; one indexed draw per star instances that patch over the star's run of
;;; sites.  A vertex reads its own record, its site, and the site's
;;; appearance; nothing is recomputed per corner and no mesh stage is needed.

(define-shader-function star-record-world (record origin)
  "The world point of one biased star-table RECORD at site ORIGIN."
  (assume-quantity
   (+ origin (/ (- (vec3 (float (swizzle record :x))
                         (float (swizzle record :y))
                         (float (swizzle record :z)))
                   (vec3 8.0 8.0 8.0))
                8.0))
   :quantity quantities:world-position :unit quantities:cell))

(define-live-shader terrain-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0
                                      :quantity quantities:world-position
                                      :unit quantities:cell)
               (mesh-normal-output :vec3 :location 1 :interpolation :flat
                                   :quantity quantities:world-orientation :unit :one)
               (current-clip-output :vec4 :location 2)
               (previous-clip-output :vec4 :location 3)
               (shadow-sample-output :vec3 :location 4
                                     :quantity quantities:shadow-coordinate :unit :one)
               (material-tone-output :vec3 :location 5 :interpolation :flat)
               (material-surface-output :vec4 :location 6 :interpolation :flat
                                        :components #.(luft.terrain::terrain-varying-components :surface))
               (material-pattern-output :vec4 :location 7 :interpolation :flat
                                        :components #.(luft.terrain::terrain-varying-components :pattern))
               (material-mortar-output :vec4 :location 8 :interpolation :flat
                                       :components #.(luft.terrain::terrain-varying-components :mortar))
               (material-accent-output :vec4 :location 9 :interpolation :flat
                                       :components #.(luft.terrain::terrain-varying-components :accent))

               (edge-shaping-output :vec4 :location 11 :interpolation :flat)
               (voxel-light-output :vec4 :location 10
                                   :components #.(luft.terrain::terrain-varying-components :light)))
     :resources ((sites :storage-buffer :binding 0 :element :uvec4)
                 (terrain-vertices :storage-buffer :binding 1 :element :uvec4)
                 (terrain-appearances :storage-buffer :binding 3 :element :uvec2)
                 (terrain-lights :storage-buffer :binding 8 :element :uvec4)
                 (material-descriptors :storage-buffer :binding 6 :element :uvec4)
                 (terrain-normals :storage-buffer :binding 7 :element :vec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))))
  (let* ((site (buffer-element sites instance-index))
         (record (buffer-element terrain-vertices vertex-index))
         (world (star-record-world
                 record (vec3 (float (swizzle site :x)) (float (swizzle site :y))
                              (float (swizzle site :z)))))
         (normal (assume-quantity
                  (swizzle (buffer-element terrain-normals
                                           (ldb (byte 16 16) (swizzle record :w)))
                           :xyz)
                  :quantity quantities:world-orientation :unit :one))
         (appearance (buffer-element terrain-appearances instance-index))
         (code-0 (ldb (byte 8 0) (swizzle appearance :x)))
         (code-1 (ldb (byte 8 8) (swizzle appearance :x)))
         (code-2 (ldb (byte 8 16) (swizzle appearance :x)))
         (code-3 (ldb (byte 8 24) (swizzle appearance :x)))
         (code-4 (ldb (byte 8 0) (swizzle appearance :y)))
         (code-5 (ldb (byte 8 8) (swizzle appearance :y)))
         (code-6 (ldb (byte 8 16) (swizzle appearance :y)))
         (code-7 (ldb (byte 8 24) (swizzle appearance :y)))
         (sample-mask (ldb (byte 8 0) (swizzle record :w)))
         (tone-total
           (+ (terrain-material-sample
               code-0 (ldb (byte 1 0) sample-mask)
               (buffer-element material-descriptors (+ code-0 code-0)) normal)
              (terrain-material-sample
               code-1 (ldb (byte 1 1) sample-mask)
               (buffer-element material-descriptors (+ code-1 code-1)) normal)
              (terrain-material-sample
               code-2 (ldb (byte 1 2) sample-mask)
               (buffer-element material-descriptors (+ code-2 code-2)) normal)
              (terrain-material-sample
               code-3 (ldb (byte 1 3) sample-mask)
               (buffer-element material-descriptors (+ code-3 code-3)) normal)
              (terrain-material-sample
               code-4 (ldb (byte 1 4) sample-mask)
               (buffer-element material-descriptors (+ code-4 code-4)) normal)
              (terrain-material-sample
               code-5 (ldb (byte 1 5) sample-mask)
               (buffer-element material-descriptors (+ code-5 code-5)) normal)
              (terrain-material-sample
               code-6 (ldb (byte 1 6) sample-mask)
               (buffer-element material-descriptors (+ code-6 code-6)) normal)
              (terrain-material-sample
               code-7 (ldb (byte 1 7) sample-mask)
               (buffer-element material-descriptors (+ code-7 code-7)) normal)))
         (material-tone
           (/ (swizzle tone-total :xyz) (max (swizzle tone-total :w) 1.0)))
         (chosen-0 (terrain-code-pick (uint 0.0) code-0
                                        (ldb (byte 1 0) sample-mask)))
         (chosen-1 (terrain-code-pick chosen-0 code-1
                                        (ldb (byte 1 1) sample-mask)))
         (chosen-2 (terrain-code-pick chosen-1 code-2
                                        (ldb (byte 1 2) sample-mask)))
         (chosen-3 (terrain-code-pick chosen-2 code-3
                                        (ldb (byte 1 3) sample-mask)))
         (chosen-4 (terrain-code-pick chosen-3 code-4
                                        (ldb (byte 1 4) sample-mask)))
         (chosen-5 (terrain-code-pick chosen-4 code-5
                                        (ldb (byte 1 5) sample-mask)))
         (chosen-6 (terrain-code-pick chosen-5 code-6
                                        (ldb (byte 1 6) sample-mask)))
         (chosen-7 (terrain-code-pick chosen-6 code-7
                                        (ldb (byte 1 7) sample-mask)))
         ;; The first sampled material's surface and pattern speak for the
         ;; whole triangle; a material's two rows sit at twice its code.
         (surface-row (buffer-element material-descriptors (+ chosen-7 chosen-7)))
         (pattern-row (buffer-element material-descriptors
                                      (+ (+ chosen-7 chosen-7) (uint 1.0))))
         (material-surface
           (terrain-surface-vector (swizzle surface-row :w) sample-mask
                                   (ldb (byte 8 8) (swizzle record :w))))
         (material-pattern (terrain-pattern-vector (swizzle pattern-row :x)))
         (material-mortar
           (assume-quantity
            (vec4 (unpack-terrain-tone (swizzle pattern-row :y))
                  (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :variance)
                                 (swizzle pattern-row :x)))
                     #.(luft.terrain::terrain-descriptor-divisor :variance)))
            :components #.(luft.terrain::terrain-varying-components :mortar)))
         ;; The accent tone, and the surface emission it glows by over 4.
         (material-accent
           (assume-quantity
            (vec4 (unpack-terrain-tone
                   (if (and (= (ldb #.(luft.terrain::terrain-descriptor-byte :relief-code)
                                    (swizzle surface-row :w)) (uint 1.0))
                            (= (ldb #.(luft.terrain::terrain-descriptor-byte :pattern-code)
                                    (swizzle pattern-row :x)) (uint 0.0)))
                       (swizzle surface-row :x) (swizzle pattern-row :z)))
                  (/ (float (ldb #.(luft.terrain::terrain-descriptor-byte :emission)
                                 (swizzle pattern-row :z)))
                     #.(luft.terrain::terrain-descriptor-divisor :emission)))
            :components #.(luft.terrain::terrain-varying-components :accent)))
         ;; A vertex lies within a tick of one of the seven lattice points
         ;; of the cube above its site; that point's RGB4 is its light.
         (lights (buffer-element terrain-lights instance-index))
         (point (+ (if (> (swizzle record :x) (uint 11.0)) (uint 1.0) (uint 0.0))
                   (if (> (swizzle record :y) (uint 11.0)) (uint 2.0) (uint 0.0))
                   (if (> (swizzle record :z) (uint 11.0)) (uint 4.0) (uint 0.0))))
         (light-pair
           (if (< point (uint 2.0)) (swizzle lights :x)
               (if (< point (uint 4.0)) (swizzle lights :y)
                   (if (< point (uint 6.0)) (swizzle lights :z)
                       (swizzle lights :w)))))
         (packed-light
           (if (> (ldb (byte 1 0) point) (uint 0.0))
               (ldb (byte 16 16) light-pair)
               (ldb (byte 16 0) light-pair)))
         (voxel-light
           (assume-quantity
            (/ (vec4 (float (ldb #.(luft::light-point-byte 0) packed-light))
                     (float (ldb #.(luft::light-point-byte 1) packed-light))
                     (float (ldb #.(luft::light-point-byte 2) packed-light))
                     (float (ldb #.(luft::light-point-byte 3) packed-light)))
               15.0)
            :components #.(luft.terrain::terrain-varying-components :light)))
         (divisor (swizzle (representation render-parameters) :z))
         (clip (mesh-view-clip world camera-position camera-right camera-up
                               camera-forward camera-projection divisor))
         (previous (mesh-view-clip world previous-camera-position
                                   previous-camera-right previous-camera-up
                                   previous-camera-forward
                                   previous-camera-projection divisor))
         (light (light-clip-position world shadow-row-x shadow-row-y
                                     shadow-row-z shadow-row-w))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle clip :x) (* (swizzle jitter :x) (swizzle clip :w)))
                      (+ (swizzle clip :y) (* (swizzle jitter :y) (swizzle clip :w)))
                      (swizzle clip :z) (swizzle clip :w)))
    (set-output world-position-output world)
    (set-output edge-shaping-output
                (let* ((n (abs (representation normal)))
                       (word (if (> (swizzle n :x) 0.5)
                                 (ldb (byte 8 8) (swizzle site :w))
                                 (if (> (swizzle n :y) 0.5)
                                     (ldb (byte 8 16) (swizzle site :w))
                                     (ldb (byte 8 24) (swizzle site :w))))))
                  (vec4 (edge-class-value (ldb (byte 2 0) word))
                        (edge-class-value (ldb (byte 2 2) word))
                        (edge-class-value (ldb (byte 2 4) word))
                        (edge-class-value (ldb (byte 2 6) word)))))
    (set-output mesh-normal-output normal)
    (set-output current-clip-output clip)
    (set-output previous-clip-output previous)
    (set-output shadow-sample-output
                (assume-quantity
                 (vec3 (+ (* (swizzle light :x) 0.5) 0.5)
                       (+ (* (swizzle light :y) 0.5) 0.5)
                       (swizzle light :z))
                 :quantity quantities:shadow-coordinate :unit :one))
    (set-output material-tone-output material-tone)
    (set-output material-surface-output material-surface)
    (set-output material-pattern-output material-pattern)
    (set-output material-mortar-output material-mortar)
    (set-output material-accent-output material-accent)

    (set-output voxel-light-output voxel-light)))

(define-live-shader terrain-shadow-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources ((sites :storage-buffer :binding 0 :element :uvec4)
                 (terrain-vertices :storage-buffer :binding 1 :element :uvec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))))
  (let* ((site (buffer-element sites instance-index))
         (world (star-record-world
                 (buffer-element terrain-vertices vertex-index)
                 (vec3 (float (swizzle site :x)) (float (swizzle site :y))
                       (float (swizzle site :z))))))
    (set-output clip-position
                (light-clip-position world shadow-row-x shadow-row-y
                                     shadow-row-z shadow-row-w))))

(define-live-shader terrain-shadow-rectangle-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources ((shadow-rectangles :storage-buffer :binding 0 :element :vec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))))
  (let* ((origin (buffer-element shadow-rectangles (* instance-index (uint 2.0))))
         (extent (buffer-element shadow-rectangles (+ (* instance-index (uint 2.0))
                                                     (uint 1.0))))
         (axis (abs (swizzle origin :w)))
         (corner-u (float (ldb (byte 1 0) vertex-index)))
         (u (if (< (swizzle origin :w) 0.0) (- 1.0 corner-u) corner-u))
         (v (float (ldb (byte 1 1) vertex-index)))
         (world (assume-quantity
                 (+ (swizzle origin :xyz)
                    (* (swizzle extent :xyz)
                       (vec3 (if (= axis 2.0) v u)
                             (if (= axis 3.0) v u)
                             (if (= axis 1.0) v u))))
                 :quantity quantities:world-position :unit quantities:cell)))
    (set-output clip-position
                (light-clip-position world shadow-row-x shadow-row-y
                                     shadow-row-z shadow-row-w))))

(define-live-shader lattice-point-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (marker-coordinate-output :vec2 :location 0)
               (current-clip-output :vec4 :location 1)
               (previous-clip-output :vec4 :location 2)
               (marker-kind-output :float :location 3
                                   :interpolation :flat))
     :resources ((lattice-points :storage-buffer :binding 0 :element :uvec4)
                 (camera-state :uniform-block :binding 1
                  :members #.(scene-uniform-prefix 14))))
  (let* ((record (buffer-element lattice-points instance-index))
         (world-position
           (assume-quantity
            (/ (vec3 (float (swizzle record :x))
                     (float (swizzle record :y))
                     (float (swizzle record :z)))
               8.0)
            :quantity quantities:world-position :unit quantities:cell))
         (index (float vertex-index))
         (right (if (= index 2.0) 1.0
                    (if (= index 3.0) 1.0
                        (if (= index 5.0) 1.0 0.0))))
         (bottom (if (= index 1.0) 1.0
                     (if (= index 4.0) 1.0
                         (if (= index 5.0) 1.0 0.0))))
         (coordinate (vec2 (- (* right 2.0) 1.0)
                           (- (* bottom 2.0) 1.0)))
         (marker-kind (float (swizzle record :w)))
         (current-clip
           (mesh-view-clip world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-clip
           (mesh-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (pixel-size
           (representation (swizzle inspection-parameters :zw)))
         (radius (if (> marker-kind 1.5) 8.5
                     (if (> marker-kind 0.5) 6.5 2.6)))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output
     clip-position
     (vec4 (+ (+ (swizzle current-clip :x)
                 (* (swizzle jitter :x) (swizzle current-clip :w)))
              (* (swizzle coordinate :x)
                 (* (* radius 2.0) (swizzle pixel-size :x))
                 (swizzle current-clip :w)))
           (+ (+ (swizzle current-clip :y)
                 (* (swizzle jitter :y) (swizzle current-clip :w)))
              (* (swizzle coordinate :y)
                 (* (* radius 2.0) (swizzle pixel-size :y))
                 (swizzle current-clip :w)))
           (- (swizzle current-clip :z)
              (* 0.00015 (swizzle current-clip :w)))
           (swizzle current-clip :w)))
    (set-output marker-coordinate-output coordinate)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)
    (set-output marker-kind-output marker-kind)))

(define-live-shader lattice-point-fragment-specification
    (:stage :fragment
     :inputs ((marker-coordinate :vec2 :location 0)
              (current-clip :vec4 :location 1)
              (previous-clip :vec4 :location 2)
              (marker-kind :float :location 3 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1)))
  (let* ((radius (sqrt (dot marker-coordinate marker-coordinate)))
         (coverage (- 1.0 (smoothstep 0.82 1.0 radius)))
         (vertex-site (if (> marker-kind 1.5) 1.0 0.0))
         (mesh-point (if (> marker-kind 0.5) 1.0 0.0))
         (center (- 1.0 (smoothstep (if (> mesh-point 0.5) 0.40 0.18)
                                    (if (> mesh-point 0.5) 0.62 0.52)
                                    radius)))
         (rim (vec3 0.035 0.075 0.095))
         (ink (vec3 1.0 0.30 0.10))
         (site (vec3 0.20 0.95 1.0))
         (color (mix (mix rim ink center) site vertex-site))
         (alpha (* coverage 0.96)))
    (set-output color-output (vec4 (* color alpha) alpha))
    (set-output motion-output
                (- (mesh-clip-uv previous-clip)
                   (mesh-clip-uv current-clip)))))

(define-live-shader present-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (ndc-output :vec2 :location 0)))
  (let* ((index (float vertex-index))
         (x (if (< index 0.5) -1.0 (if (< index 1.5) 3.0 -1.0)))
         (y (if (< index 1.5) -1.0 3.0)))
    (set-output clip-position (vec4 x y 0.0 1.0))
    (set-output ndc-output (vec2 x y))))

(define-shader-function view-depth (depth projection divisor)
  (if (< divisor 0.5)
      (/ (- depth (swizzle projection :w)) (swizzle projection :z))
      (/ (swizzle projection :w)
         (- depth (swizzle projection :z)))))

(define-shader-function depth-occlusion (centre sample radius)
  (let* ((delta (- centre sample))
         (near (smoothstep 0.025 (* radius 0.42) delta))
         (far (- 1.0 (smoothstep (* radius 0.62) radius delta))))
    (* near far)))

(define-shader-function rgb-to-ycocg (rgb)
  "Put RGB into a luminance/chroma space whose box clips history usefully."
  (vec3 (+ (* (swizzle rgb :x) 0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) 0.25))
        (* 0.5 (- (swizzle rgb :x) (swizzle rgb :z)))
        (+ (* (swizzle rgb :x) -0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) -0.25))))

(define-shader-function ycocg-to-rgb (value)
  "Invert RGB-TO-YCOCG."
  (vec3 (+ (swizzle value :x) (swizzle value :y)
           (- (swizzle value :z)))
        (+ (swizzle value :x) (swizzle value :z))
        (+ (swizzle value :x) (- (swizzle value :y))
           (- (swizzle value :z)))))

;;; Temporal reconstruction is two fullscreen passes.  The resolve draws
;;; more output pixels than there are scene texels, and each output pixel's
;;; history clamp and motion belong to the scene texel it falls in; gathered
;;; there, the same nine taps of colour, motion, and depth would be taken
;;; again for every output pixel.  So a pass at the scene's size gathers them
;;; once per texel into two targets: the low and high YCoCg bounds, with the
;;; nearest surface's motion in their fourth lanes.  See #C4ED2V.

(define-shader-abstraction scene-texel (centre dx dy size)
  `(uvec2 (uint (clamp (+ (swizzle ,centre :x) ,dx) 0.0
                       (- (swizzle ,size :x) 1.0)))
          (uint (clamp (+ (swizzle ,centre :y) ,dy) 0.0
                       (- (swizzle ,size :y) 1.0)))))

(define-shader-abstraction scene-neighbourhood (extreme scene centre size)
  (let ((taps (loop for dy in '(-1.0 0.0 1.0)
                    append (loop for dx in '(-1.0 0.0 1.0)
                                 collect `(rgb-to-ycocg
                                           (swizzle
                                            (texel-load ,scene
                                                        (scene-texel ,centre
                                                                     ,dx ,dy
                                                                     ,size))
                                            :xyz))))))
    (reduce (lambda (a b) `(,extreme ,a ,b)) taps)))

(define-shader-function motion-tap (motion depth)
  (vec3 (swizzle motion :x) (swizzle motion :y) (swizzle depth :x)))

(define-shader-function nearer-motion-tap (a b)
  "Keep whichever tap is nearer the camera; depth grows away from it."
  (mix a b (step (swizzle b :z) (swizzle a :z))))

(define-shader-abstraction nearest-motion (motion depth centre size)
  ;; At a silhouette the centre may be background while its neighbour is the
  ;; moving foreground; history must follow the foreground, or its old colour
  ;; is left behind as a trail on the newly revealed background.
  (let ((taps (loop for dy in '(-1.0 0.0 1.0)
                    append (loop for dx in '(-1.0 0.0 1.0)
                                 collect `(motion-tap
                                           (texel-load ,motion
                                                       (scene-texel ,centre
                                                                    ,dx ,dy
                                                                    ,size))
                                           (texel-load ,depth
                                                       (scene-texel ,centre
                                                                    ,dx ,dy
                                                                    ,size)))))))
    (reduce (lambda (a b) `(nearer-motion-tap ,a ,b)) taps)))

(define-live-shader temporal-neighbourhood-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((low-output :vec4 :location 0)
               (high-output :vec4 :location 1))
     :resources
     ((current :texture-2d :binding 0 :sample-transfer :identity)
      (motion-texture :texture-2d :binding 1 :sample-transfer :identity)
      (scene-depth :depth-texture-2d :binding 2)
      (camera-state :uniform-block :binding 3
       :members #.(scene-uniform-prefix 13))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         ;; INSPECTION-PARAMETERS.ZW is the inverse internal scene extent.
         (texel (representation (swizzle inspection-parameters :zw)))
         (size (/ (vec2 1.0 1.0) texel))
         (centre (floor (* uv size)))
         (motion (nearest-motion motion-texture scene-depth centre size)))
    (set-output low-output
                (vec4 (scene-neighbourhood min current centre size)
                      (swizzle motion :x)))
    (set-output high-output
                (vec4 (scene-neighbourhood max current centre size)
                      (swizzle motion :y)))))

(define-shader-abstraction catmull-rom-history (texture sampler w0 w12 w3 at0 at12 at3)
  ;; Bicubic Catmull-Rom from five bilinear taps: the four corner taps carry
  ;; little weight and are dropped.  A plain bilinear fetch would blur
  ;; history a little more on every frame it is reprojected.
  (flet ((tap (x y wx wy)
           `(* (swizzle (sample ,texture ,sampler
                                (vec2 (swizzle ,x :x) (swizzle ,y :y)))
                        :xyz)
               (* (swizzle ,wx :x) (swizzle ,wy :y)))))
    `(/ (+ ,(tap at12 at0 w12 w0) ,(tap at0 at12 w0 w12)
           ,(tap at12 at12 w12 w12) ,(tap at3 at12 w3 w12)
           ,(tap at12 at3 w12 w3))
        (+ (* (swizzle ,w12 :x) (swizzle ,w0 :y))
           (* (swizzle ,w0 :x) (swizzle ,w12 :y))
           (* (swizzle ,w12 :x) (swizzle ,w12 :y))
           (* (swizzle ,w3 :x) (swizzle ,w12 :y))
           (* (swizzle ,w12 :x) (swizzle ,w3 :y))))))

(define-live-shader temporal-resolve-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((current :texture-2d :binding 0 :sample-transfer :identity)
      (bounds-low :texture-2d :binding 1 :sample-transfer :identity)
      (bounds-high :texture-2d :binding 2 :sample-transfer :identity)
      (history :texture-2d :binding 3 :sample-transfer :identity)
      (temporal-sampler :sampler :binding 4)
      (camera-state :uniform-block :binding 5
       :members #.(scene-uniform-prefix 31))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (texel (representation (swizzle inspection-parameters :zw)))
         (size (/ (vec2 1.0 1.0) texel))
         ;; The scene was rasterized displaced by half its clip jitter, so the
         ;; unjittered estimate of this output pixel lies there.
         (here (+ uv (* (representation (swizzle temporal-parameters :xy))
                        0.5)))
         (fresh (sample current temporal-sampler here))
         (centre (scene-texel (floor (* here size)) 0.0 0.0 size))
         (below (texel-load bounds-low centre))
         (above (texel-load bounds-high centre))
         ;; Motion excludes jitter and history lives on the fixed output grid,
         ;; so a static point reads the same history address every frame.
         (motion (vec2 (swizzle below :w) (swizzle above :w)))
         (history-uv (+ uv motion))
         (inside
           (* (* (step 0.0 (swizzle history-uv :x))
                 (step (swizzle history-uv :x) 1.0))
              (* (step 0.0 (swizzle history-uv :y))
                 (step (swizzle history-uv :y) 1.0))))
         ;; LENS-EXTENT.ZW is the inverse output extent, history's own.
         (output-texel (representation (swizzle lens-extent :zw)))
         (output-size (/ (vec2 1.0 1.0) output-texel))
         (position (* history-uv output-size))
         (middle (+ (floor (- position (vec2 0.5 0.5))) (vec2 0.5 0.5)))
         (f (- position middle))
         (w0 (* f (+ (vec2 -0.5 -0.5) (* f (- (vec2 1.0 1.0) (* f 0.5))))))
         (w1 (+ (vec2 1.0 1.0) (* f (* f (+ (vec2 -2.5 -2.5) (* f 1.5))))))
         (w2 (* f (+ (vec2 0.5 0.5) (* f (- (vec2 2.0 2.0) (* f 1.5))))))
         (w3 (* f (* f (- (* f 0.5) (vec2 0.5 0.5)))))
         (w12 (+ w1 w2))
         (at0 (* (- middle (vec2 1.0 1.0)) output-texel))
         (at3 (* (+ middle (vec2 2.0 2.0)) output-texel))
         (at12 (* (+ middle (/ w2 w12)) output-texel))
         (old (rgb-to-ycocg
               (catmull-rom-history history temporal-sampler
                                    w0 w12 w3 at0 at12 at3)))
         (clipped (clamp old (swizzle below :xyz) (swizzle above :xyz)))
         (speed (clamp (* (sqrt (dot motion motion)) 48.0) 0.0 1.0))
         ;; The otherwise-unused W components of these previous-view lanes
         ;; carry resolve validity and weight without changing the frame ABI.
         (history-weight
           (* (* (swizzle (representation previous-camera-position) :w)
                 inside)
              (* (swizzle (representation previous-camera-right) :w)
                 (- 1.0 (* speed 0.35)))))
         (resolved (mix (swizzle fresh :xyz)
                        (ycocg-to-rgb clipped)
                        history-weight)))
    ;; Alpha is current-frame focus metadata, never temporal colour history.
    (set-output color-output (vec4 resolved (swizzle fresh :w)))))

(define-live-shader present-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)
                 (scene-depth :depth-texture-2d :binding 2)
                 (camera-state :uniform-block :binding 3
                  :members #.(scene-uniform-prefix 31))
                 (bloom :texture-2d :binding 4 :sample-transfer :identity)
                 (shafts :texture-2d :binding 5 :sample-transfer :identity)
                 (panel-color :texture-2d :binding 6 :sample-transfer :identity)
                 (panel-depth :depth-texture-2d :binding 7)
                 (viewmodel :texture-2d :binding 8 :sample-transfer :identity)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (value (sample scene scene-sampler uv))
         (auto-exposure (swizzle sky-color-vector :w))
         (texel
           (representation (swizzle inspection-parameters :zw)))
         ;; Geometry depth carries the subpixel projection jitter consumed by
         ;; MetalFX; presentation UVs do not.  Sample depth at the same current
         ;; geometry location so the AO does not crawl across a resolved edge.
         (depth-uv
           (+ uv
              (* (representation (swizzle temporal-parameters :xy)) 0.5)))
         (depth (swizzle (sample scene-depth scene-sampler depth-uv) :x))
         (divisor (swizzle (representation render-parameters) :z))
         (centre (view-depth depth camera-projection divisor))
         (perspective-scale (if (< divisor 0.5) 1.0 (/ 1.0 centre)))
         (outer
           (* (swizzle camera-projection :xy)
              (* 0.5 #.*screen-ambient-occlusion-radius*
                 perspective-scale)))
         (inner (* outer 0.42))
         (diagonal (* outer 0.70710678))
         (occlusion
           (* (/ 1.0 12.0)
              (+
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 (swizzle inner :x) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (- (swizzle inner :x)) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (swizzle inner :y))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (- (swizzle inner :y)))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 (swizzle outer :x) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (- (swizzle outer :x)) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (swizzle outer :y))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (- (swizzle outer :y)))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle (sample scene-depth scene-sampler
                                                  (+ depth-uv diagonal))
                                          :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle (sample scene-depth scene-sampler
                                                  (- depth-uv diagonal))
                                          :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (swizzle diagonal :x)
                                            (- (swizzle diagonal :y)))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (- (swizzle diagonal :x))
                                            (swizzle diagonal :y))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*))))
         (accessibility
           (if (< depth 0.9999)
               (- 1.0 (* #.*screen-ambient-occlusion-strength*
                         (min 1.0 (* occlusion 1.6))))
               1.0))
         (pooling
           (* #.*ambient-pigment-strength*
              (min 1.0 (* occlusion 1.6))))
         ;; A common cool shadow pigment makes the depth signal read as an
         ;; illustrated wash rather than neutral post-process darkening.
         (shadow-pigment (vec3 0.76 0.88 1.03))
         (shadowed (* (swizzle value :xyz) accessibility))
         (pigmented (* shadowed
                       (mix (vec3 1.0 1.0 1.0)
                            shadow-pigment pooling)))
         (surface-color (presentation-medium pigmented shadowed))
         ;; A gentle screen-space tilt shift holds the player's projected
         ;; height crisp and gives the near/far route a miniature depth cue.
         ;; It is deliberately only a five-tap gather after MetalFX resolves.
         (player-clip
           (mesh-view-clip (swizzle character-parameters :xyz)
                           camera-position camera-right camera-up
                           camera-forward camera-projection divisor))
         (focus-y
           (+ (representation
               (swizzle (mesh-clip-uv player-clip) :y))
              (* (swizzle (representation temporal-parameters) :y) 0.5)))
         (tilt
           (smoothstep 0.16 0.52 (abs (- (swizzle uv :y) focus-y))))
         (blur-radius (* texel 2.6))
         (blurred
           (* 0.2
              (+ (swizzle value :xyz)
                 (swizzle (sample scene scene-sampler
                                  (+ uv (vec2 (swizzle blur-radius :x) 0.0)))
                          :xyz)
                 (swizzle (sample scene scene-sampler
                                  (- uv (vec2 (swizzle blur-radius :x) 0.0)))
                          :xyz)
                 (swizzle (sample scene scene-sampler
                                  (+ uv (vec2 0.0 (swizzle blur-radius :y))))
                          :xyz)
                 (swizzle (sample scene scene-sampler
                                  (- uv (vec2 0.0 (swizzle blur-radius :y))))
                          :xyz))))
         (glowing
           (mix (assume-quantity
                 (swizzle surface-color :xyz)
                 :quantity quantities:scene-radiance :unit :one)
                (assume-quantity blurred
                                 :quantity quantities:scene-radiance
                                 :unit :one)
                (assume-quantity (if (< divisor 0.5) (* tilt 0.52) 0.0) :unit :one)))
         ;; MetalFX has already reconstructed GLOWING at this point.  Sky is
         ;; HDR scene radiance too, so it participates in metering and gets
         ;; the same exposure and grade as geometry; only the geometry-only AO
         ;; and tilt-shift stay out of background pixels.
         (radiance
           (if (< depth 0.9999) glowing
               (assume-quantity
                (swizzle value :xyz)
                :quantity quantities:scene-radiance :unit :one)))
         ;; The lens chain already carries exposed units; the scene does not.
         (lens
           (assume-quantity
            (+ (* (swizzle (sample bloom scene-sampler uv) :xyz)
                  (swizzle lens-parameters :x))
               (* (swizzle (sample shafts scene-sampler uv) :xyz)
                  (swizzle lens-parameters :z)))
            :quantity quantities:scene-radiance :unit :one))
         ;; Like the original Luvcraft world-panel layer: native analytic
         ;; commands bypass reconstruction, AO, tilt shift and the lens chain.
         ;; Compare only their completed depth, never coplanar command depths.
         (panel (sample panel-color scene-sampler uv))
         (panel-z (swizzle (sample panel-depth scene-sampler uv) :x))
         (panel-visible (if (< panel-z depth) 1.0 0.0))
         (panel-alpha (* (swizzle panel :w) panel-visible))
         (scene-share (assume-quantity (- 1.0 panel-alpha) :unit :one))
         (panel-radiance
           (assume-quantity (* (swizzle panel :xyz) panel-visible)
                            :quantity quantities:scene-radiance :unit :one))
         (exposed-radiance
           (+ (interpret (* (+ panel-radiance (* radiance scene-share)) auto-exposure)
                         :quantity quantities:scene-radiance :unit :one)
              (* lens scene-share)))
         ;; Native view-local geometry has no relation to terrain depth:
         ;; neither its AO nor its tilt shift may paint across a player's arms.
         (held (sample viewmodel scene-sampler uv))
         (world-share (assume-quantity (- 1.0 (swizzle held :w)) :unit :one))
         (held-radiance (assume-quantity (swizzle held :xyz)
                                         :quantity quantities:scene-radiance :unit :one))
         (view-radiance
           (+ (interpret (* held-radiance auto-exposure)
                         :quantity quantities:scene-radiance :unit :one)
              (* exposed-radiance world-share)))
         (graded
           (representation (presentation-grade (paper-tonemap view-radiance))))
         ;; A restrained corner falloff; the frame should feel printed, not
         ;; port-holed.
         (centered (- uv (vec2 0.5 0.5)))
         (vignette
           (- 1.0 (* (swizzle atmosphere-parameters :y)
                     (smoothstep 0.10 0.75 (dot centered centered)))))
         ;; Paper: a fixed, screen-locked fibre in two scales.  It multiplies,
         ;; so it lives in the light tones and vanishes into the darks the way
         ;; ink on a rough sheet does.  Fixed to the page rather than the world
         ;; so it reads as the medium, not as a surface texture.
         (output-texel (representation (swizzle lens-extent :zw)))
         (pixel (/ uv output-texel))
         (fibre
           (+ (* (- (paper-noise (vec3 (* (swizzle pixel :x) 0.55)
                                       (* (swizzle pixel :y) 0.55) 3.7))
                    0.5)
                 0.65)
              (* (- (paper-noise (vec3 (* (swizzle pixel :x) 0.09)
                                       (* (swizzle pixel :y) 0.21) 9.1))
                    0.5)
                 0.35)))
         (grain (presentation-medium
                 (+ 1.0 (* fibre (* 2.0 (swizzle atmosphere-parameters :z))))
                 1.0))
         ;; Eight bits cannot hold a sky gradient; interleaved gradient noise
         ;; at half a step turns the contour into grain too fine to see.
         (dither-phase
           (+ (* (swizzle pixel :x) 0.06711056)
              (* (swizzle pixel :y) 0.00583715)))
         (dither (* (- (fract (* 52.9829189 (fract dither-phase))) 0.5)
                    (/ 1.0 255.0)))
         (presented
           (clamp (+ (* graded (* vignette grain)) (vec3 dither dither dither))
                  (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0)))
         (cross-pixel (abs (/ (* ndc 0.5) texel)))
         (cross-long (max (swizzle cross-pixel :x) (swizzle cross-pixel :y)))
         (cross-short (min (swizzle cross-pixel :x) (swizzle cross-pixel :y)))
         (crosshair (if (> (swizzle (representation camera-position) :w) 0.5)
                        (if (< cross-long 7.0)
                            (if (< cross-short 1.6) 1.0 0.0) 0.0) 0.0))
         (cross-color (if (< cross-long 6.0)
                          (if (< cross-short 0.7) (vec3 0.95 0.95 0.95)
                              (vec3 0.08 0.08 0.08))
                          (vec3 0.08 0.08 0.08))))
    (set-output color-output
                (vec4 (mix presented cross-color crosshair)
                      1.0))))

(define-live-shader hdr-copy-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5))))
    (set-output color-output (sample scene scene-sampler uv))))
