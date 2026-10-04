(in-package #:luft.render.shaders)

;;; Text and flat panels standing on world surfaces: terminal walls first.
;;;
;;; Each glyph is one world-space quad whose fragment stage resolves Slug's
;;; Bezier bands exactly, so lettering stays sharp at any distance and angle.
;;; The records arrive in storage buffers, six vec4 rows a glyph and four a
;;; panel, read by instance index like the lattice overlay.  Both programs
;;; join the scene pass as ordinary static geometry: they carry the frame's
;;; jitter and write reprojection motion, which is what keeps temporal
;;; upscaling from smearing a line of text as the camera moves.
;;;
;;; A glyph record:
;;;   0  quad origin XYZ        outline left
;;;   1  right edge XYZ         outline bottom
;;;   2  up edge XYZ            outline right
;;;   3  outline top            horizontal bands, vertical bands, atlas X
;;;   4  atlas Y                band bounds min X, min Y, max X
;;;   5  band bounds max Y      linear ink RGB
;;;
;;; A panel record: origin XYZ, right edge XYZ, up edge XYZ (each padded to
;;; a vec4), then premultiplied-ready linear RGB and alpha.

(define-shader-function world-quad-corner (vertex-index)
  "The unit-square corner of one of a quad's six triangle-list vertices."
  (let* ((index (float vertex-index)))
    (vec2 (if (= index 2.0) 1.0
              (if (= index 3.0) 1.0
                  (if (= index 5.0) 1.0 0.0)))
          (if (= index 1.0) 1.0
              (if (= index 4.0) 1.0
                  (if (= index 5.0) 1.0 0.0))))))

(define-shader-function world-quad-point (origin right-edge up-edge corner)
  "The world position at CORNER of the parallelogram ORIGIN, RIGHT, UP."
  (assume-quantity
   (+ origin
      (* right-edge (swizzle corner :x))
      (* up-edge (swizzle corner :y)))
   :quantity quantities:world-position :unit quantities:cell))

(define-shader-function world-quad-jittered (clip jitter)
  "Offset CLIP by this frame's temporal sample position."
  (vec4 (+ (swizzle clip :x) (* (swizzle jitter :x) (swizzle clip :w)))
        (+ (swizzle clip :y) (* (swizzle jitter :y) (swizzle clip :w)))
        (swizzle clip :z)
        (swizzle clip :w)))

(define-shader-function world-quad-edge-pixels (origin-clip edge-clip pixel-size)
  "The on-screen length in pixels between two projected points."
  (let* ((difference (- (/ (swizzle edge-clip :xy) (swizzle edge-clip :w))
                        (/ (swizzle origin-clip :xy) (swizzle origin-clip :w))))
         (pixels (/ difference (* pixel-size 2.0))))
    (sqrt (dot pixels pixels))))

(define-live-shader world-glyph-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate-output :vec2 :location 0)
               (atlas-base-output :vec2 :location 1
                                  :interpolation :flat)
               (band-bounds-output :vec4 :location 2
                                   :interpolation :flat)
               (band-counts-output :vec2 :location 3
                                   :interpolation :flat)
               (ink-output :vec3 :location 4 :interpolation :flat)
               (current-clip-output :vec4 :location 5)
               (previous-clip-output :vec4 :location 6))
     :resources ((glyph-records :storage-buffer :binding 0 :element :vec4)
                 (camera-state :uniform-block :binding 1
                  :members #.*scene-uniform-members*)))
  (let* ((base (* instance-index (uint 6.0)))
         (row-0 (buffer-element glyph-records base))
         (row-1 (buffer-element glyph-records (+ base (uint 1.0))))
         (row-2 (buffer-element glyph-records (+ base (uint 2.0))))
         (row-3 (buffer-element glyph-records (+ base (uint 3.0))))
         (row-4 (buffer-element glyph-records (+ base (uint 4.0))))
         (row-5 (buffer-element glyph-records (+ base (uint 5.0))))
         (origin (swizzle row-0 :xyz))
         (right-edge (swizzle row-1 :xyz))
         (up-edge (swizzle row-2 :xyz))
         (outline-low (vec2 (swizzle row-0 :w) (swizzle row-1 :w)))
         (outline-high (vec2 (swizzle row-2 :w) (swizzle row-3 :x)))
         (divisor (swizzle (representation render-parameters) :z))
         (pixel-size (representation (swizzle inspection-parameters :zw)))
         (corner (world-quad-corner vertex-index))
         ;; Dynamic dilation: grow the quad by a fixed number of pixels at
         ;; whatever size it lands on screen, moving its em coordinates with
         ;; it so the outline itself stays put.
         (origin-clip
           (mesh-view-clip (world-quad-point origin right-edge up-edge
                                              (vec2 0.0 0.0))
                            camera-position camera-right camera-up
                            camera-forward camera-projection divisor))
         (right-clip
           (mesh-view-clip (world-quad-point origin right-edge up-edge
                                              (vec2 1.0 0.0))
                            camera-position camera-right camera-up
                            camera-forward camera-projection divisor))
         (up-clip
           (mesh-view-clip (world-quad-point origin right-edge up-edge
                                              (vec2 0.0 1.0))
                            camera-position camera-right camera-up
                            camera-forward camera-projection divisor))
         (right-pixels (world-quad-edge-pixels origin-clip right-clip pixel-size))
         (up-pixels (world-quad-edge-pixels origin-clip up-clip pixel-size))
         (dilation (* luv.slug:slug-dilation-pixels
                      luv.slug:slug-filter-width))
         (dilated
           (+ corner
              (* (- (* corner 2.0) (vec2 1.0 1.0))
                 (vec2 (/ dilation (max right-pixels 0.001))
                       (/ dilation (max up-pixels 0.001))))))
         (point (world-quad-point origin right-edge up-edge dilated))
         (current-clip
           (mesh-view-clip point camera-position camera-right camera-up
                           camera-forward camera-projection divisor))
         (previous-clip
           (mesh-view-clip point previous-camera-position previous-camera-right
                           previous-camera-up previous-camera-forward
                           previous-camera-projection divisor))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (world-quad-jittered current-clip jitter))
    (set-output render-coordinate-output
                (+ outline-low (* (- outline-high outline-low) dilated)))
    (set-output atlas-base-output (vec2 (swizzle row-3 :w) (swizzle row-4 :x)))
    (set-output band-bounds-output
                (vec4 (swizzle row-4 :y) (swizzle row-4 :z)
                      (swizzle row-4 :w) (swizzle row-5 :x)))
    (set-output band-counts-output (vec2 (swizzle row-3 :y) (swizzle row-3 :z)))
    (set-output ink-output (swizzle row-5 :yzw))
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)))

(define-live-shader world-glyph-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (atlas-base :vec2 :location 1 :interpolation :flat)
              (band-bounds :vec4 :location 2 :interpolation :flat)
              (band-counts :vec2 :location 3 :interpolation :flat)
              (ink :vec3 :location 4 :interpolation :flat)
              (current-clip :vec4 :location 5)
              (previous-clip :vec4 :location 6))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((band-data :uint-texture-2d :binding 2)
                 (curve-data :texture-2d :binding 3)))
  ;; Slug's band walk, as in LUV.SLUG:SLUG-ATLAS-FRAGMENT-SPECIFICATION:
  ;; choose the pixel's horizontal and vertical band, walk each band's sorted
  ;; curves until one lies wholly behind the sample, and combine the two
  ;; rays' coverage.  Addresses are relative to the glyph's ATLAS-BASE.
  (let* ((one (uint 1.0))
         (width (uint 4096.0))
         (band-base (uint (swizzle atlas-base :x)))
         (curve-base (uint (swizzle atlas-base :y)))
         (horizontal-band-count (uint (swizzle band-counts :x)))
         (vertical-band-count (uint (swizzle band-counts :y)))
         (pixels-per-em (luv.slug::slug-pixels-per-em render-coordinate))
         (horizontal-position
           (clamp (/ (- (swizzle render-coordinate :y) (swizzle band-bounds :y))
                     (max (- (swizzle band-bounds :w) (swizzle band-bounds :y))
                          luv.slug:slug-root-epsilon))
                  0.0 1.0))
         (vertical-position
           (clamp (/ (- (swizzle render-coordinate :x) (swizzle band-bounds :x))
                     (max (- (swizzle band-bounds :z) (swizzle band-bounds :x))
                          luv.slug:slug-root-epsilon))
                  0.0 1.0))
         (horizontal-band-candidate
           (uint (* horizontal-position (float horizontal-band-count))))
         (vertical-band-candidate
           (uint (* vertical-position (float vertical-band-count))))
         (horizontal-band
           (if (< horizontal-band-candidate horizontal-band-count)
               horizontal-band-candidate
               (- horizontal-band-count one)))
         (vertical-band
           (if (< vertical-band-candidate vertical-band-count)
               vertical-band-candidate
               (- vertical-band-count one)))
         (horizontal-header-address (+ band-base horizontal-band))
         (vertical-header-address
           (+ band-base horizontal-band-count vertical-band))
         (horizontal-header
           (texel-load band-data
                       (uvec2 (mod horizontal-header-address width)
                              (/ horizontal-header-address width))))
         (vertical-header
           (texel-load band-data
                       (uvec2 (mod vertical-header-address width)
                              (/ vertical-header-address width))))
         (horizontal-count (swizzle horizontal-header :x))
         (horizontal-offset (swizzle horizontal-header :y))
         (vertical-count (swizzle vertical-header :x))
         (vertical-offset (swizzle vertical-header :y))
         (horizontal
           (counted-fold
               (index horizontal-count state (vec3 0.0 0.0 0.0)
                :until (luv.slug::slug-band-done-p state))
             (let* ((entry-address (+ band-base horizontal-offset index))
                    (local-location
                      (swizzle (texel-load band-data
                                           (uvec2 (mod entry-address width)
                                                  (/ entry-address width)))
                               :xy))
                    (curve-address
                      (+ curve-base (swizzle local-location :x)
                         (* (swizzle local-location :y) width)))
                    (curve (texel-load curve-data
                                       (uvec2 (mod curve-address width)
                                              (/ curve-address width))))
                    (next (texel-load curve-data
                                      (uvec2 (mod (+ curve-address one) width)
                                             (/ (+ curve-address one) width)))))
               (luv.slug::slug-horizontal-band-step
                state curve next render-coordinate pixels-per-em))))
         (vertical
           (counted-fold
               (index vertical-count state (vec3 0.0 0.0 0.0)
                :until (luv.slug::slug-band-done-p state))
             (let* ((entry-address (+ band-base vertical-offset index))
                    (local-location
                      (swizzle (texel-load band-data
                                           (uvec2 (mod entry-address width)
                                                  (/ entry-address width)))
                               :xy))
                    (curve-address
                      (+ curve-base (swizzle local-location :x)
                         (* (swizzle local-location :y) width)))
                    (curve (texel-load curve-data
                                       (uvec2 (mod curve-address width)
                                              (/ curve-address width))))
                    (next (texel-load curve-data
                                      (uvec2 (mod (+ curve-address one) width)
                                             (/ (+ curve-address one) width)))))
               (luv.slug::slug-vertical-band-step
                state curve next render-coordinate pixels-per-em))))
         (coverage
           (luv.slug::slug-combine-band-coverage
            (swizzle horizontal :x) (swizzle horizontal :y)
            (swizzle vertical :x) (swizzle vertical :y))))
    (set-output color-output (vec4 (* ink coverage) coverage))
    (set-output motion-output (mesh-temporal-motion previous-clip current-clip))))

(define-live-shader world-panel-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (color-output :vec4 :location 0 :interpolation :flat)
               (current-clip-output :vec4 :location 1)
               (previous-clip-output :vec4 :location 2))
     :resources ((panel-records :storage-buffer :binding 0 :element :vec4)
                 (camera-state :uniform-block :binding 1
                  :members #.*scene-uniform-members*)))
  (let* ((base (* instance-index (uint 4.0)))
         (origin (swizzle (buffer-element panel-records base) :xyz))
         (right-edge (swizzle (buffer-element panel-records (+ base (uint 1.0))) :xyz))
         (up-edge (swizzle (buffer-element panel-records (+ base (uint 2.0))) :xyz))
         (color (buffer-element panel-records (+ base (uint 3.0))))
         (divisor (swizzle (representation render-parameters) :z))
         (point (world-quad-point origin right-edge up-edge
                                  (world-quad-corner vertex-index)))
         (current-clip
           (mesh-view-clip point camera-position camera-right camera-up
                           camera-forward camera-projection divisor))
         (previous-clip
           (mesh-view-clip point previous-camera-position previous-camera-right
                           previous-camera-up previous-camera-forward
                           previous-camera-projection divisor))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (world-quad-jittered current-clip jitter))
    (set-output color-output color)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)))

(define-live-shader world-panel-fragment-specification
    (:stage :fragment
     :inputs ((color :vec4 :location 0 :interpolation :flat)
              (current-clip :vec4 :location 1)
              (previous-clip :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1)))
  (let* ((alpha (swizzle color :w)))
    (set-output color-output (vec4 (* (swizzle color :xyz) alpha) alpha))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))
