;;; Slug's quadratic outline calculation, first as a fixed proof and then as
;;; the data-driven band texture path used by real outlines.
;;;
;;; The shape of the shaders follows Lengyel's open reference implementation
;;; (github.com/EricLengyel/Slug, 2017-2026): sorted bands with an early
;;; exit, a nonzero or even-odd fill, an optional optical-weight boost, and
;;; a bounding polygon dilated per vertex rather than by a constant.  The
;;; constants the reference bakes in are named values here, so a live
;;; pipeline can be retuned from a knob without editing shader source.
;;;
;;; The per-pixel mathematics lives in slug-shader.lisp.

(in-package #:luv.slug)

(defun slug-dilation-em (pixels-per-em)
  "The em distance a glyph quad laid out at PIXELS-PER-EM should grow on
each side: the live dilation in filter widths, plus the static padding.
For a stage that cannot dilate per vertex (a flat screen quad, whose pixel
scale is known when it is laid out)."
  (+ *slug-static-padding*
     (if (plusp pixels-per-em)
         (/ (* *slug-dilation-pixels* *slug-filter-width*) pixels-per-em)
         0.0)))

(defun slug-font-cap-height (font-loader)
  "FONT-LOADER's sCapHeight from its OS/2 table, in font units, or NIL when
the font has no OS/2 table or one too old (version < 2) to carry it.
ZPB-TTF does not read OS/2, so this seeks the table itself."
  (let ((table (zpb-ttf::table-info "OS/2" font-loader)))
    (when (and table (>= (zpb-ttf::size table) 90))
      (zpb-ttf::seek-to-table table font-loader)
      (let* ((stream (zpb-ttf::input-stream font-loader))
             (version (zpb-ttf::read-uint16 stream)))
        (when (>= version 2)
          ;; sCapHeight sits at byte 88 of the table; the version was 2.
          (zpb-ttf::advance-file-position stream 86)
          (let ((cap-height (zpb-ttf::read-int16 stream)))
            (and (plusp cap-height) cap-height)))))))

(defun slug-cap-height-aligned-size (font-loader size)
  "The font size nearest SIZE (in pixels per em) at which FONT-LOADER's cap
height lands on a whole number of pixels, so the tops of most capitals
share the pixel grid: the reference's substitute for hinting.  Falls back
to SIZE when the font declares no cap height."
  (let ((cap-height (slug-font-cap-height font-loader))
        (units-per-em (zpb-ttf:units/em font-loader)))
    (if (and cap-height (plusp units-per-em))
        (let* ((cap-em (/ cap-height units-per-em))
               (pixels (max 1 (round (* size cap-em)))))
          (/ pixels cap-em))
        size)))

(shader:define-shader-function slug-quadratic-outline
    (coordinate pixels-per-em color p0 c0 p1 c1 p2 c2 p3 c3)
  "Render one connected four-quadratic contour with Slug's two-ray pixel math.

This is a typed shader function: its LET* bindings and nested calls are parsed
directly into shader objects.  The fixed curve count remains an atelier proof,
not the band-texture font renderer.  #OWR8OZ"
  (let* ((q0p0 (- p0 coordinate))
         (q0c0 (- c0 coordinate))
         (q0p1 (- p1 coordinate))
         (q1c1 (- c1 coordinate))
         (q1p2 (- p2 coordinate))
         (q2c2 (- c2 coordinate))
         (q2p3 (- p3 coordinate))
         (q3c3 (- c3 coordinate))
         (horizontal0
           (slug-horizontal-contribution
            q0p0 q0c0 q0p1 pixels-per-em))
         (horizontal1
           (slug-horizontal-contribution
            q0p1 q1c1 q1p2 pixels-per-em))
         (horizontal2
           (slug-horizontal-contribution
            q1p2 q2c2 q2p3 pixels-per-em))
         (horizontal3
           (slug-horizontal-contribution
            q2p3 q3c3 q0p0 pixels-per-em))
         (vertical0
           (slug-vertical-contribution
            q0p0 q0c0 q0p1 pixels-per-em))
         (vertical1
           (slug-vertical-contribution
            q0p1 q1c1 q1p2 pixels-per-em))
         (vertical2
           (slug-vertical-contribution
            q1p2 q2c2 q2p3 pixels-per-em))
         (vertical3
           (slug-vertical-contribution
            q2p3 q3c3 q0p0 pixels-per-em))
         (coverage
           (slug-combine-coverage
            horizontal0 horizontal1 horizontal2 horizontal3
            vertical0 vertical1 vertical2 vertical3)))
    (* color coverage)))

(shader:define-shader slug-bezier-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (outline-coordinate :vec3 :location 1)
              (pixels-per-em :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-pixels-per-em :vec2 :location 1)))
  (let* ((clip (shader:vec4 (shader:swizzle position :xy) 0.0 1.0)))
    (shader:set-output clip-position clip)
    (shader:set-output render-coordinate
                    (shader:swizzle outline-coordinate :xy))
    (shader:set-output render-pixels-per-em
                    (shader:swizzle pixels-per-em :xy))))

(shader:define-shader slug-bezier-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (pixels-per-em :vec2 :location 1))
     :outputs ((color-output :vec4 :location 0)))
  (shader:set-output
   color-output
   (slug-quadratic-outline
    render-coordinate pixels-per-em
    (shader:vec4 0.96 0.32 0.48 1.0)
    (shader:vec2 0.50 0.08) (shader:vec2 0.08 0.38)
    (shader:vec2 0.14 0.70) (shader:vec2 0.18 0.98)
    (shader:vec2 0.50 0.74) (shader:vec2 0.82 0.98)
    (shader:vec2 0.86 0.70) (shader:vec2 0.92 0.38))))

(shader:define-live-shader slug-banded-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (pixels-per-em :vec2 :location 1))
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  ;; One band per axis is the complete correctness path for an outline: every
  ;; serialized curve participates.  Subdividing the same lists into spatial
  ;; bands is the subsequent culling optimization, not a different algorithm.
  (let* ((zero (shader:uint 0.0))
         (horizontal-header-location (shader:uvec2 zero zero))
         (vertical-header-location
           (shader:uvec2 (shader:uint 1.0) zero))
         (horizontal-header
           (shader:texel-load band-data horizontal-header-location))
         (vertical-header
           (shader:texel-load band-data vertical-header-location))
         (horizontal-count (shader:swizzle horizontal-header :x))
         (horizontal-offset (shader:swizzle horizontal-header :y))
         (vertical-count (shader:swizzle vertical-header :x))
         (vertical-offset (shader:swizzle vertical-header :y))
         (horizontal
           (shader:counted-fold
               (index horizontal-count state (shader:vec3 0.0 0.0 0.0)
                :until (slug-band-done-p state))
             (let* ((entry-address (+ horizontal-offset index))
                    (curve-location
                      (shader:swizzle
                       (shader:texel-load
                        band-data
                        (slug-texel-coordinate entry-address))
                       :xy))
                    (next-location
                      (slug-texel-coordinate
                       (+ (shader:swizzle curve-location :x)
                          (* (shader:swizzle curve-location :y)
                             (shader:uint 4096.0))
                          (shader:uint 1.0))))
                    (curve (shader:texel-load curve-data curve-location))
                    (next (shader:texel-load curve-data next-location)))
               (slug-horizontal-band-step
                state curve next render-coordinate pixels-per-em))))
         (vertical
           (shader:counted-fold
               (index vertical-count state (shader:vec3 0.0 0.0 0.0)
                :until (slug-band-done-p state))
             (let* ((entry-address (+ vertical-offset index))
                    (curve-location
                      (shader:swizzle
                       (shader:texel-load
                        band-data
                        (slug-texel-coordinate entry-address))
                       :xy))
                    (next-location
                      (slug-texel-coordinate
                       (+ (shader:swizzle curve-location :x)
                          (* (shader:swizzle curve-location :y)
                             (shader:uint 4096.0))
                          (shader:uint 1.0))))
                    (curve (shader:texel-load curve-data curve-location))
                    (next (shader:texel-load curve-data next-location)))
               (slug-vertical-band-step
                state curve next render-coordinate pixels-per-em))))
         (coverage
           (slug-combine-band-coverage
            (shader:swizzle horizontal :x) (shader:swizzle horizontal :y)
            (shader:swizzle vertical :x) (shader:swizzle vertical :y))))
    (shader:set-output
     color-output
     (* (shader:vec4 0.96 0.32 0.48 1.0) coverage))))

(shader:define-live-shader slug-atlas-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (atlas-base :vec2 :location 1)
              (band-bounds :vec4 :location 2)
              (band-counts :vec2 :location 3)
              (render-color :vec4 :location 4))
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  ;; The reference's SlugRender: pick the pixel's horizontal and vertical
  ;; band, walk each band's sorted curve list until a curve is wholly behind
  ;; the sample, and combine the two rays' coverage by their weights.  The
  ;; glyph's data lives at ATLAS-BASE inside shared band and curve atlases;
  ;; every address below is relative to it.
  (let* ((one (shader:uint 1.0))
         (width (shader:uint 4096.0))
         (band-base (shader:uint (shader:swizzle atlas-base :x)))
         (curve-base (shader:uint (shader:swizzle atlas-base :y)))
         (horizontal-band-count
           (shader:uint (shader:swizzle band-counts :x)))
         (vertical-band-count
           (shader:uint (shader:swizzle band-counts :y)))
         (pixels-per-em (slug-pixels-per-em render-coordinate))
         (horizontal-position
           (shader:clamp
            (/ (- (shader:swizzle render-coordinate :y)
                  (shader:swizzle band-bounds :y))
               (max (- (shader:swizzle band-bounds :w)
                       (shader:swizzle band-bounds :y))
                    slug-root-epsilon))
            0.0 1.0))
         (vertical-position
           (shader:clamp
            (/ (- (shader:swizzle render-coordinate :x)
                  (shader:swizzle band-bounds :x))
               (max (- (shader:swizzle band-bounds :z)
                       (shader:swizzle band-bounds :x))
                    slug-root-epsilon))
            0.0 1.0))
         (horizontal-band-candidate
           (shader:uint
            (* horizontal-position (shader:float horizontal-band-count))))
         (vertical-band-candidate
           (shader:uint
            (* vertical-position (shader:float vertical-band-count))))
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
         (horizontal-header-location
           (shader:uvec2 (mod horizontal-header-address width)
                      (/ horizontal-header-address width)))
         (vertical-header-location
           (shader:uvec2 (mod vertical-header-address width)
                      (/ vertical-header-address width)))
         (horizontal-header
           (shader:texel-load band-data horizontal-header-location))
         (vertical-header
           (shader:texel-load band-data vertical-header-location))
         (horizontal-count (shader:swizzle horizontal-header :x))
         (horizontal-offset (shader:swizzle horizontal-header :y))
         (vertical-count (shader:swizzle vertical-header :x))
         (vertical-offset (shader:swizzle vertical-header :y))
         (horizontal
           (shader:counted-fold
               (index horizontal-count state (shader:vec3 0.0 0.0 0.0)
                :until (slug-band-done-p state))
             (let* ((entry-address (+ band-base horizontal-offset index))
                    (entry-location
                      (shader:uvec2 (mod entry-address width)
                                 (/ entry-address width)))
                    (local-location
                      (shader:swizzle
                       (shader:texel-load band-data entry-location)
                       :xy))
                    (curve-address
                      (+ curve-base
                         (shader:swizzle local-location :x)
                         (* (shader:swizzle local-location :y) width)))
                    (curve-location
                      (shader:uvec2 (mod curve-address width)
                                 (/ curve-address width)))
                    (next-location
                      (shader:uvec2 (mod (+ curve-address one) width)
                                 (/ (+ curve-address one) width)))
                    (curve (shader:texel-load curve-data curve-location))
                    (next (shader:texel-load curve-data next-location)))
               (slug-horizontal-band-step
                state curve next render-coordinate pixels-per-em))))
         (vertical
           (shader:counted-fold
               (index vertical-count state (shader:vec3 0.0 0.0 0.0)
                :until (slug-band-done-p state))
             (let* ((entry-address (+ band-base vertical-offset index))
                    (entry-location
                      (shader:uvec2 (mod entry-address width)
                                 (/ entry-address width)))
                    (local-location
                      (shader:swizzle
                       (shader:texel-load band-data entry-location)
                       :xy))
                    (curve-address
                      (+ curve-base
                         (shader:swizzle local-location :x)
                         (* (shader:swizzle local-location :y) width)))
                    (curve-location
                      (shader:uvec2 (mod curve-address width)
                                 (/ curve-address width)))
                    (next-location
                      (shader:uvec2 (mod (+ curve-address one) width)
                                 (/ (+ curve-address one) width)))
                    (curve (shader:texel-load curve-data curve-location))
                    (next (shader:texel-load curve-data next-location)))
               (slug-vertical-band-step
                state curve next render-coordinate pixels-per-em))))
         (coverage
           (slug-combine-band-coverage
            (shader:swizzle horizontal :x) (shader:swizzle horizontal :y)
            (shader:swizzle vertical :x) (shader:swizzle vertical :y)))
         (ink (* render-color coverage))
         ;; The debug view paints the whole quad with the two bands' loads,
         ;; so the banding of a glyph -- and what the band count buys -- can
         ;; be seen at a glance.
         (band-load
           (shader:vec4 (/ (shader:float horizontal-count) 16.0)
                     (/ (shader:float vertical-count) 16.0)
                     (* coverage 0.5)
                     1.0)))
    (shader:set-output color-output (shader:mix ink band-load slug-debug-view))))
