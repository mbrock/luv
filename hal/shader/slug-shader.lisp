;;; Slug's per-pixel mathematics: the shader functions and the tunable
;;; values they fold in as literals, apart from fonts and the atelier's
;;; pipelines so that luv-shaderc can load them.  A shader source file calls
;;; them by their LUV.SLUG names (LUV.SLUG::SLUG-HORIZONTAL-BAND-STEP and so
;;; on); hal/shader/slug.lisp builds the atelier's programs from them.
;;;
;;; The shape follows Lengyel's open reference implementation
;;; (github.com/EricLengyel/Slug, 2017-2026): sorted bands with an early
;;; exit, a nonzero or even-odd fill, an optional optical-weight boost, and
;;; a bounding polygon dilated per vertex rather than by a constant.  The
;;; constants the reference bakes in are named values here, so a live
;;; pipeline can be retuned from a knob without editing shader source.

(in-package #:luv.slug)

(defconstant +slug-root-epsilon+ (/ 1.0 65536.0))

;;; ---------------------------------------------------------------------
;;; Tunable values.
;;;
;;; Each special is one number the shaders would otherwise carry as a
;;; literal.  A shader body names it by the unstarred symbol; the parser
;;; folds that name to a literal through SHADER-SOURCE-VALUE and remembers
;;; it did, so a live pipeline rebuilds when the special moves.  A game that
;;; owns a session wraps these in knobs (LUVCRAFT::DEFINE-KNOB in
;;; luvcraft/text.lisp); here they are only the values.

(defparameter *slug-filter-width* 1.0
  "The box filter's width in pixels.  One is the reference's exact pixel
coverage; wider softens (and, past the dilation, clips) the edge.")

(defparameter *slug-fill-rule* 0.0
  "0 fills by the nonzero winding rule, 1 by even-odd; fractions blend.")

(defparameter *slug-optical-weight* 1.0
  "The exponent applied to coverage.  1 is linear coverage; the reference's
SLUG_WEIGHT is 0.5, a square root that boosts thin strokes.")

(defparameter *slug-footprint-norm* 0.0
  "How the pixel footprint in em space is measured from the coordinate
derivatives: 0 is the gradient length (L2), 1 is fwidth (L1), which is what
the reference uses.")

(defparameter *slug-early-exit* 1.0
  "1 leaves a band's sorted curve list at the first curve wholly behind
the sample; 0 walks every curve (the reference without its break).")

(defparameter *slug-root-epsilon* +slug-root-epsilon+
  "Below this |a| a curve's polynomial is solved as linear; also the floor
of every division in the coverage combination.")

(defparameter *slug-debug-view* 0.0
  "0 renders ink; 1 paints each glyph's quad with its band loads (red the
horizontal band's curve count, green the vertical's, over sixteen).")

(defparameter *slug-dilation-pixels* 0.6
  "How far a glyph's bounding quad grows past its outline, in filter widths
(pixels when the filter is one pixel wide), so the filter's half-width and a
little more always lie inside the quad.  The reference dilates exactly half
a pixel; a hair more forgives the per-vertex approximation of the pixel size
across a perspective quad.")

(defparameter *slug-static-padding* 0.0
  "A constant dilation of every glyph quad in em, added on the CPU when the
quads are laid out.  Zero leaves the work to the per-vertex dilation; the
value luv used before dynamic dilation was 0.035.")

(defmacro define-slug-source-value (name special)
  "Let NAME stand for SPECIAL's value in shader source."
  `(defmethod shader:shader-source-value ((name (eql ',name)))
     (declare (ignore name))
     (values ,special nil t)))

(define-slug-source-value slug-filter-width *slug-filter-width*)
(define-slug-source-value slug-fill-rule *slug-fill-rule*)
(define-slug-source-value slug-optical-weight *slug-optical-weight*)
(define-slug-source-value slug-footprint-norm *slug-footprint-norm*)
(define-slug-source-value slug-early-exit *slug-early-exit*)
(define-slug-source-value slug-root-epsilon *slug-root-epsilon*)
(define-slug-source-value slug-debug-view *slug-debug-view*)
(define-slug-source-value slug-dilation-pixels *slug-dilation-pixels*)

(defun slug-root-eligibility (y1 y2 y3)
  "Return the two Slug root-eligibility bits as numeric masks.

Only strict positivity matters.  The arithmetic form is Table 1 of Lengyel's
2017 paper without an integer lookup, and is the form generated into the proof
pixel shader.  #S2F8SA"
  (let ((s1 (if (plusp y1) 1 0))
        (s2 (if (plusp y2) 1 0))
        (s3 (if (plusp y3) 1 0)))
    (values (slug-first-root-eligibility s1 s2 s3)
            (slug-second-root-eligibility s1 s2 s3))))

(arith-lisp:define-lisp-arithmetic-function
    slug-first-root-eligibility ((s1) (s2) (s3))
  (+ (* s1 (- 1 (* s2 s3)))
     (* (- 1 s1) s2 (- 1 s3))))

(arith-lisp:define-lisp-arithmetic-function
    slug-second-root-eligibility ((s1) (s2) (s3))
  (+ (* s3 (- 1 (* s1 s2)))
     (* (- 1 s3) s2 (- 1 s1))))

(shader:define-shader-function slug-axis-contribution
    (p1-axis p2-axis p3-axis p1-other p2-other p3-other pixels-per-em)
  "Return coverage1, coverage2, weight1, and weight2 for one ray axis."
  (let* ((axis-a (+ (- p1-axis (* p2-axis 2.0)) p3-axis))
         (axis-b (- p1-axis p2-axis))
         (other-a (+ (- p1-other (* p2-other 2.0)) p3-other))
         (other-b (- p1-other p2-other))
         (linear
           (- 1.0
              (shader:step slug-root-epsilon (abs axis-a))))
         (safe-a (shader:mix axis-a 1.0 linear))
         (safe-b
           (shader:mix axis-b 1.0
                    (- 1.0
                       (shader:step slug-root-epsilon (abs axis-b)))))
         (discriminant
           (max (- (* axis-b axis-b) (* axis-a p1-axis)) 0.0))
         (root-distance (sqrt discriminant))
         (quadratic-t1 (/ (- axis-b root-distance) safe-a))
         (quadratic-t2 (/ (+ axis-b root-distance) safe-a))
         (linear-t (/ (* p1-axis 0.5) safe-b))
         (t1 (shader:mix quadratic-t1 linear-t linear))
         (t2 (shader:mix quadratic-t2 linear-t linear))
         ;; FSign makes zero exactly "not positive", matching the eligibility
         ;; table without epsilon classification at shared control points.
         (s1 (max (signum p1-axis) 0.0))
         (s2 (max (signum p2-axis) 0.0))
         (s3 (max (signum p3-axis) 0.0))
         (eligible1 (slug-first-root-eligibility s1 s2 s3))
         (eligible2 (slug-second-root-eligibility s1 s2 s3))
         (crossing1
           (+ (* (- (* other-a t1) (* other-b 2.0)) t1)
              p1-other))
         (crossing2
           (+ (* (- (* other-a t2) (* other-b 2.0)) t2)
              p1-other))
         (scaled1 (* crossing1 pixels-per-em))
         (scaled2 (* crossing2 pixels-per-em))
         (coverage1
           (* eligible1 (shader:clamp (+ scaled1 0.5) 0.0 1.0)))
         (coverage2
           (* eligible2 (shader:clamp (+ scaled2 0.5) 0.0 1.0)))
         (weight1
           (* eligible1
              (shader:clamp (- 1.0 (* (abs scaled1) 2.0)) 0.0 1.0)))
         (weight2
           (* eligible2
              (shader:clamp (- 1.0 (* (abs scaled2) 2.0)) 0.0 1.0))))
    (shader:vec4 coverage1 coverage2 weight1 weight2)))

(shader:define-shader-function slug-horizontal-contribution
    (p1 p2 p3 pixels-per-em)
  "Evaluate one quadratic against the pixel's horizontal winding ray."
  (slug-axis-contribution
   (shader:swizzle p1 :y) (shader:swizzle p2 :y) (shader:swizzle p3 :y)
   (shader:swizzle p1 :x) (shader:swizzle p2 :x) (shader:swizzle p3 :x)
   (shader:swizzle pixels-per-em :x)))

(shader:define-shader-function slug-vertical-contribution
    (p1 p2 p3 pixels-per-em)
  "Evaluate one quadratic against the pixel's vertical winding ray."
  (slug-axis-contribution
   (shader:swizzle p1 :x) (shader:swizzle p2 :x) (shader:swizzle p3 :x)
   (shader:swizzle p1 :y) (shader:swizzle p2 :y) (shader:swizzle p3 :y)
   (shader:swizzle pixels-per-em :y)))

(shader:define-shader-function slug-combine-coverage
    (horizontal0 horizontal1 horizontal2 horizontal3
     vertical0 vertical1 vertical2 vertical3)
  "Combine four curves' horizontal and vertical ray contributions."
  (let* ((xcov
           (+ 0.0
              (- (shader:swizzle horizontal0 :x)
                 (shader:swizzle horizontal0 :y))
              (- (shader:swizzle horizontal1 :x)
                 (shader:swizzle horizontal1 :y))
              (- (shader:swizzle horizontal2 :x)
                 (shader:swizzle horizontal2 :y))
              (- (shader:swizzle horizontal3 :x)
                 (shader:swizzle horizontal3 :y))))
         (ycov
           (+ 0.0
              (- (shader:swizzle vertical0 :y)
                 (shader:swizzle vertical0 :x))
              (- (shader:swizzle vertical1 :y)
                 (shader:swizzle vertical1 :x))
              (- (shader:swizzle vertical2 :y)
                 (shader:swizzle vertical2 :x))
              (- (shader:swizzle vertical3 :y)
                 (shader:swizzle vertical3 :x))))
         (xweight
           (max 0.0
                (shader:swizzle horizontal0 :z)
                (shader:swizzle horizontal0 :w)
                (shader:swizzle horizontal1 :z)
                (shader:swizzle horizontal1 :w)
                (shader:swizzle horizontal2 :z)
                (shader:swizzle horizontal2 :w)
                (shader:swizzle horizontal3 :z)
                (shader:swizzle horizontal3 :w)))
         (yweight
           (max 0.0
                (shader:swizzle vertical0 :z)
                (shader:swizzle vertical0 :w)
                (shader:swizzle vertical1 :z)
                (shader:swizzle vertical1 :w)
                (shader:swizzle vertical2 :z)
                (shader:swizzle vertical2 :w)
                (shader:swizzle vertical3 :z)
                (shader:swizzle vertical3 :w))))
    (slug-finish-coverage
     (max
      (/ (abs (+ (* xcov xweight) (* ycov yweight)))
         (max (+ xweight yweight) slug-root-epsilon))
      (min (abs xcov) (abs ycov))))))

(shader:define-shader-function slug-finish-coverage (winding)
  "Turn an unbounded signed WINDING estimate into a fill by the fill rule,
then boost it by the optical weight.

The nonzero rule saturates; even-odd folds the winding number back and forth
between zero and one, as the reference's SLUG_EVENODD does.  The optical
weight is an exponent: one leaves coverage linear, the reference's
SLUG_WEIGHT is one half."
  (let* ((nonzero (shader:clamp winding 0.0 1.0))
         (even-odd
           (- 1.0 (abs (- 1.0 (* (shader:fract (* winding 0.5)) 2.0)))))
         (filled (shader:mix nonzero even-odd slug-fill-rule)))
    (expt filled slug-optical-weight)))

(shader:define-shader-function slug-combine-band-coverage
    (xcov xweight ycov yweight)
  "Combine the accumulated horizontal and vertical band traversals."
  (slug-finish-coverage
   (max
    (/ (abs (+ (* xcov xweight) (* ycov yweight)))
       (max (+ xweight yweight) slug-root-epsilon))
    (min (abs xcov) (abs ycov)))))

(shader:define-shader-function slug-pixels-per-em (render-coordinate)
  "The pixel scale of the em square along each axis, from the sample
coordinate's screen derivatives, already divided by the filter width so the
rest of the pipeline works in filter widths rather than pixels.

The footprint norm chooses between the gradient length and fwidth."
  (let* ((coordinate-dx (shader:derivative-x render-coordinate))
         (coordinate-dy (shader:derivative-y render-coordinate))
         (x-gradient
           (shader:vec2 (shader:swizzle coordinate-dx :x)
                     (shader:swizzle coordinate-dy :x)))
         (y-gradient
           (shader:vec2 (shader:swizzle coordinate-dx :y)
                     (shader:swizzle coordinate-dy :y)))
         (length-footprint
           (shader:vec2 (sqrt (shader:dot x-gradient x-gradient))
                     (sqrt (shader:dot y-gradient y-gradient))))
         (width-footprint (+ (abs coordinate-dx) (abs coordinate-dy)))
         (ems-per-pixel
           (shader:mix length-footprint width-footprint slug-footprint-norm)))
    (shader:vec2
     (/ 1.0 (max (* (shader:swizzle ems-per-pixel :x) slug-filter-width)
                 slug-root-epsilon))
     (/ 1.0 (max (* (shader:swizzle ems-per-pixel :y) slug-filter-width)
                 slug-root-epsilon)))))

(shader:define-shader-function slug-horizontal-band-step
    (state curve next render-coordinate pixels-per-em)
  "Fold one horizontal-band curve into STATE = (xcov, xweight, done).

CURVE holds p1 and p2, NEXT's first two lanes p3.  DONE becomes one when the
curve lies wholly more than half a filter width left of the sample: the
band is sorted by descending maximum x, so nothing after it can contribute
and the fold's :UNTIL leaves the loop.  #3YHNO3"
  (let* ((p1 (- (shader:swizzle curve :xy) render-coordinate))
         (p2 (- (shader:swizzle curve :zw) render-coordinate))
         (p3 (- (shader:swizzle next :xy) render-coordinate))
         (contribution
           (slug-horizontal-contribution p1 p2 p3 pixels-per-em))
         (reach
           (* (max (shader:swizzle p1 :x) (shader:swizzle p2 :x)
                   (shader:swizzle p3 :x))
              (shader:swizzle pixels-per-em :x))))
    (shader:vec3
     (+ (shader:swizzle state :x)
        (- (shader:swizzle contribution :x) (shader:swizzle contribution :y)))
     (max (shader:swizzle state :y)
          (shader:swizzle contribution :z) (shader:swizzle contribution :w))
     (- 1.0 (shader:step -0.5 reach)))))

(shader:define-shader-function slug-vertical-band-step
    (state curve next render-coordinate pixels-per-em)
  "Fold one vertical-band curve into STATE = (ycov, yweight, done); the
band is sorted by descending maximum y."
  (let* ((p1 (- (shader:swizzle curve :xy) render-coordinate))
         (p2 (- (shader:swizzle curve :zw) render-coordinate))
         (p3 (- (shader:swizzle next :xy) render-coordinate))
         (contribution
           (slug-vertical-contribution p1 p2 p3 pixels-per-em))
         (reach
           (* (max (shader:swizzle p1 :y) (shader:swizzle p2 :y)
                   (shader:swizzle p3 :y))
              (shader:swizzle pixels-per-em :y))))
    (shader:vec3
     (+ (shader:swizzle state :x)
        (- (shader:swizzle contribution :y) (shader:swizzle contribution :x)))
     (max (shader:swizzle state :y)
          (shader:swizzle contribution :z) (shader:swizzle contribution :w))
     (- 1.0 (shader:step -0.5 reach)))))

(shader:define-shader-function slug-band-done-p (state)
  "Whether a band fold with STATE = (cov, weight, done) may stop: the last
curve was wholly behind the sample and the early exit is on."
  (> (* (shader:swizzle state :z) slug-early-exit) 0.5))

(shader:define-shader-function slug-texel-coordinate (address)
  "Map Slug's fixed-width linear texture ADDRESS to exact uint coordinates."
  (shader:uvec2 (mod address (shader:uint 4096.0))
             (/ address (shader:uint 4096.0))))
