(defpackage #:luv.slug
  (:use #:cl)
  (:local-nicknames (#:spv #:luv.spir-v)
                    (#:arith-lisp #:luv.arithmetic.lisp))
  (:export #:slug-outline-error
           #:slug-outline-error-reason
           #:slug-outline-error-details
           #:slug-point
           #:make-slug-point
           #:slug-point-x
           #:slug-point-y
           #:slug-quadratic
           #:make-slug-quadratic
           #:slug-quadratic-start
           #:slug-quadratic-control
           #:slug-quadratic-end
           #:make-slug-line
           #:slug-outline
           #:make-slug-outline
           #:slug-outline-contours
           #:slug-outline-curves
           #:slug-contour-signed-area
           #:slug-contour-orientation
           #:slug-band
           #:slug-band-curve-indices
           #:slug-band-ascending-curve-indices
           #:slug-packed-outline
           #:slug-packed-outline-curves
           #:slug-packed-outline-min-x
           #:slug-packed-outline-min-y
           #:slug-packed-outline-max-x
           #:slug-packed-outline-max-y
           #:slug-packed-outline-horizontal-bands
           #:slug-packed-outline-vertical-bands
           #:pack-slug-outline
           #:slug-glyph
           #:make-slug-glyph
           #:slug-glyph-character
           #:slug-glyph-outline
           #:slug-glyph-advance-width
           #:slug-glyph-left-side-bearing
           #:slug-glyph-units-per-em
           #:slug-outline-from-zpb-glyph
           #:make-slug-glyph-from-zpb-glyph
           #:load-slug-glyph
           #:slug-root-eligibility
           #:slug-quadratic-outline
           #:slug-bezier-vertex-specification
           #:slug-bezier-fragment-specification))
