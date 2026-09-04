;;; Semantic quantities shared by Luft's CPU frame writers and GPU arithmetic.
;;;
;;; The public names are ordinary symbols owned by LUFT.RENDER.QUANTITIES,
;;; rather than keywords.  Each is a self-designating constant because the
;;; arithmetic definition macros accept their semantic names as forms.  This
;;; keeps Luft's definitions distinct from the keyword vocabulary installed by
;;; Luvcraft when the full atelier is loaded in the same image.

(in-package #:luft.render.quantities)

(defconstant spatial-coordinate 'spatial-coordinate)
(defconstant unit-direction 'unit-direction)
(defconstant orientation-vector 'orientation-vector)
(defconstant normalized-coordinate 'normalized-coordinate)
(defconstant relative-color-signal 'relative-color-signal)
(defconstant control-signal 'control-signal)
(defconstant sample-count 'sample-count)
(defconstant cell 'cell)

(defconstant world-position 'world-position)
(defconstant world-x-position 'world-x-position)
(defconstant world-y-position 'world-y-position)
(defconstant world-z-position 'world-z-position)
(defconstant world-velocity 'world-velocity)
(defconstant world-acceleration 'world-acceleration)
(defconstant world-direction 'world-direction)
(defconstant world-x-direction 'world-x-direction)
(defconstant world-y-direction 'world-y-direction)
(defconstant world-z-direction 'world-z-direction)
(defconstant world-orientation 'world-orientation)
(defconstant world-x-orientation 'world-x-orientation)
(defconstant world-y-orientation 'world-y-orientation)
(defconstant world-z-orientation 'world-z-orientation)
(defconstant horizontal-direction 'horizontal-direction)
(defconstant horizontal-x-direction 'horizontal-x-direction)
(defconstant horizontal-y-direction 'horizontal-y-direction)
(defconstant world-distance 'world-distance)
(defconstant spatial-scale 'spatial-scale)
(defconstant gait-phase 'gait-phase)

(defconstant texture-coordinate 'texture-coordinate)
(defconstant texture-u-coordinate 'texture-u-coordinate)
(defconstant texture-v-coordinate 'texture-v-coordinate)
(defconstant temporal-jitter 'temporal-jitter)
(defconstant temporal-x-jitter 'temporal-x-jitter)
(defconstant temporal-y-jitter 'temporal-y-jitter)
(defconstant texel-extent 'texel-extent)
(defconstant texel-width 'texel-width)
(defconstant texel-height 'texel-height)
(defconstant shadow-coordinate 'shadow-coordinate)
(defconstant shadow-u-coordinate 'shadow-u-coordinate)
(defconstant shadow-v-coordinate 'shadow-v-coordinate)
(defconstant shadow-depth-coordinate 'shadow-depth-coordinate)
(defconstant shadow-bias 'shadow-bias)
(defconstant shadow-filter-radius 'shadow-filter-radius)
(defconstant bevel-proportion 'bevel-proportion)
(defconstant construction-line-strength 'construction-line-strength)
(defconstant inspection-ink-strength 'inspection-ink-strength)

(defconstant elapsed-time 'elapsed-time)
(defconstant scene-radiance 'scene-radiance)
(defconstant scene-red-radiance 'scene-red-radiance)
(defconstant scene-green-radiance 'scene-green-radiance)
(defconstant scene-blue-radiance 'scene-blue-radiance)
(defconstant scene-luminance 'scene-luminance)
(defconstant exposure 'exposure)
(defconstant presented-color 'presented-color)
(defconstant presented-red-color 'presented-red-color)
(defconstant presented-green-color 'presented-green-color)
(defconstant presented-blue-color 'presented-blue-color)

;;; Luft's lattice has a continuous coordinate measured in cells.  It is
;;; dimension one, but CELL is deliberately not the identity unit: values in
;;; the lattice are not silently interchangeable with arbitrary scalars.
(define-quantity-kind spatial-coordinate
  :dimension nil :parent :dimensionless)
(define-quantity-kind unit-direction
  :dimension nil :parent :dimensionless)
(define-quantity-kind orientation-vector
  :dimension nil :parent :dimensionless)
(define-quantity-kind normalized-coordinate
  :dimension nil :parent :dimensionless)
(define-quantity-kind relative-color-signal
  :dimension nil :parent :dimensionless)
(define-quantity-kind control-signal
  :dimension nil :parent :dimensionless)
(define-quantity-kind sample-count
  :dimension nil :parent :dimensionless)

(define-unit cell :dimension nil :quantity-kind spatial-coordinate)

(define-quantity world-position :kind spatial-coordinate
  :character :point
  :components (world-x-position world-y-position world-z-position))
;;; Rates are lattice rates, not SI speed/acceleration: a cell has no
;;; implicit conversion to a metre. Keep these kinds private to Luft too.
(define-quantity-kind 'spatial-velocity :dimension ((:duration -1)))
(define-quantity-kind 'spatial-acceleration :dimension ((:duration -2)))
(define-quantity world-velocity :kind 'spatial-velocity)
(define-quantity world-acceleration :kind 'spatial-acceleration)
(define-quantity world-direction :kind unit-direction
  :components (world-x-direction world-y-direction world-z-direction))
(define-quantity world-orientation :kind orientation-vector
  :components
  (world-x-orientation world-y-orientation world-z-orientation))
(define-quantity horizontal-direction :kind unit-direction
  :components (horizontal-x-direction horizontal-y-direction))
(define-quantity world-distance :kind spatial-coordinate
  :non-negative-p t)
(define-quantity spatial-scale :kind spatial-coordinate
  :non-negative-p t)
(define-quantity gait-phase :kind :angular-measure)

;;; Texture coordinates are points.  Jitter is a signed displacement and a
;;; texel extent is a non-negative displacement, even though all three use the
;;; same normalized-coordinate unit ONE at the shader boundary.
(define-quantity texture-coordinate :kind normalized-coordinate
  :character :point
  :components (texture-u-coordinate texture-v-coordinate))
(define-quantity temporal-jitter :kind normalized-coordinate
  :character :difference
  :components (temporal-x-jitter temporal-y-jitter))
(define-quantity texel-extent :kind normalized-coordinate
  :non-negative-p t
  :components (texel-width texel-height))
(define-quantity shadow-coordinate :kind normalized-coordinate
  :character :point
  :components
  (shadow-u-coordinate shadow-v-coordinate shadow-depth-coordinate))
(define-quantity shadow-bias :kind normalized-coordinate
  :non-negative-p t)
(define-quantity shadow-filter-radius :kind sample-count
  :non-negative-p t)
(define-quantity bevel-proportion :kind :proportion
  :non-negative-p t)
(define-quantity construction-line-strength :kind :proportion
  :non-negative-p t)
(define-quantity inspection-ink-strength :kind :proportion
  :non-negative-p t)

;;; Effect clocks are elapsed durations.  Luft's HDR values are a named,
;;; relative scene-linear signal rather than a claim of SI radiometry.  The
;;; presented colour remains distinct because a tone-mapped display signal is
;;; not valid input to scene-linear lighting even though both use unit ONE.
(define-quantity elapsed-time :kind :duration
  :non-negative-p t)
(define-quantity scene-radiance :kind relative-color-signal
  :non-negative-p t
  :components
  (scene-red-radiance scene-green-radiance scene-blue-radiance))
(define-quantity scene-luminance :kind relative-color-signal
  :non-negative-p t)
(define-quantity exposure :kind control-signal
  :non-negative-p t)
(define-quantity presented-color :kind relative-color-signal
  :non-negative-p t
  :components
  (presented-red-color presented-green-color presented-blue-color))
