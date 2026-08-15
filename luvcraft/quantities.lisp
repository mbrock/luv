;;; Semantic quantities shared by luvcraft's CPU and GPU arithmetic.
;;;
;;; These definitions describe the block-world domain, not an execution
;;; backend.  Their EQL methods are available after loading
;;; :LUV/LUVCRAFT/QUANTITIES without loading the shader or SPIR-V systems.

(in-package #:luvcraft.quantities)

;;; These kinds classify which units make sense without collapsing the much
;;; finer exact quantity names used by arithmetic.  They follow mp-units' key
;;; distinction: many meanings share dimension one and unit ONE, while units
;;; such as RADIAN remain confined to their own semantic subtree.
(math:define-quantity-kind :normalized-coordinate
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :normalized-gradient
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :unit-direction
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :relative-color-signal
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :control-signal
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :sample-count
  :dimension nil :parent :dimensionless)

(math:define-quantity :shadow-uv :kind :normalized-coordinate
  :components (:shadow-u :shadow-v))
(math:define-quantity :texture-uv :kind :normalized-coordinate
  :components (:texture-u :texture-v))
(math:define-quantity :shadow-depth :kind :normalized-coordinate)
(math:define-quantity :sun-disc-coordinate :kind :normalized-coordinate)
(math:define-quantity :shadow-depth-gradient :kind :normalized-gradient)
(math:define-quantity :world-direction :kind :unit-direction
  :components
  (:world-x-direction :world-y-direction :world-z-direction))
(math:define-quantity :linear-rgb :kind :relative-color-signal)
(math:define-quantity :linear-rgba :kind :relative-color-signal)
(math:define-quantity :day-factor :kind :proportion)
(math:define-quantity :opacity :kind :proportion)
(math:define-quantity :ambient-occlusion :kind :proportion)
(math:define-quantity :fog-amount :kind :proportion)
(math:define-quantity :sky-light-level :kind :proportion)
(math:define-quantity :block-light-level :kind :proportion)
(math:define-quantity :material-emission :kind :proportion)
(math:define-quantity :shadow-diagnostic :kind :control-signal)
(math:define-quantity :shadow-filter-radius :kind :sample-count)
(math:define-quantity :world-distance :kind :length)
(math:define-quantity :view-distance :kind :length)
(math:define-quantity :world-position :kind :length
  :components (:world-x-position :world-y-position :world-z-position))
(math:define-quantity :projection-scale :kind :control-signal)
(math:define-quantity :clip-coordinate :kind :normalized-coordinate
  :components
  (:clip-x-coordinate :clip-y-coordinate :clip-z-coordinate))
