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
(math:define-quantity-kind :velocity
  :dimension ((:length 1) (:duration -1)))
(math:define-quantity-kind :acceleration
  :dimension ((:length 1) (:duration -2)))

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
(math:define-quantity :world-velocity :kind :velocity
  :components (:world-x-velocity :world-y-velocity :world-z-velocity))
(math:define-quantity :player-half-width :kind :length
  :non-negative-p t)
(math:define-quantity :player-height :kind :length
  :non-negative-p t)
(math:define-quantity :player-eye-height :kind :length
  :non-negative-p t)
(math:define-quantity :player-walk-speed :kind :velocity
  :non-negative-p t)
(math:define-quantity :player-jump-speed :kind :velocity
  :non-negative-p t)
(math:define-quantity :player-acceleration :kind :acceleration
  :non-negative-p t)
(math:define-quantity :gravity-magnitude :kind :acceleration
  :non-negative-p t)
(math:define-quantity :frame-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :frame-cpu-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :simulation-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :streaming-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :presentation-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :shader-refresh-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :mesh-publication-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :uniform-update-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :shadow-encode-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :scene-encode-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :surface-copy-encode-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :benchmark-completion-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :benchmark-drain-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :camera-yaw :kind :angular-measure
  :character :point)
(math:define-quantity :camera-pitch :kind :angular-measure
  :character :point)
(math:define-quantity :look-sensitivity :kind :angular-measure
  :non-negative-p t)
(math:define-quantity :sky-cycle-rate :kind :frequency
  :non-negative-p t)
(math:define-quantity :block-light-attenuation-step :kind :sample-count
  :non-negative-p t)
(math:define-quantity :block-light-emission-step :kind :sample-count
  :non-negative-p t)
(math:define-quantity :day-fraction :kind :normalized-coordinate
  :non-negative-p t)
(math:define-quantity :sun-angular-width :kind :angular-measure
  :non-negative-p t)
(math:define-quantity :exposure :kind :control-signal
  :non-negative-p t)
(math:define-quantity :projection-scale :kind :control-signal)
(math:define-quantity :clip-coordinate :kind :normalized-coordinate
  :components
  (:clip-x-coordinate :clip-y-coordinate :clip-z-coordinate))
