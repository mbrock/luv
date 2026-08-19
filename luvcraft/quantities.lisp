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
(math:define-quantity-kind :relative-color-signal
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :control-signal
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :sample-count
  :dimension nil :parent :dimensionless)

(math:define-quantity :shadow-uv :kind :normalized-coordinate
  :character :point
  :components (:shadow-u :shadow-v))
(math:define-quantity :texture-uv :kind :normalized-coordinate
  :character :point
  :components (:texture-u :texture-v))
(math:define-quantity :tile-local-uv :kind :normalized-coordinate
  :character :point
  :components (:tile-local-u :tile-local-v))
(math:define-quantity :atlas-tile-offset :kind :sample-count
  :non-negative-p t)
(math:define-quantity :atlas-texel-width :kind :sample-count
  :non-negative-p t)
(math:define-quantity :shadow-depth :kind :normalized-coordinate
  :character :point)
(math:define-quantity :sun-disc-coordinate :kind :normalized-coordinate)
(math:define-quantity :shadow-depth-gradient :kind :normalized-gradient)
(math:define-quantity :linear-rgb :kind :relative-color-signal
  :non-negative-p t)
(math:define-quantity :linear-rgba :kind :relative-color-signal
  :non-negative-p t)
(math:define-quantity :day-factor :kind :proportion
  :non-negative-p t)
(math:define-quantity :opacity :kind :proportion
  :non-negative-p t)
(math:define-quantity :ambient-occlusion :kind :proportion
  :non-negative-p t)
(math:define-quantity :fog-amount :kind :proportion
  :non-negative-p t)
(math:define-quantity :sky-light-level :kind :proportion
  :non-negative-p t)
(math:define-quantity :block-light-level :kind :proportion
  :non-negative-p t)
(math:define-quantity :material-emission :kind :proportion
  :non-negative-p t)
;;; A material's own micro-surface height, retained in the generated normal
;;; atlas alongside the unit normal derived from it.
(math:define-quantity :surface-relief :kind :proportion
  :non-negative-p t)
;;; The normal atlas stores a tangent-space unit normal in unsigned texture
;;; channels.  Sampling produces this encoded control value; the block shader
;;; recentres it before placing it in the face's world-space tangent frame.
(math:define-quantity :surface-normal-sample :kind :control-signal
  :non-negative-p t)
(math:define-quantity :sky-propagation-level :kind :sample-count
  :non-negative-p t)
(math:define-quantity :block-propagation-level :kind :sample-count
  :non-negative-p t)
(math:define-quantity :shadow-diagnostic :kind :control-signal)
;;; Per-face edge shaping: what the mesher knows about a block face's four
;;; in-plane boundaries.  Signed, because a concave edge and a convex one are
;;; opposite shapings of the same surface rather than different amounts of one.
(math:define-quantity :edge-shaping :kind :control-signal)
(math:define-quantity :packed-edge-shaping :kind :control-signal
  :non-negative-p t)
(math:define-quantity :shadow-filter-radius :kind :sample-count
  :non-negative-p t)
(math:define-quantity :view-distance :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :player-half-width :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :player-height :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :player-eye-height :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :player-walk-speed :kind :lattice-velocity
  :non-negative-p t)
(math:define-quantity :player-jump-speed :kind :lattice-velocity
  :non-negative-p t)
(math:define-quantity :player-acceleration :kind :lattice-acceleration
  :non-negative-p t)
;;; An animal is measured in the same lattice as the player it shares the
;;; world with, but its own dimensions and gait are its own quantities: a
;;; turtle's walking speed is not a slow player's.
(math:define-quantity :critter-half-width :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :critter-height :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :critter-walk-speed :kind :lattice-velocity
  :non-negative-p t)
(math:define-quantity :critter-behavior-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :gravity-magnitude :kind :lattice-acceleration
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
(math:define-quantity :lighting-reconciliation-duration :kind :duration
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
(math:define-quantity :production-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :physics-accumulated-duration :kind :duration
  :non-negative-p t)
(math:define-quantity :monotonic-frame-time :kind :duration
  :character :point)
;;; The sky's own clock: a bounded elapsed time the cloud deck drifts with.
;;; It is a point on a wrapped timeline rather than a duration measured
;;; between two events, which is why it is not :FRAME-DURATION.
(math:define-quantity :sky-time :kind :duration
  :character :point)
(math:define-quantity :cloudiness :kind :proportion
  :non-negative-p t)
(math:define-quantity :camera-yaw :kind :angular-measure
  :character :point)
(math:define-quantity :critter-yaw :kind :angular-measure
  :character :point)
(math:define-quantity :camera-pitch :kind :angular-measure
  :character :point)
(math:define-quantity :camera-field-of-view :kind :angular-measure
  :non-negative-p t)
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
  :character :point
  :components
  (:clip-x-coordinate :clip-y-coordinate :clip-z-coordinate))

;;; Units and quantities the knobs (KNOBS.LISP) present values in.  A knob's
;;; unit is the same vocabulary the arithmetic checks, so the metabar prints
;;; the unit the shader would have typed.
(math:define-unit :minute :reference :second :magnitude 60
  :quantity-kind :duration)
(math:define-unit :hour :reference :second :magnitude 3600
  :quantity-kind :duration)
(math:define-unit :degree :reference :radian
  :magnitude #.(/ pi 180)
  :quantity-kind :angular-measure)
(math:define-unit :milliradian :reference :radian :magnitude 1/1000
  :quantity-kind :angular-measure)

(math:define-quantity :time-of-day :kind :duration
  :character :point)
(math:define-quantity :day-length :kind :duration
  :non-negative-p t)
(math:define-quantity :emission-gain :kind :control-signal
  :non-negative-p t)
(math:define-quantity :lens-gain :kind :control-signal
  :non-negative-p t)
(math:define-quantity :luminance-threshold :kind :control-signal
  :non-negative-p t)
(math:define-quantity :vignette-strength :kind :proportion
  :non-negative-p t)
(math:define-quantity :shaft-decay :kind :proportion
  :non-negative-p t)
(math:define-quantity :sun-orbit-tilt :kind :control-signal)
(math:define-quantity :sun-disc-scale :kind :control-signal
  :non-negative-p t)
(math:define-quantity :sun-disc-radiance :kind :control-signal
  :non-negative-p t)
(math:define-quantity :direct-light-gain :kind :control-signal
  :non-negative-p t)
(math:define-quantity :screen-effect-strength :kind :proportion
  :non-negative-p t)
(math:define-quantity :screen-curvature :kind :control-signal
  :non-negative-p t)
(math:define-quantity :scanline-count :kind :sample-count
  :non-negative-p t)
;;; The render target's extent in pixels, carried in the frame uniform so a
;;; vertex stage can size a pixel (the world text's dilation).
(math:define-quantity :target-pixel-extent :kind :sample-count
  :non-negative-p t)
(math:define-quantity :font-scale :kind :control-signal
  :non-negative-p t)
(math:define-quantity :chunk-radius :kind :sample-count
  :non-negative-p t)
(math:define-quantity :frame-budget :kind :sample-count
  :non-negative-p t)
(math:define-quantity :critter-count :kind :sample-count
  :non-negative-p t)
(math:define-quantity :seat-offset :kind :lattice-coordinate)
(math:define-quantity :seat-pitch :kind :angular-measure)
(math:define-quantity :switch :kind :control-signal)

;;; What the reworked sky and surface materials measure.  Cloud cover and
;;; star and moon radiance are sky facts; detail, roughness, and bounce are
;;; facts about a surface's micro-structure and the light it gathers from
;;; everything that is not the sun.  The grading pair at the end are the two
;;; display controls the filmic curve leaves to art direction.
(math:define-quantity :cloud-coverage :kind :proportion
  :non-negative-p t)
(math:define-quantity :cloud-altitude :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :cloud-shadow :kind :proportion
  :non-negative-p t)
(math:define-quantity :star-brightness :kind :control-signal
  :non-negative-p t)
(math:define-quantity :moon-radiance :kind :control-signal
  :non-negative-p t)
(math:define-quantity :scatter-gain :kind :control-signal
  :non-negative-p t)
(math:define-quantity :surface-detail :kind :proportion
  :non-negative-p t)
(math:define-quantity :surface-roughness :kind :proportion
  :non-negative-p t)
(math:define-quantity :specular-gain :kind :control-signal
  :non-negative-p t)
(math:define-quantity :ambient-bounce :kind :proportion
  :non-negative-p t)
(math:define-quantity :chromatic-aberration :kind :proportion
  :non-negative-p t)
(math:define-quantity :grade-saturation :kind :control-signal
  :non-negative-p t)
(math:define-quantity :grade-contrast :kind :control-signal
  :non-negative-p t)
(math:define-quantity :bounciness :kind :control-signal
  :non-negative-p t)
(math:define-quantity :body-count :kind :sample-count
  :non-negative-p t)
(math:define-quantity :substep-count :kind :sample-count
  :non-negative-p t)
