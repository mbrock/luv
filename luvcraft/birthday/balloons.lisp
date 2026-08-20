;;; Balloons: bright SDF spheres-with-knots bobbing on their strings.
;;;
;;; Contract:
;;;   (ADD-BIRTHDAY-BALLOONS session balloons) registers one scene overlay
;;;   drawing every balloon as an instanced, camera-facing, sphere-traced
;;;   billboard in the manner of the gnome's body overlay.  BALLOONS is a
;;;   list of plists (:x :y :z :radius :hue :phase); the overlay owns its
;;;   instance buffer and pipelines.  Balloons bob and sway gently on the
;;;   frame clock with per-instance phase, and each wears its own bright
;;;   glossy colour.  (REMOVE-BIRTHDAY-BALLOONS session) releases the overlay.
;;;
;;;   Shader sections live under (in-package #:luvcraft.shaders).

(in-package #:luvcraft.birthday)
