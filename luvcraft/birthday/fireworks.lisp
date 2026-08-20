;;; Fireworks: a great big show over the meadow at dusk.
;;;
;;; Contract:
;;;   (ADD-BIRTHDAY-FIREWORKS session &key origin) registers one scene
;;;   overlay running a continuous show: shells launch from near ORIGIN every
;;;   few seconds, ascend with a sparkling trail, and burst into sphere,
;;;   ring, and willow patterns of a few hundred sparks each.  The spark
;;;   population is simulated on the CPU in REFRESH-LUVCRAFT-OVERLAY
;;;   (gravity, drag, fade) and drawn as one instanced draw of small glowing
;;;   billboards whose radiance clears the bloom threshold, so the lens makes
;;;   them blaze.  Per-spark colour comes from the shell's palette.
;;;   (STOP-BIRTHDAY-FIREWORKS session) releases the overlay.
;;;
;;;   Shader sections live under (in-package #:luvcraft.shaders).

(in-package #:luvcraft.birthday)
