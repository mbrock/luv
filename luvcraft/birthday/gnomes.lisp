;;; Dancing gnomes: the agent's SDF body, multiplied and set to music.
;;;
;;; Contract:
;;;   (ADD-DANCING-GNOMES session gnomes) registers one scene overlay drawing
;;;   every gnome as an instanced SDF billboard, reusing the gnome part
;;;   functions from luvcraft/agent/shaders.lisp (GNOME-DISTANCE and friends
;;;   live in #:luvcraft.shaders; define any dance variants here rather than
;;;   editing that file).  GNOMES is a list of plists
;;;   (:x :y :z :scale :phase :hue); each gnome dances -- bouncing, swaying,
;;;   slowly turning -- driven by the frame clock offset by its phase, and
;;;   its hue shifts the robe and hat so the troupe reads as individuals.
;;;   (REMOVE-DANCING-GNOMES session) releases the overlay.

(in-package #:luvcraft.birthday)
