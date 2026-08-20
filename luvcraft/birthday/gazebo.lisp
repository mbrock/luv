;;; The gazebo: a big beautiful pavilion at the heart of the party.
;;;
;;; Contract:
;;;   (BUILD-BIRTHDAY-GAZEBO world &key center-x center-z ground-y radius)
;;;   builds the gazebo out of block edits (LUVCRAFT:EDIT-BLOCK-AT inside one
;;;   LUVCRAFT:WITH-WORLD-CHANGE-TRANSACTION; the caller relights) and returns
;;;   a plist describing the interesting anchor points for the other
;;;   decorations:
;;;     (:floor-y N
;;;      :roof-apex (x y z)          ; above the finial, where a balloon bundle ties
;;;      :post-tops ((x y z) ...)    ; top of each post, for balloon strings
;;;      :cake (x y z))              ; where the cake stands
;;;
;;;   Blocks are the existing palette only: stone bricks and cobblestone for
;;;   the base, planks for floor and railings, wood for posts, slate for the
;;;   tiered roof, crystal for the finial and garland lights, snow for the
;;;   cake's frosting.  Leave openings so a four-year-old can run straight in.

(in-package #:luvcraft.birthday)
