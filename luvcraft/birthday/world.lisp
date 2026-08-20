;;; The birthday meadow: a gentle world for a small person to run around in.
;;;
;;; Contract:
;;;   Class BIRTHDAY-WORLD-SOURCE, a subclass of LUVCRAFT::LITTLE-WORLD-SOURCE,
;;;   generating gentle grassy hills (no snow, no desert), with the ground
;;;   blending toward one flat party disc around the origin so the gazebo has
;;;   a natural clearing.  Flower blocks freckle the grass, denser near the
;;;   party.  Sparse pretty trees stay outside the clearing.
;;;
;;;   (MAKE-BIRTHDAY-WORLD &key seed chunk-radius chunk-height) => block world
;;;   whose source is a BIRTHDAY-WORLD-SOURCE; CHUNK-HEIGHT defaults to 32 so
;;;   there is sky for balloons and fireworks.
;;;
;;;   The save description must round-trip: WORLD-SOURCE-SAVE-DESCRIPTION and
;;;   its restorer must know this source class, so a world saved under
;;;   worlds/alex-birthday.sexp comes back as the same meadow with its edits.

(in-package #:luvcraft.birthday)
