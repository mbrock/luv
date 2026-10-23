;;; Attachment point lights
;;;
;;; Lanterns and torches light their surroundings directly: each frame the
;;; renderer writes the eight attachment lights nearest the eye into the tail
;;; of the scene uniform (POINT-LIGHT-0 ... POINT-LIGHT-COLOR-7), and every
;;; lit surface adds their Lambert terms.  The reach is the light's RGB4 level
;;; in cells, the distance Luvcraft's block light fades over.  Nothing
;;; occludes these lights yet; a wall between a lantern and a surface facing
;;; it does not stop the light.

(in-package #:luft.render)

(defconstant +point-light-count+ 8)
(defconstant +point-light-scalar-count+ (* 8 +point-light-count+))
(defconstant +point-light-uniform-offset+ 136
  "The scalar offset of POINT-LIGHT-0 in the scene uniform.")

(in-package #:luft.render.shaders)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *lantern-glass-emission* 1.0
    "HDR strength of a lantern's lit glass, over its glow profile."))

(define-shader-function point-light-radiance (world normal light colour)
  "Irradiance at WORLD, facing NORMAL, from one LIGHT of COLOUR.

LIGHT is a position and reach.  The response falls off as an inverse square
softened near the source and windowed to zero at the reach."
  (let* ((delta (- (swizzle light :xyz) world))
         (distance-squared (dot delta delta))
         (distance (sqrt distance-squared))
         (direction (/ delta (max distance 0.001)))
         ;; A little wrap keeps a lantern on the floor from leaving the floor
         ;; beside it black where the light grazes it.
         (facing (clamp (/ (+ (dot normal direction) 0.15) 1.15) 0.0 1.0))
         (reach (clamp (- 1.0 (/ distance (max (swizzle light :w) 0.001)))
                       0.0 1.0)))
    (* (swizzle colour :xyz)
       (/ (* facing (* reach reach)) (+ 0.6 (* 0.4 distance-squared))))))

(define-shader-function point-lights-radiance
    (world normal l0 c0 l1 c1 l2 c2 l3 c3 l4 c4 l5 c5 l6 c6 l7 c7)
  "The sum of the eight uniform attachment lights at WORLD facing NORMAL."
  (+ (point-light-radiance world normal l0 c0)
     (point-light-radiance world normal l1 c1)
     (point-light-radiance world normal l2 c2)
     (point-light-radiance world normal l3 c3)
     (point-light-radiance world normal l4 c4)
     (point-light-radiance world normal l5 c5)
     (point-light-radiance world normal l6 c6)
     (point-light-radiance world normal l7 c7)))
