;;; Quantities owned by the world model rather than the luvcraft application.
;;;
;;; A continuous lattice coordinate is measured in CELL.  CELL is
;;; dimensionless but deliberately not the identity unit: one cell is not one
;;; metre unless a particular VOXEL-SPACE says so through its CELL-EXTENT.

(in-package #:luvcraft.world.quantities)

(math:define-quantity-kind :lattice-coordinate
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :unit-direction
  :dimension nil :parent :dimensionless)
(math:define-quantity-kind :lattice-velocity
  :dimension ((:duration -1)))
(math:define-quantity-kind :lattice-acceleration
  :dimension ((:duration -2)))

(math:define-unit :cell :dimension nil
  :quantity-kind :lattice-coordinate)

(math:define-quantity :world-direction :kind :unit-direction
  :components
  (:world-x-direction :world-y-direction :world-z-direction))
(math:define-quantity :world-distance :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :ray-distance :kind :lattice-coordinate
  :non-negative-p t)
(math:define-quantity :world-position :kind :lattice-coordinate
  :character :point
  :components (:world-x-position :world-y-position :world-z-position))
(math:define-quantity :world-velocity :kind :lattice-velocity
  :components (:world-x-velocity :world-y-velocity :world-z-velocity))
(math:define-quantity :voxel-cell-extent :kind :length
  :non-negative-p t)
