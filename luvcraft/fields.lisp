;;; Luvcraft's existing dense light columns as semantic voxel fields.

(in-package #:luv)

(luv.world.fields:define-voxel-field :sky-light
  :site-kind :voxel-cell
  :value-type (unsigned-byte 8)
  :quantity (:quantity :sky-propagation-level :unit :one)
  :missing-value :unavailable
  :legal-values (integer 0 15)
  :representation :u8-levels)

(luv.world.fields:define-voxel-field :block-light
  :site-kind :voxel-cell
  :value-type (unsigned-byte 8)
  :quantity (:quantity :block-propagation-level :unit :one)
  :missing-value :unavailable
  :legal-values (integer 0 15)
  :representation :u8-levels)
