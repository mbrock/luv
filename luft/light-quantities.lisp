;;;; Meanings at the packed light boundary. No runtime lane wrappers.
(in-package #:luft)

(defconstant block-propagation-level 'block-propagation-level)
(defconstant sky-propagation-level 'sky-propagation-level)
(defconstant rgb4-light 'rgb4-light)
(defconstant block-light-proportion 'block-light-proportion)
(defconstant sky-light-proportion 'sky-light-proportion)

(luv.arithmetic:define-quantity-kind 'light-level
  :dimension nil :parent :dimensionless)
(luv.arithmetic:define-quantity 'block-propagation-level :kind 'light-level
  :non-negative-p t)
(luv.arithmetic:define-quantity 'sky-propagation-level :kind 'light-level
  :non-negative-p t)
(luv.arithmetic:define-quantity 'rgb4-light :kind 'light-level
  :non-negative-p t)
(luv.arithmetic:define-quantity 'block-light-proportion :kind :proportion
  :non-negative-p t)
(luv.arithmetic:define-quantity 'sky-light-proportion :kind :proportion
  :non-negative-p t)

;;; The old world's field declaration protocol, without loading that world.
;;; Site indices remain categorical. #GGTU6H
(luv.fields:define-voxel-field :luft-rgb4
  :site-kind :voxel-cell :value-type (unsigned-byte 12)
  :quantity (:quantity rgb4-light :unit :one)
  :missing-value 0 :legal-values (unsigned-byte 12) :representation :paged-u16)
(luv.fields:define-voxel-field :luft-sky
  :site-kind :voxel-cell :value-type (integer 0 15)
  :quantity (:quantity sky-propagation-level :unit :one)
  :missing-value :baseline :legal-values (integer 0 15) :representation :paged-u16)

(luv.fields:define-voxel-field :luft-block-lane
  :site-kind :voxel-cell :value-type (integer 0 15)
  :quantity (:quantity block-propagation-level :unit :one)
  :missing-value 0 :legal-values (integer 0 15) :representation :rgb4-lane)

(defun light-point-byte (lane)
  "The declared nibble of one RGB4/sky point, as a CPU/shader BYTE form."
  (check-type lane (integer 0 3))
  `(byte 4 ,(* 4 lane)))

(defun light-point-product-layout ()
  "Decoded RGB4 plus sky: four logical lanes, not four radiance samples."
  (luv.arithmetic:make-quantity-layout
   4
   (loop for lane below 4 collect
     (luv.arithmetic:make-quantity-projection
      (list lane)
      (luv.arithmetic:make-declared-quantity-specification
       (list :quantity (if (= lane 3) 'sky-light-proportion
                          'block-light-proportion)
             :unit :one))))))

(defun light-sidecar-bit-layout ()
  "Seven RGB4/sky readings in four u32 words; the eighth u16 is padding. #X5GB5S"
  (luv.arithmetic:make-quantity-layout
   128
   (loop for point below 7 append
     (loop for lane below 4 collect
       (luv.arithmetic:make-quantity-projection
        (loop for bit below 4 collect (+ (* point 16) (* lane 4) bit))
        (luv.arithmetic:make-declared-quantity-specification
         (list :quantity (if (= lane 3) 'sky-propagation-level
                            'block-propagation-level)
               :unit :one)))))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql 'light-sidecar)))
  (declare (ignore name))
  (load-time-value
   (luv.arithmetic:make-represented-value-declaration
    :representation-type '(simple-array (unsigned-byte 32) (*))
    :quantity-layout
    (luv.arithmetic:make-repeated-quantity-layout (light-sidecar-bit-layout))
    :source-form '(light-sidecar :stride-bits 128 :points 7 :encoding :rgb4-sky4))))
