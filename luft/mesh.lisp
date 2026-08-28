(in-package #:luft)

(defconstant +mesh-cell-size+ 8
  "Integer atlas ticks per voxel edge.")

(defconstant +mesh-bevel-width+ 1
  "The sole width represented by the star atlas.")

(defstruct (surface-mesh
             (:constructor make-surface-mesh (domain star-site-words))
             (:copier nil))
  "One chunk's complete GPU input: four unsigned words per active star."
  (domain nil :type world-domain :read-only t)
  (star-site-words
    #.(make-array 0 :element-type '(unsigned-byte 32))
    :type (simple-array (unsigned-byte 32) (*))
    :read-only t)
  ;; Appearance is a parallel derived product: eight u8 material codes for
  ;; each active star, in sample-bit order.  It never participates in star
  ;; selection, atlas geometry, or triangle ownership.
  (appearance-codes #.(make-array 0 :element-type '(unsigned-byte 8))
                    :type (simple-array (unsigned-byte 8) (*)))
  (appearance-descriptor-words
    #.(make-array 0 :element-type '(unsigned-byte 32))
    :type (simple-array (unsigned-byte 32) (*)))
  ;; Renderer publication metadata.  None of these participate in geometry.
  (voxel-light nil)
  (companions nil :type list)
  (attachments nil :type list))

(defstruct (surface-attachment-frame
             (:constructor make-surface-attachment-frame
                 (origin normal tangent primitive-kinds stocks)))
  (origin #() :type vector :read-only t)
  (normal #() :type vector :read-only t)
  (tangent #() :type vector :read-only t)
  (primitive-kinds nil :read-only t)
  (stocks nil :read-only t))

(defun surface-mesh-triangle-count (mesh)
  "The exact atlas triangle count selected by MESH's star words."
  (loop with words = (surface-mesh-star-site-words mesh)
        for offset from 3 below (length words) by 4
        sum (length (star-atlas-owned-triangles (aref words offset)))))
