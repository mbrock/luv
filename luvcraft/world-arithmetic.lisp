;;; Lisp arithmetic realization for luvcraft's transparent three-component value.

(in-package #:luv.arithmetic.lisp)

(defun map-vec3-binary (function left right)
  (luvcraft.world:make-vec3
   (funcall function (luvcraft.world:vec3-x left) (luvcraft.world:vec3-x right))
   (funcall function (luvcraft.world:vec3-y left) (luvcraft.world:vec3-y right))
   (funcall function (luvcraft.world:vec3-z left) (luvcraft.world:vec3-z right))))

(defmethod lisp-binary-operation
    (function (left luvcraft.world:vec3) (right luvcraft.world:vec3))
  (map-vec3-binary function left right))

(defmethod lisp-binary-operation
    (function (left luvcraft.world:vec3) (right number))
  (luvcraft.world:make-vec3
   (funcall function (luvcraft.world:vec3-x left) right)
   (funcall function (luvcraft.world:vec3-y left) right)
   (funcall function (luvcraft.world:vec3-z left) right)))

(defmethod lisp-binary-operation
    (function (left number) (right luvcraft.world:vec3))
  (luvcraft.world:make-vec3
   (funcall function left (luvcraft.world:vec3-x right))
   (funcall function left (luvcraft.world:vec3-y right))
   (funcall function left (luvcraft.world:vec3-z right))))

(defmethod lisp-dot ((left luvcraft.world:vec3) (right luvcraft.world:vec3))
  (luvcraft.world:vec3-dot left right))

(defmethod lisp-normalize ((vector luvcraft.world:vec3))
  (luvcraft.world:vec3-normalize vector))
