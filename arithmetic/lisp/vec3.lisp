;;; Lisp arithmetic realization for luv's transparent three-component value.

(in-package #:luv.arithmetic.lisp)

(defun map-vec3-binary (function left right)
  (luv:make-vec3
   (funcall function (luv:vec3-x left) (luv:vec3-x right))
   (funcall function (luv:vec3-y left) (luv:vec3-y right))
   (funcall function (luv:vec3-z left) (luv:vec3-z right))))

(defmethod lisp-binary-operation
    (function (left luv:vec3) (right luv:vec3))
  (map-vec3-binary function left right))

(defmethod lisp-binary-operation
    (function (left luv:vec3) (right number))
  (luv:make-vec3
   (funcall function (luv:vec3-x left) right)
   (funcall function (luv:vec3-y left) right)
   (funcall function (luv:vec3-z left) right)))

(defmethod lisp-binary-operation
    (function (left number) (right luv:vec3))
  (luv:make-vec3
   (funcall function left (luv:vec3-x right))
   (funcall function left (luv:vec3-y right))
   (funcall function left (luv:vec3-z right))))

(defmethod lisp-dot ((left luv:vec3) (right luv:vec3))
  (luv:vec3-dot left right))

(defmethod lisp-normalize ((vector luv:vec3))
  (luv:vec3-normalize vector))
