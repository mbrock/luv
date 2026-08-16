;;; A transparent three-component Lisp value and its arithmetic realization.
;;; Semantic owners name what a value means; packed colours and GPU vertex
;;; lanes remain dense vectors instead.

(in-package #:luv.arithmetic.lisp.vec3)

(declaim (inline %make-vec3))
(defstruct (vec3
             (:constructor %make-vec3 (x y z)))
  "A transparent CPU spatial triple, deliberately separate from its meaning.
See #7K8UBF."
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real))

(declaim (inline make-vec3))
(defun make-vec3 (x y z)
  (check-type x real)
  (check-type y real)
  (check-type z real)
  (%make-vec3 x y z))

(declaim (inline vec3-component (setf vec3-component)))
(defun vec3-component (vector axis)
  "Return VECTOR's component on the closed geometric AXIS vocabulary."
  (check-type vector vec3)
  (ecase axis
    (:x (vec3-x vector))
    (:y (vec3-y vector))
    (:z (vec3-z vector))))

(defun (setf vec3-component) (value vector axis)
  (check-type value real)
  (check-type vector vec3)
  (ecase axis
    (:x (setf (vec3-x vector) value))
    (:y (setf (vec3-y vector) value))
    (:z (setf (vec3-z vector) value))))

(declaim (inline vec3-scale vec3-dot vec3-cross vec3-length vec3-normalize))
(defun vec3-scale (vector scale)
  (check-type vector vec3)
  (check-type scale real)
  (make-vec3 (* (vec3-x vector) scale)
             (* (vec3-y vector) scale)
             (* (vec3-z vector) scale)))

(defun vec3-dot (left right)
  (check-type left vec3)
  (check-type right vec3)
  (+ (* (vec3-x left) (vec3-x right))
     (* (vec3-y left) (vec3-y right))
     (* (vec3-z left) (vec3-z right))))

(defun vec3-cross (left right)
  (check-type left vec3)
  (check-type right vec3)
  (make-vec3 (- (* (vec3-y left) (vec3-z right))
                (* (vec3-z left) (vec3-y right)))
             (- (* (vec3-z left) (vec3-x right))
                (* (vec3-x left) (vec3-z right)))
             (- (* (vec3-x left) (vec3-y right))
                (* (vec3-y left) (vec3-x right)))))

(defun vec3-length (vector)
  (check-type vector vec3)
  (sqrt (vec3-dot vector vector)))

(defun vec3-normalize (vector)
  (check-type vector vec3)
  (let ((length (vec3-length vector)))
    (if (plusp length) (vec3-scale vector (/ length)) vector)))

(defun vec3-list (vector)
  "Return VECTOR's three components as portable external data."
  (check-type vector vec3)
  (list (vec3-x vector) (vec3-y vector) (vec3-z vector)))

(defun map-vec3-binary (function left right)
  (make-vec3
   (funcall function (vec3-x left) (vec3-x right))
   (funcall function (vec3-y left) (vec3-y right))
   (funcall function (vec3-z left) (vec3-z right))))

(defmethod lisp:lisp-binary-operation
    (function (left vec3) (right vec3))
  (map-vec3-binary function left right))

(defmethod lisp:lisp-binary-operation
    (function (left vec3) (right number))
  (make-vec3
   (funcall function (vec3-x left) right)
   (funcall function (vec3-y left) right)
   (funcall function (vec3-z left) right)))

(defmethod lisp:lisp-binary-operation
    (function (left number) (right vec3))
  (make-vec3
   (funcall function left (vec3-x right))
   (funcall function left (vec3-y right))
   (funcall function left (vec3-z right))))

(defmethod lisp:lisp-dot ((left vec3) (right vec3))
  (vec3-dot left right))

(defmethod lisp:lisp-normalize ((vector vec3))
  (vec3-normalize vector))
