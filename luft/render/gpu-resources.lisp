(in-package #:luft.render)

;;; Small, immutable GPU programs acquire their resources once and release
;;; them in reverse order. The list is private custody, recorded at allocation;
;;; operational slots never become a second teardown inventory. Targets and
;;; resident meshes have different replacement lifetimes and keep their owners.

(defclass gpu-resource-owner ()
  ((resources :initform nil :accessor owned-gpu-resources)))

(defun own-gpu-object (owner object)
  "Adopt OBJECT, including a composed program, for reverse-order destruction."
  (push object (owned-gpu-resources owner))
  object)

(defun own-gpu-resource (owner device descriptor)
  (own-gpu-object owner (create device descriptor)))

(defun release-owned-gpu-resources (owner)
  "Release each resource, retaining failed handles for a later retry."
  (with-release-report
    (let ((retained nil))
      (dolist (resource (owned-gpu-resources owner))
        (let ((released-p nil))
          (releasing :owned-gpu-resource
            (destroy resource)
            (setf released-p t))
          (unless released-p (push resource retained))))
      (setf (owned-gpu-resources owner) (nreverse retained))))
  (values))

(defmacro with-gpu-construction ((owner) &body body)
  "Return OWNER after BODY succeeds; otherwise release its partial resources."
  (let ((completed (gensym "COMPLETED"))
        (value (gensym "OWNER")))
    `(let ((,value ,owner)
           (,completed nil))
       (unwind-protect
            (progn ,@body (setf ,completed t) ,value)
         (unless ,completed
           (with-release-warnings
             (releasing :gpu-construction
               (release-owned-gpu-resources ,value))))))))
