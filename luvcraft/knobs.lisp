;;; Luvcraft's lifecycle hook; the value protocol belongs to LUV/KNOBS.
(in-package #:luvcraft)

(defgeneric toggle-luvcraft-metabar (session)
  (:documentation "Toggle SESSION's metabar when its frontend supplies one.")
  (:method (session) (declare (ignore session)) nil))
