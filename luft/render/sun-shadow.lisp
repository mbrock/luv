(in-package #:luft.render)

;;; One sun depth image is shared by all shadow casters and lighting readers.
;;; Geometry components own the programs that write it. Frames own bindings
;;; that borrow its view and comparison sampler and retire before this owner.

(defclass sun-shadow (gpu-resource-owner)
  ((texture :accessor sun-shadow-texture)
   (view :accessor sun-shadow-view)
   (sampler :accessor sun-shadow-sampler)))

(defun make-sun-shadow (device)
  (let ((shadow (make-instance 'sun-shadow)))
    (with-gpu-construction (shadow)
      (setf (sun-shadow-texture shadow)
            (own-gpu-resource
             shadow device
             (make-texture-descriptor
              :label "luft sun shadow depth" :size (list +shadow-map-size+ +shadow-map-size+)
              :dimensions :2d :format :depth32-float :usage '(:render-attachment :texture-binding)))
            (sun-shadow-view shadow)
            (own-gpu-resource shadow device
                              (make-texture-view-descriptor :texture (sun-shadow-texture shadow)))
            (sun-shadow-sampler shadow)
            (own-gpu-resource
             shadow device
             (make-sampler-descriptor
              :label "luft soft shadow comparison sampler"
              :mag-filter :linear :min-filter :linear
              :mipmap-filter :nearest :compare :less-or-equal))))))
