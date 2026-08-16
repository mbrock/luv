(defpackage #:luv.arithmetic.lisp.vec3
  (:use #:cl)
  (:local-nicknames (#:lisp #:luv.arithmetic.lisp))
  (:documentation
   "A transparent CPU vec3 and its Common Lisp arithmetic realization.")
  (:export #:vec3
           #:make-vec3
           #:vec3-component
           #:vec3-cross
           #:vec3-dot
           #:vec3-length
           #:vec3-list
           #:vec3-normalize
           #:vec3-scale
           #:vec3-x
           #:vec3-y
           #:vec3-z))
