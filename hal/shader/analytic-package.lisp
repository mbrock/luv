(defpackage #:luv.analytic
  (:use #:cl)
  (:local-nicknames (#:arith-lisp #:luv.arithmetic.lisp)
                    (#:spv #:luv.spir-v))
  (:export #:roundrect-signed-distance
           #:roundrect-coverage
           #:roundrect-vertex-specification
           #:roundrect-fragment-specification))
