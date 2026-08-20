(defpackage #:luv.analytic
  (:use #:cl)
  (:local-nicknames (#:arith-lisp #:luv.arithmetic.lisp)
                    (#:shader #:luv.shader))
  (:export #:roundrect-signed-distance
           #:roundrect-coverage
           #:roundrect-vertex-specification
           #:roundrect-fragment-specification
           #:lattice-fragment-specification))
