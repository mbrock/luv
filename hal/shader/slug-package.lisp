(defpackage #:luv.slug
  (:use #:cl)
  (:local-nicknames (#:spv #:luv.spir-v)
                    (#:arith-lisp #:luv.arithmetic.lisp))
  (:export #:slug-root-eligibility
           #:slug-quadratic-outline
           #:slug-bezier-vertex-specification
           #:slug-bezier-fragment-specification))
