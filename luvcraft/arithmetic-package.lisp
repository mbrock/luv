(defpackage #:luvcraft.arithmetic
  (:use #:cl)
  (:local-nicknames (#:math #:luv.arithmetic)
                    (#:lang #:luv.arithmetic.language)
                    (#:lisp #:luv.arithmetic.lisp))
  (:export #:fog-amount-at-view-distance
           #:light-propagation-loss)
  (:documentation
   "Backend-neutral checked arithmetic laws for the luvcraft domain."))
