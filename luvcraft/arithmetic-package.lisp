(defpackage #:luvcraft.arithmetic
  (:use #:cl)
  (:local-nicknames (#:math #:luv.arithmetic)
                    (#:lang #:luv.arithmetic.language))
  (:export #:fog-amount-at-view-distance)
  (:documentation
   "Backend-neutral checked arithmetic laws for the luvcraft domain."))
