(defpackage #:luv.arithmetic.lisp
  (:use #:cl)
  (:local-nicknames (#:lang #:luv.arithmetic.language)
                    (#:math #:luv.arithmetic))
  (:import-from #:luv.arithmetic
                #:dot #:clamp #:mix #:smoothstep #:normalize)
  (:shadowing-import-from #:luv.arithmetic #:step)
  (:export #:lisp-arithmetic-error
           #:lisp-arithmetic-error-definition
           #:lisp-arithmetic-error-expression
           #:lisp-arithmetic-error-reason
           #:lisp-arithmetic-error-details
           #:lisp-arithmetic-operator-function
           #:lower-lisp-arithmetic-expression
           #:lower-arithmetic-function
           #:compile-arithmetic-function
           #:define-lisp-arithmetic-function))
