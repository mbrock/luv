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
           #:lisp-binary-operation
           #:lisp-dot
           #:lisp-normalize
           #:lisp-arithmetic-operator-function
           #:*lisp-arithmetic-lowering*
           #:lisp-scalar-operator-form
           #:define-lisp-scalar-operator
           #:lower-lisp-arithmetic-expression
           #:lower-arithmetic-function
           #:compile-arithmetic-function
           #:define-lisp-arithmetic-function
           #:lisp-arithmetic-realization
           #:lisp-arithmetic-realization-definition
           #:lisp-arithmetic-realization-parameter-declarations
           #:lisp-arithmetic-realization-result-declaration
           #:lisp-arithmetic-realization-function
           #:make-lisp-arithmetic-realization
           #:bind-lisp-arithmetic-realization))
