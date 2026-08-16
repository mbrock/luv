(defpackage #:luv.arithmetic.records
  (:use #:cl)
  (:local-nicknames (#:math #:luv.arithmetic))
  (:export #:quantity-class
           #:quantity-slot-conflict
           #:quantity-slot-conflict-class
           #:quantity-slot-conflict-slot-name
           #:quantity-slot-conflict-declarations
           #:define-quantity-struct
           #:structure-declaration
           #:structure-declaration-for
           #:record-slot-declarations
           #:record-slot-declaration
           #:record-slot-name))
