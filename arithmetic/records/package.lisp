(defpackage #:luv.arithmetic.records
  (:use #:cl)
  (:local-nicknames (#:domains #:luv.domains)
                    (#:math #:luv.arithmetic))
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
           #:record-slot-name
           #:columnar-declaration-error
           #:columnar-declaration-error-definition
           #:columnar-declaration-error-lane-name
           #:columnar-declaration-error-declaration
           #:columnar-declaration-error-reason
           #:columnar-lane-definition
           #:columnar-lane-definition-name
           #:columnar-lane-definition-initial-element
           #:columnar-lane-definition-clear-on-remove-p
           #:columnar-layout-definition
           #:columnar-layout-definition-name
           #:columnar-layout-definition-lanes
           #:columnar-layout-definition-quantity-layout
           #:columnar-layout-definition-source-form
           #:columnar-layout-definition-for
           #:columnar-layout-lane-definition
           #:columnar-row-declaration
           #:columnar-row-declaration-layout-definition
           #:columnar-row-declaration-lane-declarations
           #:columnar-row-declaration-quantity-layout
           #:columnar-row-declaration-revision
           #:columnar-row-lane-declaration
           #:make-columnar-row-declaration
           #:define-columnar-buffer
           #:define-columnar-materialization
           #:do-columnar-buffer-rows
           #:with-columnar-buffer-storage
           #:with-columnar-materialization-storage
           #:with-columnar-buffer-row))
