(defpackage #:luvcraft.frontier
  (:use #:cl #:luvcraft.world)
  (:local-nicknames (#:lang #:luv.arithmetic.language)
                    (#:lisp #:luv.arithmetic.lisp)
                    (#:math #:luv.arithmetic)
                    (#:records #:luv.arithmetic.records))
  (:documentation
   "Inspectable frontier programs, their packed execution substrates, and the
compiler which closes a program over bound fields into a scalar loop.")
  (:export #:admit-frontier-realization-site
           #:admit-frontier-site
           #:as-field-quantity
           #:compile-frontier-program
           #:drain-frontier-realization
           #:frontier-execution-admitted-sites
           #:frontier-execution-emissions
           #:frontier-field-role-invalidated-p
           #:schedule-frontier-realization-site
           #:frontier-field-binding
           #:frontier-field-binding-declaration
           #:frontier-field-binding-name
           #:frontier-field-role
           #:frontier-field-role-name
           #:frontier-field-role-relaxed-p
           #:frontier-field-role-memo-p
           #:frontier-program-definition-fields
           #:frontier-program-definition-constants
           #:frontier-program-definition-predicates
           #:frontier-program-definition-transfer
           #:frontier-program-definition-admission
           #:frontier-program-definition-priority
           #:frontier-program-definition-retain-admissions-p
           #:frontier-realization
           #:frontier-realization-admission
           #:frontier-realization-admit-form
           #:frontier-realization-admit-function
           #:frontier-realization-bindings
           #:frontier-realization-current-p
           #:frontier-realization-definition
           #:frontier-realization-drain-form
           #:frontier-realization-drain-function
           #:frontier-realization-maximum-priority
           #:frontier-realization-priority
           #:frontier-realization-priority-meaning
           #:frontier-realization-relate-form
           #:frontier-realization-relate-function
           #:relate-frontier-realization-site
           #:frontier-realization-transfer
           #:frontier-site-buffer
           #:frontier-site-buffer-length
           #:frontier-site-buffer-materialization-lane
           #:frontier-site-buffer-offset-lane
           #:make-frontier-field-binding
           #:make-realization-execution
           #:make-realization-frontier
           #:note-frontier-program-redefinition
           #:bucket-frontier
           #:bucket-frontier-count
           #:bucket-frontier-current-priority
           #:bucket-frontier-empty-p
           #:bucket-frontier-maximum-priority
           #:bucket-frontier-peak-count
           #:bucket-frontier-pop
           #:bucket-frontier-pops
           #:bucket-frontier-priority-meaning
           #:bucket-frontier-push
           #:bucket-frontier-pushes
           #:define-frontier-program
           #:do-voxel-frontier-relations
           #:frontier-execution
           #:frontier-execution-admissions
           #:frontier-execution-crossings
           #:frontier-execution-frontier
           #:frontier-execution-input
           #:frontier-execution-program
           #:frontier-execution-relations
           #:frontier-execution-unavailable
           #:frontier-execution-visits
           #:frontier-program-definition
           #:frontier-program-definition-family
           #:frontier-program-definition-for
           #:frontier-program-definition-frontier-layout
           #:frontier-program-definition-name
           #:frontier-program-definition-neighborhood
           #:frontier-program-definition-revision
           #:frontier-program-definition-source-form
           #:make-bucket-frontier
           #:make-frontier-execution))
