(defpackage #:luvcraft.world.fields
  (:use #:cl)
  (:local-nicknames (#:math #:luv.arithmetic))
  (:export #:voxel-field-definition
           #:voxel-field-definition-name
           #:voxel-field-definition-site-kind
           #:voxel-field-definition-missing-value-semantics
           #:voxel-field-definition-legal-value-type
           #:voxel-field-definition-representation-policy
           #:voxel-field-definition-revision
           #:field-definition-for
           #:define-voxel-field
           #:field-representation-domain
           #:materialized-field-definition
           #:materialized-field-representation
           #:materialized-field-current-p))
