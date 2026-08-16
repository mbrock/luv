;;; Small protocols shared by finite-domain materializations.

(in-package #:luv.domains)

(defgeneric domain-cardinality (domain)
  (:documentation
   "Return the exact number of sites in finite DOMAIN.

This deliberately says nothing about coordinate or offset representation.
Those mappings belong to concrete domain protocols and are added only when a
client needs to traverse them."))
