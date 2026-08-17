;;; Chains: sparse, weighted collections of sites.
;;;
;;; LUFT uses SITE for every kind of piece in the lattice: a point, an edge, a
;;; square face, or a cubic cell.  A CHAIN is simply a table that associates
;;; some of those sites with integers.  For example, a solid containing two
;;; cells starts out as the table
;;;
;;;     first cell   -> 1
;;;     second cell  -> 1
;;;
;;; One nonzero table entry is called a TERM.  Its integer is called its
;;; COEFFICIENT.  In an ordinary solid world, coefficient 1 means "this cell
;;; is present" and coefficient 0 means "it is absent."  Chains also allow -1,
;;; 2, and other integers because later operations need to express opposite
;;; directions, repeated contributions, and cancellation.  Storing a zero
;;; would be pointless, so adding contributions that total zero removes the
;;; term from the table.
;;;
;;; The number in "3-chain" says what kind of sites the table contains.  A
;;; chain of cubic cells is a 3-chain; a chain of square faces is a 2-chain; a
;;; chain of edges is a 1-chain; and a chain of vertices is a 0-chain.  The
;;; CHAIN structure itself is general and does not enforce that all its sites
;;; have the same dimension.
;;;
;;; The BOUNDARY operation turns every site into the pieces around its edge:
;;; a cell gives six faces, a face gives four edges, and an edge gives two
;;; vertices.  So the boundary of a solid 3-chain is a 2-chain containing its
;;; faces.  That output table is called its BOUNDARY CHAIN.  This is how
;;; SURFACE-CHAIN finds the faces to render.
;;;
;;; Boundary pieces carry coefficients +1 or -1.  The sign records their
;;; direction, or ORIENTATION.  LUFT chooses one consistent sign for every
;;; low and high boundary from the ordering X,Y,Z,T.  "Canonical incidence
;;; sign" is the mathematical name for that choice: "incidence" just means
;;; that a face belongs to the boundary of a cell, and "canonical" means the
;;; sign follows the one fixed rule rather than being chosen afresh.
;;;
;;; That sign rule makes neighbouring solid cells cancel their shared face.
;;; The high-X face of the first cell and the low-X face of the second cell are
;;; the same face SITE, but they contribute opposite coefficients:
;;;
;;;     shared face  -> (+1) + (-1) = 0
;;;
;;; The table therefore drops the shared face.  An outside face is contributed
;;; by only one cell, so it remains with coefficient +1 or -1.  Its sign tells
;;; the renderer which of the face's two directions points out of the solid.
;;;
;;; The same cancellation happens one dimension lower.  If we take the
;;; boundary of the resulting faces, every edge is reached twice with opposite
;;; signs.  Thus "the boundary of a boundary is zero" means the second result
;;; is an empty chain.  For surfaces produced from cells, this is the useful
;;; bookkeeping fact that the oriented faces leave no unpaired boundary edge.

(in-package #:luft)

;;; ------------------------------------------------------------------------
;;; Chains

(defstruct (chain (:constructor %make-chain (domain coefficients)))
  "A sparse table from DOMAIN's sites to nonzero integer weights.

Each table entry is called a term, and its integer weight is its coefficient.
A site whose coefficient becomes zero is removed from the table."
  (domain nil :type world-domain :read-only t)
  (coefficients (make-hash-table) :type hash-table :read-only t))

(defmethod print-object ((chain chain) stream)
  (print-unreadable-object (chain stream :type t :identity t)
    (format stream "~D term~:P" (chain-count chain))))

(defun make-chain (domain)
  "Make an empty chain for DOMAIN, with coefficient zero at every site."
  (check-type domain world-domain)
  (%make-chain domain (make-hash-table)))

(defun chain-count (chain)
  "Return the number of stored sites, or terms, in CHAIN.

Sites with coefficient zero are not stored and therefore are not counted."
  (hash-table-count (chain-coefficients chain)))

(defun chain-coefficient (chain site)
  "Return SITE's integer weight in CHAIN, or zero when SITE is absent."
  (values (gethash site (chain-coefficients chain) 0)))

(defun (setf chain-coefficient) (coefficient chain site)
  "Set SITE's integer weight in CHAIN; setting it to zero removes the term."
  (check-type coefficient integer)
  (checked-site (chain-domain chain) site)
  (if (zerop coefficient)
      (remhash site (chain-coefficients chain))
      (setf (gethash site (chain-coefficients chain)) coefficient))
  coefficient)

(defun add-chain-term (chain site coefficient)
  "Add COEFFICIENT to SITE's current weight and return the new weight.

For example, adding -1 to a site whose weight is 1 cancels it and removes the
site from CHAIN."
  (check-type coefficient integer)
  (setf (chain-coefficient chain site)
        (+ (chain-coefficient chain site) coefficient)))

(defun map-chain (function chain)
  "Call FUNCTION with each SITE and COEFFICIENT of CHAIN, in site order."
  (let ((sites (chain-sites chain)))
    (loop for site across sites
          do (funcall function site (chain-coefficient chain site)))
    chain))

(defun chain-sites (chain)
  "Return CHAIN's sites in ascending packed order as a fresh vector.

Packed order is Z-major, then Y, then X, then extent: sorting by the packed
word already groups terms by plane and row."
  (let ((sites (make-array (chain-count chain) :element-type 'site))
        (index 0))
    (maphash (lambda (site coefficient)
               (declare (ignore coefficient))
               (setf (aref sites index) site)
               (incf index))
             (chain-coefficients chain))
    (sort sites #'<)))

(defun chain-spatial-p (chain)
  "Whether no term of CHAIN extends along time."
  (loop for site being the hash-keys of (chain-coefficients chain)
        never (site-extends-p site :t)))

;;; ------------------------------------------------------------------------
;;; Boundaries

;;; For each input term, MAP-SITE-BOUNDARY supplies every one-dimension-lower
;;; boundary site and the +1 or -1 chosen by LUFT's orientation rule.  The
;;; expression (* SIGN COEFFICIENT) below has a plain meaning: if the input
;;; site has weight N, add N copies of each boundary piece, reversing the
;;; weight for pieces whose orientation sign is -1.  Contributions to the same
;;; boundary site are added together, which is where shared faces cancel.

(defun boundary-chain (chain)
  "Return CHAIN's boundary pieces, with direction, as a new chain.  #9HLYEE

A cell contributes its six faces, a face contributes its four edges, and an
edge contributes its two vertices.  Each contribution has the input site's
integer weight, made positive or negative according to LUFT's fixed
orientation rule.  Contributions to the same site are added, so the shared
face of two neighbouring cells cancels.

CHAIN must not contain sites that extend through time.  The two time ends of
such a site have the same packed spatial value and can only be distinguished
by an ambient time offset, which CHAIN has nowhere to store."
  (unless (chain-spatial-p chain)
    (error "The boundary of a temporal chain needs an ambient origin: ~S."
           chain))
  (let ((domain (chain-domain chain))
        (boundary (make-chain (chain-domain chain))))
    (maphash (lambda (site coefficient)
               (map-site-boundary
                (lambda (face sign temporal-offset axis side)
                  (declare (ignore temporal-offset axis side))
                  (add-chain-term boundary face (* sign coefficient)))
                domain site))
             (chain-coefficients chain))
    boundary))

;;; ------------------------------------------------------------------------
;;; Solid worlds

(defun make-solid-chain (domain)
  "Make an empty solid world for DOMAIN.

Adding cubic cell sites with coefficient 1 turns this into what mathematics
calls a 3-chain."
  (make-chain domain))

(defun solid-cell-p (chain x y z)
  "Whether the cell anchored at X,Y,Z is solid in CHAIN."
  (plusp (chain-coefficient
          chain (make-site (chain-domain chain) x y z +cell-extent+))))

(defun (setf solid-cell-p) (solid-p chain x y z)
  "Make the cell anchored at X,Y,Z solid or empty in CHAIN."
  (setf (chain-coefficient
         chain (make-site (chain-domain chain) x y z +cell-extent+))
        (if solid-p 1 0))
  solid-p)

(defun surface-chain (solid)
  "Return SOLID's exposed faces, each with coefficient +1 or -1.

SOLID normally contains cubic cells with coefficient 1.  Neighbouring cells'
shared faces cancel, leaving only outside faces.  Each remaining sign combines
with the face's fixed orientation to tell the renderer which way is outward."
  (boundary-chain solid))

;;; ------------------------------------------------------------------------
;;; Packed terms

;;; A packed term puts one site and its coefficient into one 64-bit word.  This
;;; compact form accepts only +1 and -1; "unit coefficient" means exactly one
;;; of those two integers, whose magnitude is one.  The site's own 60 bits are
;;; left unchanged, and bit 60 records whether the coefficient is negative.
;;; Sixty-one used bits keep the word an immediate fixnum on 64-bit SBCL.  A
;;; specialized (unsigned-byte 64) vector of these words is also exactly the
;;; buffer a shader reads.  A general chain may contain larger coefficients
;;; and cannot use this encoding, but a solid world's exposed faces never need
;;; larger ones.

(defconstant +term-sign-bit+ 60)
(defconstant +term-site-mask+ (1- (ash 1 +term-sign-bit+)))

(deftype packed-term ()
  '(unsigned-byte 61))

(defun pack-term (site coefficient)
  "Pack SITE and COEFFICIENT, which must be +1 or -1, into one word.  #AGVXGM"
  (check-type site site)
  (check-type coefficient (member 1 -1))
  (if (minusp coefficient)
      (logior site (ash 1 +term-sign-bit+))
      site))

(declaim (inline packed-term-site packed-term-coefficient))
(defun packed-term-site (term)
  (check-type term packed-term)
  (logand term +term-site-mask+))

(defun packed-term-coefficient (term)
  (check-type term packed-term)
  (if (logbitp +term-sign-bit+ term) -1 1))

(defun chain-packed-terms (chain)
  "Return CHAIN's terms as a fresh (unsigned-byte 64) vector in site order.

Every coefficient must be +1 or -1; see PACK-TERM."
  (let* ((sites (chain-sites chain))
         (terms (make-array (length sites) :element-type '(unsigned-byte 64))))
    (loop for site across sites
          for index from 0
          do (setf (aref terms index)
                   (pack-term site (chain-coefficient chain site))))
    terms))

;;; ------------------------------------------------------------------------
;;; Dense cell bits

;;; For a small world, a solid can also be represented as one bit per possible
;;; cell: 1 for solid and 0 for empty.  The bits run through X first, then Y,
;;; then Z, over the domain's horizontal periods and 256 vertical planes.  A
;;; shader uses this dense form when it needs to ask which cells surround a
;;; particular edge or vertex.  Mathematics calls that surrounding collection
;;; the site's "star"; the code only needs the direct occupancy lookup.

(defconstant +vertical-cell-rows+ 256)

(defun cell-bit-index (domain x y z)
  "The dense bit index of the cell anchored at X,Y,Z in DOMAIN."
  (+ (logand x (world-domain-x-mask domain))
     (* (world-domain-x-period domain)
        (+ (logand y (world-domain-y-mask domain))
           (* (world-domain-y-period domain) z)))))

(defun chain-cell-bit-count (domain)
  "The number of cell bits DOMAIN needs."
  (* (world-domain-x-period domain)
     (world-domain-y-period domain)
     +vertical-cell-rows+))

(defun chain-cell-bits (chain)
  "Return CHAIN's positive cell terms as a dense (unsigned-byte 32) bit vector.

Bit CELL-BIT-INDEX is set for every cell whose coefficient is positive; the
words are ordered so that word W holds bits 32W through 32W+31."
  (let* ((domain (chain-domain chain))
         (words (make-array (ceiling (chain-cell-bit-count domain) 32)
                            :element-type '(unsigned-byte 32)
                            :initial-element 0)))
    (maphash (lambda (site coefficient)
               (when (and (plusp coefficient)
                          (= (site-extent site) +cell-extent+))
                 (let ((index (cell-bit-index domain (site-x site)
                                              (site-y site) (site-z site))))
                   (setf (ldb (byte 1 (mod index 32))
                              (aref words (floor index 32)))
                         1))))
             (chain-coefficients chain))
    words))
