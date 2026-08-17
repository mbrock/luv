;;; Chains: collections of signed sites that annihilate in opposite pairs.
;;;
;;; LUFT uses SITE for every kind of piece in the lattice: a point, an edge, a
;;; square face, or a cubic cell.  Every site carries its own positive or
;;; negative polarity.  A CHAIN collects those signed sites.  For example, a
;;; solid containing two cells starts as
;;;
;;;     +first-cell
;;;     +second-cell
;;;
;;; Adding the opposite version of a site annihilates one existing copy:
;;;
;;;     +face  plus  -face  -> nothing
;;;
;;; Internally, CHAIN keeps two hash tables like credit and debit ledgers.  The
;;; keys contain only geometric identity; the choice of table supplies the
;;; polarity bit.  A small occurrence count in each table makes addition
;;; independent of arrival order.  The ordinary solid and its drawable surface
;;; use at most one copy of any signed site.
;;;
;;; The number in "3-chain" says what kind of sites the collection contains.  A
;;; chain of cubic cells is a 3-chain; a chain of square faces is a 2-chain; a
;;; chain of edges is a 1-chain; and a chain of vertices is a 0-chain.  The
;;; CHAIN structure itself is general and does not enforce one dimension.
;;;
;;; The BOUNDARY operation turns every site into the pieces around its edge: a
;;; cell gives six faces, a face gives four edges, and an edge gives two
;;; vertices.  The boundary of a solid 3-chain is therefore a 2-chain of faces.
;;; This is how SURFACE-CHAIN finds the faces to render.
;;;
;;; Boundary pieces carry their direction, or ORIENTATION, in their polarity
;;; bit.  LUFT chooses one consistent polarity for every low and high boundary
;;; from the ordering X,Y,Z.  The mathematical phrase "canonical incidence
;;; sign" merely names that fixed choice.
;;;
;;; That choice makes neighbouring solid cells annihilate their shared face.
;;; The high-X face of the first cell and the low-X face of the second cell have
;;; the same geometry but opposite polarities:
;;;
;;;     +shared-face  plus  -shared-face  -> nothing
;;;
;;; An outside face arrives from only one cell, so it remains positive or
;;; negative.  Its polarity tells the renderer which of the face's two
;;; directions points out of the solid.
;;;
;;; The same annihilation happens one dimension lower.  Taking the boundary of
;;; the resulting faces sends every edge once to each polarity ledger, leaving
;;; both empty.  Thus "the boundary of a boundary is zero" means the second
;;; result contains no signed sites.

(in-package #:luft)

;;; ------------------------------------------------------------------------
;;; Chains

(defstruct (chain (:constructor %make-chain
                      (domain positive-counts negative-counts)))
  "A normalized collection of positive and negative sites in DOMAIN.

The two hash tables are polarity ledgers keyed by SITE-GEOMETRY.  Opposite
occurrences annihilate immediately, so one geometry appears in at most one
ledger.  Counts preserve repeated unmatched occurrences without making an
integer coefficient part of a site's public meaning."
  (domain nil :type world-domain :read-only t)
  (positive-counts (make-hash-table) :type hash-table :read-only t)
  (negative-counts (make-hash-table) :type hash-table :read-only t))

(defmethod print-object ((chain chain) stream)
  (print-unreadable-object (chain stream :type t :identity t)
    (format stream "~D signed site~:P" (chain-count chain))))

(defun make-chain (domain)
  "Make an empty collection of signed sites in DOMAIN."
  (check-type domain world-domain)
  (%make-chain domain (make-hash-table) (make-hash-table)))

(defun chain-polarity-table (chain site)
  (if (site-negative-p site)
      (chain-negative-counts chain)
      (chain-positive-counts chain)))

(defun chain-opposite-polarity-table (chain site)
  (if (site-negative-p site)
      (chain-positive-counts chain)
      (chain-negative-counts chain)))

(defun chain-count (chain)
  "Return the number of unmatched signed-site occurrences in CHAIN."
  (+ (loop for count being the hash-values of (chain-positive-counts chain)
           sum count)
     (loop for count being the hash-values of (chain-negative-counts chain)
           sum count)))

(defun chain-site-count (chain site)
  "Return how many unmatched copies of signed SITE occur in CHAIN."
  (checked-site (chain-domain chain) site)
  (gethash (site-geometry site) (chain-polarity-table chain site) 0))

(defun chain-site-p (chain site)
  "Whether at least one unmatched copy of signed SITE occurs in CHAIN."
  (plusp (chain-site-count chain site)))

(defun decrement-site-count (table geometry)
  (let ((count (gethash geometry table 0)))
    (cond ((= count 1) (remhash geometry table))
          ((> count 1) (setf (gethash geometry table) (1- count)))
          (t (error "No signed site ~S to remove." geometry)))))

(defun add-chain-site (chain site)
  "Add signed SITE to CHAIN, annihilating one opposite copy when present.

The polarity bit is masked from the hash key and chooses one of CHAIN's two
ledgers.  Return SITE."
  (checked-site (chain-domain chain) site)
  (let* ((geometry (site-geometry site))
         (same (chain-polarity-table chain site))
         (opposite (chain-opposite-polarity-table chain site)))
    (if (plusp (gethash geometry opposite 0))
        (decrement-site-count opposite geometry)
        (incf (gethash geometry same 0))))
  site)

(defun map-chain-unordered (function chain)
  "Call FUNCTION with each signed site occurrence without sorting or allocating."
  (flet ((map-ledger (table polarity)
           (maphash (lambda (geometry count)
                      (loop repeat count
                            do (funcall function
                                        (site-with-polarity geometry polarity))))
                    table)))
    (map-ledger (chain-positive-counts chain) 1)
    (map-ledger (chain-negative-counts chain) -1))
  chain)

(defun map-chain (function chain)
  "Call FUNCTION with each signed site occurrence in CHAIN, in packed order."
  (loop for site across (chain-sites chain)
        do (funcall function site))
  chain)

(defun chain-sites (chain)
  "Return CHAIN's signed sites in ascending packed order as a fresh vector.  #AGVXGM

Packed order is Z-major, then Y, then X, then extent and polarity.  Repeated
unmatched occurrences appear repeatedly in the vector."
  (let ((sites (make-array (chain-count chain) :element-type 'site))
        (index 0))
    (flet ((copy-ledger (table polarity)
             (maphash (lambda (geometry count)
                        (loop repeat count
                              do
                          (setf (aref sites index)
                                (site-with-polarity geometry polarity))
                          (incf index)))
                      table)))
      (copy-ledger (chain-positive-counts chain) 1)
      (copy-ledger (chain-negative-counts chain) -1))
    (sort sites #'<)))

;;; ------------------------------------------------------------------------
;;; Boundaries

;;; MAP-SITE-BOUNDARY returns signed boundary sites directly.  BOUNDARY-CHAIN
;;; adds each one to the appropriate polarity ledger.  When the opposite site
;;; is already there, the pair annihilates.  This is where shared faces vanish.

(defun boundary-chain (chain)
  "Return CHAIN's signed boundary pieces as a new chain.  #9HLYEE

A cell contributes its six signed faces, a face contributes its four signed
edges, and an edge contributes its two signed vertices.  Opposite copies of
one geometry annihilate, so the shared face of two neighbouring cells
disappears."
  (let ((domain (chain-domain chain))
        (boundary (make-chain (chain-domain chain))))
    (map-chain-unordered
     (lambda (site)
       (map-site-boundary
        (lambda (part axis side)
          (declare (ignore axis side))
          (add-chain-site boundary part))
        domain site))
     chain)
    boundary))

;;; ------------------------------------------------------------------------
;;; Solid worlds

(defun make-solid-chain (domain)
  "Make an empty solid world for DOMAIN.

Adding positive cubic cell sites turns this into what mathematics calls a
3-chain."
  (make-chain domain))

(defun solid-cell-p (chain x y z)
  "Whether the cell anchored at X,Y,Z is solid in CHAIN."
  (chain-site-p
   chain (make-site (chain-domain chain) x y z +cell-extent+)))

(defun (setf solid-cell-p) (solid-p chain x y z)
  "Make the cell anchored at X,Y,Z solid or empty in CHAIN."
  (let* ((site (make-site (chain-domain chain) x y z +cell-extent+))
         (geometry (site-geometry site)))
    (remhash geometry (chain-negative-counts chain))
    (if solid-p
        (setf (gethash geometry (chain-positive-counts chain)) 1)
        (remhash geometry (chain-positive-counts chain))))
  solid-p)

(defun surface-chain (solid)
  "Return SOLID's exposed faces, each with positive or negative polarity.

SOLID normally contains one positive site for each occupied cubic cell.
Neighbouring cells' shared faces annihilate, leaving only outside faces.  Each
remaining face's polarity tells the renderer which way is outward."
  (boundary-chain solid))

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
  "Return CHAIN's positive cells as a dense (unsigned-byte 32) bit vector.

Bit CELL-BIT-INDEX is set for every positive cell site; the words are ordered
so that word W holds bits 32W through 32W+31."
  (let* ((domain (chain-domain chain))
         (words (make-array (ceiling (chain-cell-bit-count domain) 32)
                            :element-type '(unsigned-byte 32)
                            :initial-element 0)))
    (maphash (lambda (site count)
               (declare (ignore count))
               (when (= (site-extent site) +cell-extent+)
                 (let ((index (cell-bit-index domain (site-x site)
                                              (site-y site) (site-z site))))
                   (setf (ldb (byte 1 (mod index 32))
                              (aref words (floor index 32)))
                         1))))
             (chain-positive-counts chain))
    words))
