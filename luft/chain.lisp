;;; Chains: formal integer combinations of sites.
;;;
;;; A block world is not a set of voxels with six decorations each.  It is a
;;; 3-chain: every solid cell contributes coefficient one.  Its visible
;;; surface is then not a meshing problem but an algebraic fact: the boundary
;;; of that chain.  Two solid neighbours share one face site with opposite
;;; incidence signs, so the shared face cancels; only exposed faces survive,
;;; and each survives with a sign that says which side is solid.  Rendering a
;;; block world therefore means drawing a boundary chain, and the boundary of
;;; a boundary being zero is the guarantee that the surface is closed.

(in-package #:luft)

;;; ------------------------------------------------------------------------
;;; Chains

(defstruct (chain (:constructor %make-chain (domain coefficients)))
  "A finite formal sum of DOMAIN's sites with nonzero integer coefficients."
  (domain nil :type world-domain :read-only t)
  (coefficients (make-hash-table) :type hash-table :read-only t))

(defmethod print-object ((chain chain) stream)
  (print-unreadable-object (chain stream :type t :identity t)
    (format stream "~D term~:P" (chain-count chain))))

(defun make-chain (domain)
  "Make the zero chain over DOMAIN."
  (check-type domain world-domain)
  (%make-chain domain (make-hash-table)))

(defun chain-count (chain)
  "Return the number of sites with nonzero coefficient in CHAIN."
  (hash-table-count (chain-coefficients chain)))

(defun chain-coefficient (chain site)
  "Return the integer coefficient of SITE in CHAIN, zero when absent."
  (values (gethash site (chain-coefficients chain) 0)))

(defun (setf chain-coefficient) (coefficient chain site)
  "Set SITE's coefficient in CHAIN; zero removes the term."
  (check-type coefficient integer)
  (checked-site (chain-domain chain) site)
  (if (zerop coefficient)
      (remhash site (chain-coefficients chain))
      (setf (gethash site (chain-coefficients chain)) coefficient))
  coefficient)

(defun add-chain-term (chain site coefficient)
  "Add COEFFICIENT times SITE into CHAIN and return the resulting coefficient."
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

(defun boundary-chain (chain)
  "Return the oriented boundary of the spatial CHAIN as a new chain.  #9HLYEE

Each term contributes its coefficient times the canonical incidence sign to
every codimension-one boundary site.  Temporal terms are refused: their two
temporal boundaries share one packed site and differ only by ambient
offset, which a packed chain cannot hold."
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
  "Make an empty solid world: a 3-chain over DOMAIN awaiting cells."
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
  "Return the exposed faces of the solid 3-chain SOLID with unit signs.

A face's sign is positive when the solid cell lies on its low side along
the missing axis and negative when it lies on the high side, so the sign
times the face's canonical orientation is its outward normal."
  (boundary-chain solid))

;;; ------------------------------------------------------------------------
;;; Packed terms

;;; A packed term is one 64-bit word carrying a site and a unit coefficient:
;;; the site in its own 60 bits and the sign in bit 60.  Sixty-one bits keep
;;; the word an immediate fixnum on 64-bit SBCL, and a specialized
;;; (unsigned-byte 64) vector of packed terms is exactly the buffer a shader
;;; reads.  Chains with larger coefficients need a wider encoding; the surface
;;; of a solid world never does.

(defconstant +term-sign-bit+ 60)
(defconstant +term-site-mask+ (1- (ash 1 +term-sign-bit+)))

(deftype packed-term ()
  '(unsigned-byte 61))

(defun pack-term (site coefficient)
  "Pack SITE with unit COEFFICIENT, either 1 or -1, into one word.  #AGVXGM"
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

Every coefficient must be a unit; see PACK-TERM."
  (let* ((sites (chain-sites chain))
         (terms (make-array (length sites) :element-type '(unsigned-byte 64))))
    (loop for site across sites
          for index from 0
          do (setf (aref terms index)
                   (pack-term site (chain-coefficient chain site))))
    terms))
