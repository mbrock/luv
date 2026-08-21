;;;; luft-foundation.lisp
;;;;
;;;; The executable foundation for LUFT, rebuilt as a single coherent file.
;;;;
;;;; This file is an executable specification.  It defines:
;;;;
;;;;   1. the packed cubical-site and world-domain model;
;;;;   2. canonical incidence, boundary, and coface operations;
;;;;   3. a compact, normalized chain representation (no hash-table ledgers);
;;;;   4. the strict-minority moment chamfer classifier;
;;;;   5. one packed 32-bit shape word per oriented boundary face;
;;;;   6. a CPU reference realization of the sixteen points of a face patch;
;;;;   7. canonical positive- and negative-winding index templates;
;;;;   8. executable tests of the mathematical and representational invariants.
;;;;
;;;; It deliberately does not implement Metal, Vulkan, or a renderer command
;;;; protocol.  It does define the exact face-record and topology ABI those
;;;; backends will consume.
;;;;
;;;; Run (luft:run-tests) after loading.

(defpackage #:luft
  (:use #:common-lisp)
  (:export
   ;; sites
   #:site #:extent-mask #:axis #:side
   #:+vertex-extent+ #:+x-edge-extent+ #:+y-edge-extent+ #:+z-edge-extent+
   #:+xy-face-extent+ #:+xz-face-extent+ #:+yz-face-extent+ #:+cell-extent+
   #:make-extent #:make-site #:site-valid-p #:checked-site
   #:site-extent #:site-x #:site-y #:site-z #:site-anchor
   #:site-negative-p #:site-positive-p #:site-polarity
   #:site-geometry #:opposite-site #:site-with-polarity
   #:site-extends-p #:site-dimension
   ;; domains
   #:world-domain #:make-world-domain
   #:world-domain-x-bits #:world-domain-y-bits
   #:world-domain-x-mask #:world-domain-y-mask
   #:world-domain-x-period #:world-domain-y-period
   ;; adjacency and incidence
   #:step-site #:site-forward #:site-backward
   #:site-boundary-low #:site-boundary-high #:site-boundary-polarity
   #:map-site-boundary #:site-coface-forward #:site-coface-backward
   ;; chains
   #:chain #:chain-domain #:chain-count #:chain-empty-p
   #:chain-site-count #:chain-site-p #:chain-equal
   #:map-chain #:chain-sites #:chain-add #:boundary-chain
   #:make-chain-builder #:chain-builder-add #:chain-builder-finish
   ;; solids
   #:solid-world #:make-solid-world #:solid-world-domain
   #:solid-cell-p #:solid-world-occupancy
   #:solid-chain #:surface-chain
   ;; classification
   #:map-site-star #:site-minority-moment #:site-shape
   #:site-displacement
   ;; shape words
   #:+edge-balanced+ #:+edge-convex+ #:+edge-concave+
   #:pack-shape-word #:unpack-shape-word #:face-shape-word
   ;; face realization
   #:face-basis #:face-outward-sign
   #:realize-face-patch #:reference-face-patch
   ;; face records
   #:+face-record-words+ #:decorated-site
   #:decorated-site-site #:decorated-site-stock
   #:pack-face-record #:unpack-face-record #:materialize-surface
   ;; raster topology
   #:local-point-index #:+positive-face-indices+ #:+negative-face-indices+
   ;; tests
   #:run-tests))

(in-package #:luft)

;;; ========================================================================
;;; 1. Sites
;;;
;;; A lattice point, an edge, a square face, and a cubic cell are not four
;;; unrelated coordinate records; they are the dimensions of one cubical
;;; lattice, related by canonical incidence.  A SITE packs:
;;;
;;;   - a lattice-point anchor (x,y,z);
;;;   - a three-bit extent mask saying which unit intervals extend forward
;;;     from the anchor;
;;;   - one polarity bit giving the site's orientation.
;;;
;;; Dimension is the population count of the extent mask.  The high-X
;;; boundary of the cell at (x,y,z) is *the same* YZ face as the low-X
;;; boundary of the cell at (x+1,y,z); the two incidences differ only in
;;; polarity.  Clearing the polarity bit gives SITE-GEOMETRY, the common
;;; identity; toggling it gives OPPOSITE-SITE.  Fixnum equality within one
;;; world domain is therefore geometric equality plus orientation.
;;;
;;; Layout, low to high (60 bits total; two fixnum payload bits spare on
;;; 64-bit SBCL):
;;;
;;;    3 bits  extent mask, XYZ from bit 0
;;;    1 bit   polarity (clear positive, set negative)
;;;   24 bits  X anchor
;;;   24 bits  Y anchor
;;;    8 bits  Z anchor
;;;
;;; X and Y wrap under independently chosen power-of-two domain masks.  Z
;;; does not wrap; a Z-extended site may not begin on the top lattice plane.

(deftype site () '(unsigned-byte 60))
(deftype extent-mask () '(unsigned-byte 3))
(deftype axis () '(member :x :y :z))
(deftype side () '(member :low :high))

(defconstant +vertex-extent+ #b000)
(defconstant +x-edge-extent+ #b001)
(defconstant +y-edge-extent+ #b010)
(defconstant +z-edge-extent+ #b100)
(defconstant +xy-face-extent+ #b011)
(defconstant +xz-face-extent+ #b101)
(defconstant +yz-face-extent+ #b110)
(defconstant +cell-extent+ #b111)

(defconstant +extent-bits+ 3)
(defconstant +site-sign-bit+ 3)
(defconstant +negative-site-mask+ (ash 1 +site-sign-bit+))
(defconstant +site-tag-bits+ 4)
(defconstant +horizontal-capacity-bits+ 24)
(defconstant +vertical-coordinate-bits+ 8)
(defconstant +x-shift+ +site-tag-bits+)
(defconstant +y-shift+ (+ +x-shift+ +horizontal-capacity-bits+))
(defconstant +z-shift+ (+ +y-shift+ +horizontal-capacity-bits+))
(defconstant +site-bits+ (+ +z-shift+ +vertical-coordinate-bits+))
(defconstant +top-z+ (1- (ash 1 +vertical-coordinate-bits+)))

;;; ------------------------------------------------------------------------
;;; World domains
;;;
;;; The packed fields describe representational capacity; a WORLD-DOMAIN
;;; supplies the actual topology.  X and Y wrap with independent power-of-two
;;; periods, so a horizontal slice is a possibly rectangular torus.  Sites
;;; have canonical fixnum equality only inside one such domain.

(defstruct (world-domain
             (:constructor %make-world-domain (x-bits y-bits x-mask y-mask)))
  (x-bits 24 :type (integer 1 24) :read-only t)
  (y-bits 24 :type (integer 1 24) :read-only t)
  (x-mask #xffffff :type (unsigned-byte 24) :read-only t)
  (y-mask #xffffff :type (unsigned-byte 24) :read-only t))

(defun make-world-domain (&key (horizontal-bits 24)
                               (x-bits horizontal-bits)
                               (y-bits horizontal-bits))
  "Make a toroidal world domain with power-of-two X and Y periods."
  (check-type x-bits (integer 1 #.+horizontal-capacity-bits+))
  (check-type y-bits (integer 1 #.+horizontal-capacity-bits+))
  (%make-world-domain x-bits y-bits
                      (1- (ash 1 x-bits))
                      (1- (ash 1 y-bits))))

(declaim (inline world-domain-x-period world-domain-y-period))
(defun world-domain-x-period (domain)
  (1+ (world-domain-x-mask domain)))
(defun world-domain-y-period (domain)
  (1+ (world-domain-y-mask domain)))

;;; ------------------------------------------------------------------------
;;; Site construction and accessors

(declaim (inline axis-bit axis-index index-axis))
(defun axis-bit (axis)
  (ecase axis (:x #b001) (:y #b010) (:z #b100)))

(defun axis-index (axis)
  (ecase axis (:x 0) (:y 1) (:z 2)))

(defun index-axis (index)
  (ecase index (0 :x) (1 :y) (2 :z)))

(defun make-extent (&rest axes)
  "Return the extent mask containing AXES."
  (reduce #'logior axes :key #'axis-bit :initial-value 0))

(declaim (inline site-extent site-x site-y site-z
                 site-negative-p site-positive-p site-polarity
                 site-geometry opposite-site site-with-polarity
                 site-dimension))

(defun site-extent (site)
  (ldb (byte +extent-bits+ 0) site))

(defun site-x (site)
  (ldb (byte +horizontal-capacity-bits+ +x-shift+) site))

(defun site-y (site)
  (ldb (byte +horizontal-capacity-bits+ +y-shift+) site))

(defun site-z (site)
  (ldb (byte +vertical-coordinate-bits+ +z-shift+) site))

(defun site-anchor (site)
  "Return SITE's X, Y, and Z lattice coordinates as three values."
  (values (site-x site) (site-y site) (site-z site)))

(defun site-negative-p (site)
  (logbitp +site-sign-bit+ site))

(defun site-positive-p (site)
  (not (site-negative-p site)))

(defun site-polarity (site)
  "Return SITE's polarity, either +1 or -1."
  (if (site-negative-p site) -1 1))

(defun site-geometry (site)
  "Return SITE's anchor and extent with positive polarity: the common
geometric identity shared by both orientations of the site."
  (logandc2 site +negative-site-mask+))

(defun opposite-site (site)
  "Return the same geometric site with the opposite polarity."
  (logxor site +negative-site-mask+))

(defun site-with-polarity (site polarity)
  "Return SITE's geometry with POLARITY, either +1 or -1."
  (if (minusp polarity)
      (logior (site-geometry site) +negative-site-mask+)
      (site-geometry site)))

(defun site-extends-p (site axis)
  "Whether SITE spans the unit interval along AXIS."
  (logtest (axis-bit axis) (site-extent site)))

(defun site-dimension (site)
  "Return SITE's dimension: zero for a vertex through three for a cell."
  (logcount (site-extent site)))

(defun site-valid-p (domain thing)
  "Whether THING is a canonical packed site in DOMAIN.

Inactive upper X and Y bits make an otherwise representable value
noncanonical for this domain; a Z-extended site cannot be anchored on the
top plane."
  (and (typep thing 'site)
       (let ((x (site-x thing))
             (y (site-y thing)))
         (and (= x (logand x (world-domain-x-mask domain)))
              (= y (logand y (world-domain-y-mask domain)))
              (not (and (= (site-z thing) +top-z+)
                        (logbitp 2 (site-extent thing))))))))

(defun make-site (domain x y z &optional (extent +vertex-extent+) (polarity 1))
  "Make DOMAIN's canonical site at X,Y,Z with EXTENT and POLARITY.

X and Y are reduced with DOMAIN's independent power-of-two masks.  Z must
name one of the 256 non-wrapping lattice planes; a Z-extended site cannot
begin on the top plane."
  (check-type x integer)
  (check-type y integer)
  (check-type z (integer 0 #.+top-z+))
  (check-type extent extent-mask)
  (check-type polarity (member 1 -1))
  (when (and (= z +top-z+) (logbitp 2 extent))
    (error "A Z-extended site cannot be anchored on top plane ~D." +top-z+))
  (logior extent
          (if (minusp polarity) +negative-site-mask+ 0)
          (ash (logand x (world-domain-x-mask domain)) +x-shift+)
          (ash (logand y (world-domain-y-mask domain)) +y-shift+)
          (ash z +z-shift+)))

(defun checked-site (domain site)
  (unless (site-valid-p domain site)
    (error "~S is not a canonical LUFT site in ~S." site domain))
  site)

(defun site-with-extent (domain site extent)
  (make-site domain (site-x site) (site-y site) (site-z site)
             extent (site-polarity site)))

;;; ------------------------------------------------------------------------
;;; Adjacency

(defun step-site (domain site axis delta)
  "Translate DOMAIN's SITE along AXIS by DELTA, keeping its polarity.

A step outside the non-wrapping vertical world returns NIL."
  (checked-site domain site)
  (check-type delta integer)
  (ecase axis
    (:x (make-site domain (+ (site-x site) delta) (site-y site) (site-z site)
                   (site-extent site) (site-polarity site)))
    (:y (make-site domain (site-x site) (+ (site-y site) delta) (site-z site)
                   (site-extent site) (site-polarity site)))
    (:z (let ((z (+ (site-z site) delta)))
          (if (or (< z 0) (> z +top-z+)
                  (and (= z +top-z+) (logbitp 2 (site-extent site))))
              nil
              (make-site domain (site-x site) (site-y site) z
                         (site-extent site) (site-polarity site)))))))

(defun site-forward (domain site axis)
  (step-site domain site axis 1))

(defun site-backward (domain site axis)
  (step-site domain site axis -1))

;;; ========================================================================
;;; 2. Incidence
;;;
;;; Axes are ordered X, Y, Z.  Removing one present extent bit produces a
;;; low and a high boundary site.  Boundary polarity is the canonical
;;; alternating incidence sign induced by that axis order, multiplied by
;;; the parent's polarity.  With that fixed choice:
;;;
;;;   - a cell yields six oriented faces, a face four oriented edges, an
;;;     edge two oriented vertices;
;;;   - neighbouring positive cells contribute opposite orientations of
;;;     their shared face;
;;;   - the boundary of a boundary normalizes to zero.
;;;
;;; These are the core topology, not renderer conveniences.

(defun require-site-extent (site axis present-p)
  (let ((present (site-extends-p site axis)))
    (unless (eq present present-p)
      (error "Site ~S ~:[already extends~;does not extend~] along ~S."
             site present-p axis))))

(defun site-boundary-polarity (site axis side)
  "Return the polarity of SITE's low or high AXIS boundary.

The fixed orientation alternates over the present axes in X,Y,Z order; the
site's own polarity reverses the whole result."
  (require-site-extent site axis t)
  (check-type side side)
  (let* ((bit (axis-bit axis))
         (earlier (logand (site-extent site) (1- bit)))
         (high-sign (if (evenp (logcount earlier)) 1 -1)))
    (* (site-polarity site)
       (if (eq side :high) high-sign (- high-sign)))))

(defun site-boundary-low (domain site axis)
  "Return DOMAIN's oriented low AXIS boundary of SITE."
  (checked-site domain site)
  (require-site-extent site axis t)
  (site-with-polarity
   (site-with-extent domain site (logandc2 (site-extent site) (axis-bit axis)))
   (site-boundary-polarity site axis :low)))

(defun site-boundary-high (domain site axis)
  "Return DOMAIN's oriented high AXIS boundary of SITE."
  (checked-site domain site)
  (require-site-extent site axis t)
  (site-with-polarity
   (site-forward domain
                 (site-with-extent domain site
                                   (logandc2 (site-extent site)
                                             (axis-bit axis)))
                 axis)
   (site-boundary-polarity site axis :high)))

(defun map-site-boundary (function domain site)
  "Call FUNCTION with each oriented boundary piece of DOMAIN's SITE.

FUNCTION receives the signed boundary site, the AXIS, and the SIDE."
  (checked-site domain site)
  (dolist (axis '(:x :y :z) site)
    (when (site-extends-p site axis)
      (funcall function (site-boundary-low domain site axis) axis :low)
      (funcall function (site-boundary-high domain site axis) axis :high))))

;;; Cofaces invert the local incidence: given a site and an absent extent
;;; axis, construct the forward or backward coface with whichever polarity
;;; makes the original site its exact signed boundary.

(defun site-coface-forward (domain site axis)
  "Return DOMAIN's coface extending forward from SITE along AXIS, or NIL
when it would extend above the vertical world.  Its low AXIS boundary is
exactly the signed SITE."
  (checked-site domain site)
  (require-site-extent site axis nil)
  (if (and (eq axis :z) (= (site-z site) +top-z+))
      nil
      (let ((coface (site-with-extent
                     domain site
                     (logior (site-extent site) (axis-bit axis)))))
        (site-with-polarity
         coface
         (* (site-polarity site)
            (site-boundary-polarity (site-with-polarity coface 1)
                                    axis :low))))))

(defun site-coface-backward (domain site axis)
  "Return DOMAIN's coface whose high AXIS boundary is exactly the signed
SITE, or NIL when it would extend below the vertical world."
  (checked-site domain site)
  (require-site-extent site axis nil)
  (let ((anchor (site-backward domain site axis)))
    (when anchor
      (let ((coface (site-with-extent
                     domain anchor
                     (logior (site-extent anchor) (axis-bit axis)))))
        (site-with-polarity
         coface
         (* (site-polarity site)
            (site-boundary-polarity (site-with-polarity coface 1)
                                    axis :high)))))))

;;; ========================================================================
;;; 3. Chains
;;;
;;; A chain is a finite collection of signed sites: algebraically a sparse
;;; integer-valued function on geometric sites, although its public elements
;;; remain oriented sites.  A packed site is compact, comparable, sortable,
;;; and streamable; surrounding each one with hash-table structure would
;;; discard that.  So a CHAIN is a normalized, immutable, sorted unboxed
;;; vector of signed sites plus its world domain.  Invariants:
;;;
;;;   - occurrences are ordered primarily by SITE-GEOMETRY, secondarily by
;;;     polarity (positive before negative);
;;;   - one geometry never retains occurrences of both polarities;
;;;   - repeated unmatched occurrences of the surviving polarity remain
;;;     repeated, preserving multiplicity;
;;;   - zero is represented by absence.
;;;
;;; A transient CHAIN-BUILDER accumulates unnormalized occurrences; finishing
;;; it sorts by the geometry-first key and cancels opposites run by run.  The
;;; normalization boundary is one explicit function so that CL:SORT can later
;;; be replaced by a packed radix sort without changing the model.

(deftype site-vector () '(simple-array site (*)))

(declaim (inline site-order-key))
(defun site-order-key (site)
  "The chain ordering key: geometry-major, polarity-minor.

The polarity bit sits below the coordinates inside the packed site, so raw
numeric order would interleave orientations with geometry.  Shifting the
geometry up one bit and appending the polarity bit restores the intended
order while staying a fixnum on 64-bit SBCL."
  (logior (ash (site-geometry site) 1)
          (if (site-negative-p site) 1 0)))

(defstruct (chain (:constructor %make-chain (domain sites))
                  (:predicate chainp))
  "A normalized, immutable collection of signed sites in DOMAIN."
  (domain nil :type world-domain :read-only t)
  (sites nil :type site-vector :read-only t))

(defun chain-count (chain)
  "The number of unmatched signed-site occurrences in CHAIN."
  (length (chain-sites chain)))

(defmethod print-object ((chain chain) stream)
  (print-unreadable-object (chain stream :type t :identity t)
    (format stream "~D signed site~:P" (chain-count chain))))

(defun chain-empty-p (chain)
  (zerop (chain-count chain)))

(defstruct (chain-builder (:constructor %make-chain-builder (domain sites)))
  "A transient accumulator of signed site occurrences."
  (domain nil :type world-domain :read-only t)
  (sites nil :type (array site (*))))

(defun make-chain-builder (domain &key (expected-size 16))
  (check-type domain world-domain)
  (%make-chain-builder
   domain
   (make-array (max expected-size 1)
               :element-type 'site :adjustable t :fill-pointer 0)))

(defun chain-builder-add (builder site)
  "Append signed SITE to BUILDER.  Normalization happens only on finish."
  (checked-site (chain-builder-domain builder) site)
  (vector-push-extend site (chain-builder-sites builder))
  site)

(defun normalize-signed-sites (sites)
  "Destructively sort SITES (a simple site vector) by the geometry-first key
and cancel opposite occurrences run by run.  Return a fresh normalized
simple vector.  This function is the single normalization boundary."
  (declare (type site-vector sites))
  (sort sites #'< :key #'site-order-key)
  (let ((result (make-array (length sites) :element-type 'site))
        (out 0)
        (index 0)
        (length (length sites)))
    (loop while (< index length)
          do (let* ((geometry (site-geometry (aref sites index)))
                    (positives 0)
                    (negatives 0))
               (loop while (and (< index length)
                                (= (site-geometry (aref sites index)) geometry))
                     do (if (site-negative-p (aref sites index))
                            (incf negatives)
                            (incf positives))
                        (incf index))
               (let ((net (- positives negatives)))
                 (unless (zerop net)
                   (let ((survivor (site-with-polarity geometry (signum net))))
                     (loop repeat (abs net)
                           do (setf (aref result out) survivor)
                              (incf out)))))))
    (subseq result 0 out)))

(defun chain-builder-finish (builder)
  "Normalize BUILDER's occurrences into an immutable chain."
  (let* ((raw (chain-builder-sites builder))
         (copy (make-array (fill-pointer raw) :element-type 'site)))
    (replace copy raw)
    (%make-chain (chain-builder-domain builder)
                 (normalize-signed-sites copy))))

(defun chain-of (domain &rest sites)
  "Convenience: the normalized chain of SITES in DOMAIN."
  (let ((builder (make-chain-builder domain :expected-size (length sites))))
    (dolist (site sites)
      (chain-builder-add builder site))
    (chain-builder-finish builder)))

(defun require-same-domain (a b)
  (unless (equalp (chain-domain a) (chain-domain b))
    (error "Chains describe different world domains: ~S and ~S."
           (chain-domain a) (chain-domain b))))

(defun chain-add (a b)
  "Return the normalized sum of chains A and B as a fresh chain.

Because both inputs are normalized, this is a linear merge with run-by-run
cancellation; no re-sorting is needed."
  (require-same-domain a b)
  (let* ((as (chain-sites a)) (bs (chain-sites b))
         (la (length as)) (lb (length bs))
         (result (make-array (+ la lb) :element-type 'site))
         (ia 0) (ib 0) (out 0))
    (flet ((run-net (sites index length geometry)
             ;; Sum the signed occurrences of GEOMETRY starting at INDEX;
             ;; return the net coefficient and the index past the run.
             (let ((net 0))
               (loop while (and (< index length)
                                (= (site-geometry (aref sites index)) geometry))
                     do (incf net (site-polarity (aref sites index)))
                        (incf index))
               (values net index)))
           (emit (geometry net)
             (unless (zerop net)
               (let ((survivor (site-with-polarity geometry (signum net))))
                 (loop repeat (abs net)
                       do (setf (aref result out) survivor)
                          (incf out))))))
      (loop while (and (< ia la) (< ib lb))
            do (let ((ga (site-geometry (aref as ia)))
                     (gb (site-geometry (aref bs ib))))
                 (cond ((< ga gb)
                        (multiple-value-bind (net next) (run-net as ia la ga)
                          (emit ga net) (setf ia next)))
                       ((> ga gb)
                        (multiple-value-bind (net next) (run-net bs ib lb gb)
                          (emit gb net) (setf ib next)))
                       (t
                        (multiple-value-bind (na nexta) (run-net as ia la ga)
                          (multiple-value-bind (nb nextb) (run-net bs ib lb gb)
                            (emit ga (+ na nb))
                            (setf ia nexta ib nextb)))))))
      (loop while (< ia la)
            do (let ((ga (site-geometry (aref as ia))))
                 (multiple-value-bind (net next) (run-net as ia la ga)
                   (emit ga net) (setf ia next))))
      (loop while (< ib lb)
            do (let ((gb (site-geometry (aref bs ib))))
                 (multiple-value-bind (net next) (run-net bs ib lb gb)
                   (emit gb net) (setf ib next)))))
    (%make-chain (chain-domain a) (subseq result 0 out))))

(defun chain-site-count (chain site)
  "How many unmatched copies of the signed SITE occur in CHAIN.

Binary search for the first occurrence of SITE's order key, then count the
(equal) occurrences sequentially."
  (checked-site (chain-domain chain) site)
  (let* ((sites (chain-sites chain))
         (key (site-order-key site))
         (lo 0)
         (hi (length sites)))
    ;; Lower bound of KEY.
    (loop while (< lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (< (site-order-key (aref sites mid)) key)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    (loop with count = 0
          for index from lo below (length sites)
          while (= (aref sites index) site)
          do (incf count)
          finally (return count))))

(defun chain-site-p (chain site)
  (plusp (chain-site-count chain site)))

(defun map-chain (function chain)
  "Call FUNCTION with each signed site occurrence, in normalized order."
  (loop for site across (chain-sites chain)
        do (funcall function site))
  chain)

(defun chain-equal (a b)
  "Whether A and B are the same chain: same domain, same normalized sites."
  (and (equalp (chain-domain a) (chain-domain b))
       (equalp (chain-sites a) (chain-sites b))))

(defun boundary-chain (chain)
  "Return CHAIN's oriented boundary as a new normalized chain.

Each occurrence emits its fixed fan-out of signed boundary sites into a
builder; normalization happens once at the end.  This is where the shared
face of two neighbouring solid cells annihilates."
  (let* ((domain (chain-domain chain))
         (builder (make-chain-builder
                   domain :expected-size (* 6 (chain-count chain)))))
    (map-chain
     (lambda (site)
       (map-site-boundary
        (lambda (part axis side)
          (declare (ignore axis side))
          (chain-builder-add builder part))
        domain site))
     chain)
    (chain-builder-finish builder)))

;;; ========================================================================
;;; 4. Solid worlds
;;;
;;; An incrementally edited solid must not dictate the chain representation.
;;; The mutable occupancy lives in its own compact bit abstraction, from
;;; which a solid 3-chain or oriented surface 2-chain can be materialized.
;;; The chain itself is the algebraic value.
;;;
;;; Cells anchor on Z planes 0 through +TOP-Z+ - 1 (a cell extends upward,
;;; so it cannot anchor on the top plane).  X and Y wrap; Z does not.  The
;;; non-wrapping vertical convention -- cells outside the vertical range are
;;; air -- is centralized in SOLID-WORLD-OCCUPANCY and must not be
;;; re-decided in edge or vertex helpers.

(defconstant +cell-z-rows+ +top-z+
  "The number of vertical cell rows: cells anchor on planes 0..254.")

(defstruct (solid-world (:constructor %make-solid-world (domain bits)))
  "Mutable dense cell occupancy for one world domain."
  (domain nil :type world-domain :read-only t)
  (bits nil :type simple-bit-vector :read-only t))

(defun make-solid-world (domain)
  (check-type domain world-domain)
  (%make-solid-world
   domain
   (make-array (* (world-domain-x-period domain)
                  (world-domain-y-period domain)
                  +cell-z-rows+)
               :element-type 'bit :initial-element 0)))

(declaim (inline cell-bit-index))
(defun cell-bit-index (domain x y z)
  "The dense bit index of the cell anchored at canonical X,Y,Z."
  (+ (logand x (world-domain-x-mask domain))
     (* (world-domain-x-period domain)
        (+ (logand y (world-domain-y-mask domain))
           (* (world-domain-y-period domain) z)))))

(defun solid-cell-p (world x y z)
  "Whether the cell anchored at X,Y,Z is solid.

X and Y wrap under the domain masks.  Z outside the cell rows is air: this
is the single home of the vertical boundary convention."
  (let ((domain (solid-world-domain world)))
    (and (<= 0 z) (< z +cell-z-rows+)
         (= 1 (sbit (solid-world-bits world)
                    (cell-bit-index domain x y z))))))

(defun (setf solid-cell-p) (solid-p world x y z)
  (check-type z (integer 0 #.(1- +cell-z-rows+)))
  (let ((domain (solid-world-domain world)))
    (setf (sbit (solid-world-bits world) (cell-bit-index domain x y z))
          (if solid-p 1 0)))
  solid-p)

(defun solid-world-occupancy (world)
  "Return the occupancy function of WORLD: (X Y Z) -> boolean.

Classification consumes occupancy only through such a function, so the
vertical air convention above applies uniformly to every star query."
  (lambda (x y z) (solid-cell-p world x y z)))

(defun solid-chain (world)
  "Materialize WORLD's solid cells as a normalized positive 3-chain."
  (let* ((domain (solid-world-domain world))
         (builder (make-chain-builder domain)))
    (dotimes (z +cell-z-rows+)
      (dotimes (y (world-domain-y-period domain))
        (dotimes (x (world-domain-x-period domain))
          (when (solid-cell-p world x y z)
            (chain-builder-add
             builder (make-site domain x y z +cell-extent+))))))
    (chain-builder-finish builder)))

(defun surface-chain (solid)
  "Return the oriented surface 2-chain of a solid 3-chain.

This is already the drawable surface topologically; raster triangles are a
later lowering.  Each surviving face's polarity says which way is outward."
  (boundary-chain solid))

;;; ========================================================================
;;; 5. Occupancy stars and the strict-minority moment classifier
;;;
;;; For a cubical site s, its star is the set of incident cells: four for an
;;; edge, eight for a vertex.  For each incident cell, the direction from
;;; the site toward the cell centre (dropping the irrelevant half) is
;;;
;;;     d(c) = sum of (+/- 1) along each axis *absent* from the site.
;;;
;;; Along every absent axis an incident cell is anchored either one unit
;;; below the site (direction component -1) or directly at the site (+1).
;;; Components along present axes are zero, so d is a world 3-vector.

(defun map-site-star (function site)
  "Call FUNCTION with (CX CY CZ DX DY DZ) for each cell in SITE's star.

Coordinates are raw lattice integers; the occupancy function is responsible
for horizontal wrapping and the vertical air convention."
  (let ((extent (site-extent site))
        (anchor (vector (site-x site) (site-y site) (site-z site)))
        (absent '()))
    (dotimes (axis 3)
      (unless (logbitp axis extent)
        (push axis absent)))
    (setf absent (nreverse absent))
    (dotimes (choice (ash 1 (length absent)))
      (let ((c (vector (aref anchor 0) (aref anchor 1) (aref anchor 2)))
            (d (vector 0 0 0)))
        (loop for axis in absent
              for bit from 0
              do (if (logbitp bit choice)
                     (setf (aref d axis) 1)
                     (progn (decf (aref c axis))
                            (setf (aref d axis) -1))))
        (funcall function
                 (aref c 0) (aref c 1) (aref c 2)
                 (aref d 0) (aref d 1) (aref d 2))))))

;;; The strict-minority moment rule:
;;;
;;;     complete site star
;;;         -> strict minority (solid if k < N/2, air if k > N/2, else none)
;;;         -> minority moment m = sum of d over the minority
;;;         -> componentwise signed direction q = sign(m)
;;;         -> half reach, or two-thirds reach at a vertex whose raw moment
;;;            is a unit cube diagonal
;;;         -> displacement  delta = w * reach * q
;;;
;;; The rule is site-local, complement-symmetric (complementing occupancy
;;; selects the same physical minority cells), and equivariant under
;;; permutations and reflections of the world axes.  A balanced star has an
;;; empty minority and stays sharp.
;;;
;;; Do not simplify the two-thirds predicate to "singleton minority": every
;;; singleton minority has a unit-diagonal moment, but some three-cell
;;; minorities do too, and they must also receive the centroid reach.

(defun site-minority-moment (site occupancy)
  "Return SITE's raw strict-minority moment and star census as
(VALUES MX MY MZ K N), where K of the N star cells are solid."
  (let ((n 0) (k 0)
        (sx 0) (sy 0) (sz 0)    ; moment of the solid cells
        (ax 0) (ay 0) (az 0))   ; moment of the air cells
    (map-site-star
     (lambda (cx cy cz dx dy dz)
       (incf n)
       (if (funcall occupancy cx cy cz)
           (progn (incf k) (incf sx dx) (incf sy dy) (incf sz dz))
           (progn (incf ax dx) (incf ay dy) (incf az dz))))
     site)
    (cond ((< (* 2 k) n) (values sx sy sz k n))
          ((> (* 2 k) n) (values ax ay az k n))
          (t (values 0 0 0 k n)))))

(defun unit-diagonal-p (mx my mz)
  "Whether the raw moment is a unit cube diagonal: all components have
magnitude one.  Equivalently norm(q)^2 = 3 and norm(m)^2 < 4."
  (and (= 1 (abs mx)) (= 1 (abs my)) (= 1 (abs mz))))

(defun site-shape (site occupancy)
  "Classify SITE's star.  Return (VALUES QX QY QZ REACH-NUMERATOR
REACH-DENOMINATOR K N): the componentwise signed direction, the reach as an
exact rational numerator/denominator pair, and the star census.

Reach is 1/2 except at a vertex whose raw minority moment is a unit cube
diagonal, where it is the centroid reach 2/3."
  (multiple-value-bind (mx my mz k n) (site-minority-moment site occupancy)
    (let ((two-thirds-p (and (zerop (site-extent site))
                             (unit-diagonal-p mx my mz))))
      (values (signum mx) (signum my) (signum mz)
              (if two-thirds-p 2 1)
              (if two-thirds-p 3 2)
              k n))))

(declaim (inline displacement-component))
(defun displacement-component (w reach q)
  "The single double-float evaluation order shared by every realization
path, so that identical (W, REACH, Q) always yield bit-identical output."
  (declare (type double-float w reach))
  (* w reach (coerce q 'double-float)))

(defun site-displacement (site occupancy w)
  "Return SITE's chamfer displacement as three double-float values.

W is the reserved chamfer width, 0 < W < 1/2.  The displacement depends
only on the canonical site and its complete star, never on which face asked
for it; that is the structural watertightness invariant."
  (declare (type double-float w))
  (multiple-value-bind (qx qy qz num den) (site-shape site occupancy)
    (let ((reach (/ (coerce num 'double-float) (coerce den 'double-float))))
      (values (displacement-component w reach qx)
              (displacement-component w reach qy)
              (displacement-component w reach qz)))))

;;; ========================================================================
;;; 6. Face bases and the local grid
;;;
;;; For a canonical positive face, tangent axes u,v are the extent axes in
;;; increasing order and the canonical normal is n = u x v:
;;;
;;;     YZ face: u=Y, v=Z, n=+X
;;;     XZ face: u=X, v=Z, n=-Y
;;;     XY face: u=X, v=Y, n=+Z
;;;
;;; The site's polarity reverses the *oriented* normal but not the anchor or
;;; the canonical positive tangent basis, so all position arithmetic remains
;;; canonical and only the winding template changes between orientations.

(defun face-basis (extent)
  "Return (VALUES U-AXIS V-AXIS N-AXIS N-SIGN) as axis indices for a face
extent, with the canonical normal n = u x v = N-SIGN along N-AXIS."
  (ecase extent
    (#.+xy-face-extent+ (values 0 1 2 1))
    (#.+xz-face-extent+ (values 0 2 1 -1))
    (#.+yz-face-extent+ (values 1 2 0 1))))

(defun face-outward-sign (face)
  "The sign of FACE's outward normal along its normal axis: the canonical
normal sign multiplied by the face's polarity."
  (multiple-value-bind (u v n n-sign) (face-basis (site-extent face))
    (declare (ignore u v n))
    (* n-sign (site-polarity face))))

;;; The local grid.  With lambda = (0, w, 1-w, 1), the flat point is
;;;
;;;     p0(i,j) = anchor + lambda_i * u + lambda_j * v,  i,j in 0..3.
;;;
;;; Realization displaces the boundary points:
;;;
;;;     interior (i,j in {1,2})            : p0
;;;     exactly one index in {0,3}         : p0 + delta(edge site)
;;;     both indices in {0,3}              : p0 + delta(vertex site)
;;;
;;; The edge and vertex sites are derived from the face anchor and extent --
;;; they are canonical lattice sites, never face-owned objects.  Every
;;; incident face therefore decodes the same world coordinate.

(deftype grid-index () '(integer 0 3))

;; The local point numbering is shared by realization (section 8) and the
;; raster index templates (section 10); define it once, here, ahead of both.
(declaim (inline local-point-index))
(defun local-point-index (i j)
  "The local index of grid point (I,J): 4*I + J."
  (+ (* 4 i) j))

(defun grid-lambda (index w)
  "The local grid coordinate: one of 0, w, 1-w, 1 as a double."
  (declare (type double-float w))
  (ecase index
    (0 0d0)
    (1 w)
    (2 (- 1d0 w))
    (3 1d0)))

(defun face-edge-geometry (domain face which)
  "The canonical (positive) edge site of FACE's boundary edge WHICH, one of
:U-LOW, :U-HIGH, :V-LOW, :V-HIGH."
  (multiple-value-bind (u v n n-sign) (face-basis (site-extent face))
    (declare (ignore n n-sign))
    (let ((x (site-x face)) (y (site-y face)) (z (site-z face)))
      (flet ((bump (axis)
               (ecase axis (0 (incf x)) (1 (incf y)) (2 (incf z)))))
        (multiple-value-bind (along)
            (ecase which
              (:u-low v)
              (:u-high (bump u) v)
              (:v-low u)
              (:v-high (bump v) u))
          (make-site domain x y z (ash 1 along) 1))))))

(defun face-corner-geometry (domain face u-high-p v-high-p)
  "The canonical vertex site of FACE's corner at (u-low/high, v-low/high)."
  (multiple-value-bind (u v n n-sign) (face-basis (site-extent face))
    (declare (ignore n n-sign))
    (let ((x (site-x face)) (y (site-y face)) (z (site-z face)))
      (flet ((bump (axis)
               (ecase axis (0 (incf x)) (1 (incf y)) (2 (incf z)))))
        (when u-high-p (bump u))
        (when v-high-p (bump v))
        (make-site domain x y z +vertex-extent+ 1)))))

;;; ========================================================================
;;; 7. The 32-bit shape word
;;;
;;; The four edge classifications and four vertex results of one face patch
;;; fit in a single u32.  The CPU authors this discrete geometric meaning
;;; once; the GPU realizes the sixteen points from the face site, the shape
;;; word, and w alone.
;;;
;;; Edge fields, two bits each.  An exposed face's boundary edge has one,
;;; two, or three solid cells in its complete star (the face itself
;;; guarantees one solid and one air neighbour):
;;;
;;;     00 balanced  two solid, two air; displacement zero
;;;     01 convex    one solid; the solid cell is the strict minority
;;;     10 concave   three solid; the air cell is the strict minority
;;;     11 reserved / invalid for a boundary edge
;;;
;;; The two-bit state omits the world direction; the face incidence supplies
;;; the outward normal n and the outward in-plane tangent t, and decodes
;;;
;;;     balanced -> q = 0
;;;     convex   -> q = -n - t
;;;     concave  -> q = +n - t
;;;
;;; Every incident face reconstructs the same canonical q: for a convex
;;; boundary edge the sole solid cell *is* the face pair's solid cell, on
;;; the (-n, -t) diagonal; for a concave one the sole air cell is the face
;;; pair's air cell, on the (+n, -t) diagonal.  So the decode agrees with
;;; the generic strict-minority moment by construction, and the tests prove
;;; it exhaustively.
;;;
;;; Corner fields, six bits each: the world-space vertex direction q maps
;;; each component through -1 -> 0, 0 -> 1, +1 -> 2 into a ternary digit,
;;;
;;;     direction-code = tx + 3*ty + 9*tz          (0..26)
;;;     corner-code    = direction-code | (two-thirds-p << 5)
;;;
;;; Direction subfield values 27..31 are reserved.

(defconstant +edge-balanced+ #b00)
(defconstant +edge-convex+   #b01)
(defconstant +edge-concave+  #b10)
(defconstant +edge-reserved+ #b11)

(defconstant +edge-field-bits+ 2)
(defconstant +edge-u-low-shift+ 0)
(defconstant +edge-u-high-shift+ 2)
(defconstant +edge-v-low-shift+ 4)
(defconstant +edge-v-high-shift+ 6)

(defconstant +corner-field-bits+ 6)
(defconstant +corner-direction-bits+ 5)
(defconstant +corner-two-thirds-bit+ 5)
(defconstant +corner-ll-shift+ 8)   ; (u-low,  v-low)
(defconstant +corner-lh-shift+ 14)  ; (u-low,  v-high)
(defconstant +corner-hl-shift+ 20)  ; (u-high, v-low)
(defconstant +corner-hh-shift+ 26)  ; (u-high, v-high)

(defconstant +max-direction-code+ 26)

(defun edge-code-from-census (k n)
  "The two-bit edge class from a boundary edge's star census."
  (unless (= n 4)
    (error "Not an edge star: ~D incident cells." n))
  (ecase k
    (2 +edge-balanced+)
    (1 +edge-convex+)
    (3 +edge-concave+)))

(defun check-edge-code (code)
  (when (= code +edge-reserved+)
    (error "Reserved edge code ~2,'0B in shape word." code))
  code)

(defun corner-code (qx qy qz two-thirds-p)
  "Pack a vertex direction and reach flag into six bits."
  (let ((code (+ (1+ qx) (* 3 (1+ qy)) (* 9 (1+ qz)))))
    (logior code (if two-thirds-p (ash 1 +corner-two-thirds-bit+) 0))))

(defun decode-corner-code (code)
  "Return (VALUES QX QY QZ TWO-THIRDS-P) from a six-bit corner code."
  (let ((direction (ldb (byte +corner-direction-bits+ 0) code)))
    (when (> direction +max-direction-code+)
      (error "Reserved corner direction code ~D in shape word." direction))
    (values (- (mod direction 3) 1)
            (- (mod (floor direction 3) 3) 1)
            (- (floor direction 9) 1)
            (logbitp +corner-two-thirds-bit+ code))))

(defun pack-shape-word (edge-u-low edge-u-high edge-v-low edge-v-high
                        corner-ll corner-lh corner-hl corner-hh)
  "Pack the four edge codes and four corner codes into one u32."
  (dolist (edge (list edge-u-low edge-u-high edge-v-low edge-v-high))
    (check-edge-code edge))
  (dolist (corner (list corner-ll corner-lh corner-hl corner-hh))
    (decode-corner-code corner))          ; validates the direction subfield
  (logior (ash edge-u-low +edge-u-low-shift+)
          (ash edge-u-high +edge-u-high-shift+)
          (ash edge-v-low +edge-v-low-shift+)
          (ash edge-v-high +edge-v-high-shift+)
          (ash corner-ll +corner-ll-shift+)
          (ash corner-lh +corner-lh-shift+)
          (ash corner-hl +corner-hl-shift+)
          (ash corner-hh +corner-hh-shift+)))

(defun unpack-shape-word (word)
  "Return the four edge codes and four corner codes of WORD as eight
values, validating reserved encodings."
  (let ((eul (ldb (byte +edge-field-bits+ +edge-u-low-shift+) word))
        (euh (ldb (byte +edge-field-bits+ +edge-u-high-shift+) word))
        (evl (ldb (byte +edge-field-bits+ +edge-v-low-shift+) word))
        (evh (ldb (byte +edge-field-bits+ +edge-v-high-shift+) word))
        (cll (ldb (byte +corner-field-bits+ +corner-ll-shift+) word))
        (clh (ldb (byte +corner-field-bits+ +corner-lh-shift+) word))
        (chl (ldb (byte +corner-field-bits+ +corner-hl-shift+) word))
        (chh (ldb (byte +corner-field-bits+ +corner-hh-shift+) word)))
    (mapc #'check-edge-code (list eul euh evl evh))
    (mapc #'decode-corner-code (list cll clh chl chh))
    (values eul euh evl evh cll clh chl chh)))

(defun face-shape-word (domain face occupancy)
  "Classify FACE's four boundary edges and four corners once each and pack
the results into one shape word.

The four interior points require no classification, and each edge or vertex
site is classified exactly once from its complete star -- never once per
grid point.  FACE must be an exposed surface face: solid on the inward side
of its oriented normal and air on the outward side."
  (checked-site domain face)
  (unless (= 2 (site-dimension face))
    (error "~S is not a face site." face))
  ;; Assert the surface invariant for the face pair.
  (multiple-value-bind (u v n-axis n-sign) (face-basis (site-extent face))
    (declare (ignore u v))
    (let* ((outward (* n-sign (site-polarity face)))
           (x (site-x face)) (y (site-y face)) (z (site-z face))
           (coords (vector x y z)))
      (flet ((cell (offset)
               (let ((c (copy-seq coords)))
                 (incf (aref c n-axis) offset)
                 (funcall occupancy (aref c 0) (aref c 1) (aref c 2)))))
        ;; The cell anchored at the face is on the +canonical-normal side.
        (let ((solid (if (plusp outward) (cell -1) (cell 0)))
              (air (if (plusp outward) (cell 0) (cell -1))))
          (unless (and solid (not air))
            (error "~S is not an exposed surface face of this occupancy."
                   face))))))
  (flet ((edge-code (which)
           (multiple-value-bind (mx my mz k n)
               (site-minority-moment (face-edge-geometry domain face which)
                                     occupancy)
             (declare (ignore mx my mz))
             (edge-code-from-census k n)))
         (corner (u-high-p v-high-p)
           (multiple-value-bind (qx qy qz num den)
               (site-shape (face-corner-geometry domain face u-high-p v-high-p)
                           occupancy)
             (declare (ignore den))
             (corner-code qx qy qz (= num 2)))))
    (pack-shape-word (edge-code :u-low) (edge-code :u-high)
                     (edge-code :v-low) (edge-code :v-high)
                     (corner nil nil) (corner nil t)
                     (corner t nil) (corner t t))))

;;; ========================================================================
;;; 8. Realizing one face patch
;;;
;;; Terminology, kept deliberately distinct:
;;;
;;;   - a ZERO-SITE is a canonical lattice vertex;
;;;   - a REALIZED POINT is a geometric position obtained from a site and
;;;     its occupancy star;
;;;   - a RENDER VERTEX is an equivalence class of vertex-stage attributes;
;;;   - a TRIANGLE CORNER is an incidence of a render vertex in a triangle.
;;;
;;; One face patch has sixteen distinct local points, eighteen triangles,
;;; and 54 triangle-corner incidences.  The index buffer represents the last
;;; incidence relation; nothing in this file ever copies a vertex record per
;;; triangle corner.

(defconstant +face-point-count+ 16)

(defun realized-points-array ()
  (make-array (* 3 +face-point-count+) :element-type 'double-float
              :initial-element 0d0))

(defun flat-point-into (points index face i j w)
  "Write the flat local grid point p0(i,j) into POINTS at slot INDEX.

Every coordinate is (+ anchor lambda): an exact integer plus one of 0, w,
1-w, 1.  Adjacent faces reach a shared world point through anchors and
lambdas that produce the *same* two addends, so this fixed evaluation order
makes shared coordinates bit-identical across faces."
  (multiple-value-bind (u v n n-sign) (face-basis (site-extent face))
    (declare (ignore n n-sign))
    (let ((base (* 3 index))
          (lu (grid-lambda i w))
          (lv (grid-lambda j w)))
      (setf (aref points (+ base 0)) (coerce (site-x face) 'double-float)
            (aref points (+ base 1)) (coerce (site-y face) 'double-float)
            (aref points (+ base 2)) (coerce (site-z face) 'double-float))
      (incf (aref points (+ base u)) lu)
      (incf (aref points (+ base v)) lv)
      points)))

(defun add-displacement (points index dx dy dz)
  (let ((base (* 3 index)))
    (incf (aref points (+ base 0)) dx)
    (incf (aref points (+ base 1)) dy)
    (incf (aref points (+ base 2)) dz)))

(defun grid-point-role (i j)
  "Classify local grid point (I,J): (VALUES ROLE WHICH U-HIGH-P V-HIGH-P)
where ROLE is :INTERIOR, :EDGE, or :CORNER."
  (let ((iu (or (= i 0) (= i 3)))
        (jv (or (= j 0) (= j 3))))
    (cond ((and iu jv) (values :corner nil (= i 3) (= j 3)))
          (iu (values :edge (if (= i 0) :u-low :u-high) nil nil))
          (jv (values :edge (if (= j 0) :v-low :v-high) nil nil))
          (t (values :interior nil nil nil)))))

(defun reference-face-patch (domain face occupancy w)
  "The CPU reference realization: the sixteen world-space points of FACE's
patch as a 48-element double-float array, indexed by LOCAL-POINT-INDEX.

Each boundary point's displacement comes from classifying the complete star
of its canonical edge or vertex site.  A shared shape word (below) must
reproduce these positions exactly."
  (declare (type double-float w))
  (checked-site domain face)
  (let ((points (realized-points-array)))
    (dotimes (i 4)
      (dotimes (j 4)
        (let ((index (local-point-index i j)))
          (flat-point-into points index face i j w)
          (multiple-value-bind (role which u-high-p v-high-p)
              (grid-point-role i j)
            (ecase role
              (:interior)
              (:edge
               (multiple-value-bind (dx dy dz)
                   (site-displacement (face-edge-geometry domain face which)
                                      occupancy w)
                 (add-displacement points index dx dy dz)))
              (:corner
               (multiple-value-bind (dx dy dz)
                   (site-displacement
                    (face-corner-geometry domain face u-high-p v-high-p)
                    occupancy w)
                 (add-displacement points index dx dy dz))))))))
    points))

;;; Realization from the shape word alone: what the GPU will eventually do.
;;; No occupancy is consulted; the face site, the shape word, and w suffice.

(defun edge-outward-tangent (which)
  "The outward in-plane tangent of a local face edge, as (VALUES BASIS-AXIS
SIGN) where BASIS-AXIS is :U or :V."
  (ecase which
    (:u-low (values :u -1))
    (:u-high (values :u 1))
    (:v-low (values :v -1))
    (:v-high (values :v 1))))

(defun decode-edge-displacement (face which code w)
  "Decode a two-bit edge class at FACE's incidence WHICH into a world
displacement (VALUES DX DY DZ), using the face's outward normal n and the
edge's outward in-plane tangent t:

    balanced -> 0,  convex -> (w/2)(-n - t),  concave -> (w/2)(+n - t)."
  (declare (type double-float w))
  (check-edge-code code)
  (multiple-value-bind (u v n-axis n-sign) (face-basis (site-extent face))
    (multiple-value-bind (t-basis t-sign) (edge-outward-tangent which)
      (let ((q (vector 0 0 0))
            (outward (* n-sign (site-polarity face)))
            (t-axis (if (eq t-basis :u) u v)))
        (cond ((= code +edge-balanced+))
              ((= code +edge-convex+)
               (decf (aref q n-axis) outward)
               (decf (aref q t-axis) t-sign))
              ((= code +edge-concave+)
               (incf (aref q n-axis) outward)
               (decf (aref q t-axis) t-sign)))
        (values (displacement-component w 0.5d0 (aref q 0))
                (displacement-component w 0.5d0 (aref q 1))
                (displacement-component w 0.5d0 (aref q 2)))))))

(defun decode-corner-displacement (code w)
  "Decode a six-bit corner code into a world displacement."
  (declare (type double-float w))
  (multiple-value-bind (qx qy qz two-thirds-p) (decode-corner-code code)
    (let ((reach (if two-thirds-p (/ 2d0 3d0) 0.5d0)))
      (values (displacement-component w reach qx)
              (displacement-component w reach qy)
              (displacement-component w reach qz)))))

(defun realize-face-patch (domain face shape-word w)
  "Realize FACE's sixteen patch points from the oriented face site and its
shape word alone, with no occupancy access.  This is the CPU reference for
the eventual GPU vertex stage; it must agree bitwise with
REFERENCE-FACE-PATCH."
  (declare (type double-float w))
  (checked-site domain face)
  (multiple-value-bind (eul euh evl evh cll clh chl chh)
      (unpack-shape-word shape-word)
    (let ((points (realized-points-array)))
      (dotimes (i 4)
        (dotimes (j 4)
          (let ((index (local-point-index i j)))
            (flat-point-into points index face i j w)
            (multiple-value-bind (role which u-high-p v-high-p)
                (grid-point-role i j)
              (ecase role
                (:interior)
                (:edge
                 (multiple-value-bind (dx dy dz)
                     (decode-edge-displacement
                      face which
                      (ecase which
                        (:u-low eul) (:u-high euh)
                        (:v-low evl) (:v-high evh))
                      w)
                   (add-displacement points index dx dy dz)))
                (:corner
                 (multiple-value-bind (dx dy dz)
                     (decode-corner-displacement
                      (cond ((and (not u-high-p) (not v-high-p)) cll)
                            ((and (not u-high-p) v-high-p) clh)
                            ((and u-high-p (not v-high-p)) chl)
                            (t chh))
                      w)
                   (add-displacement points index dx dy dz))))))))
      points)))

;;; ========================================================================
;;; 9. The 16-byte face-record ABI
;;;
;;; One materialization owner stores a dense array of four u32 words per
;;; exposed face:
;;;
;;;     word 0   low 32 bits of the decorated oriented face site
;;;     word 1   high 32 bits of the decorated oriented face site
;;;     word 2   shape word
;;;     word 3   zero; reserved for ABI revision or future data
;;;
;;; The topological site occupies the low 60 bits; the spare high nibble is
;;; a stock/material slot.  Site-plus-nibble is a DECORATED SITE; mask the
;;; nibble before applying topological operations.  The wire record is four
;;; u32s even though the CPU manipulates the site as one integer, so the GPU
;;; ABI never depends on native 64-bit shader integers.
;;;
;;; An isolated cell costs six 16-byte records -- 96 bytes -- rather than
;;; thousands of copied floats.  The shared topology (the two 54-index
;;; winding templates) is not a per-cell cost.

(defconstant +face-record-words+ 4)
(defconstant +stock-bits+ 4)
(defconstant +stock-shift+ +site-bits+)

(defun decorated-site (site &optional (stock 0))
  "Attach a four-bit stock/material slot above the 60 topological bits."
  (check-type stock (unsigned-byte 4))
  (logior site (ash stock +stock-shift+)))

(defun decorated-site-site (decorated)
  "The topological site of a decorated site: the low 60 bits."
  (ldb (byte +site-bits+ 0) decorated))

(defun decorated-site-stock (decorated)
  (ldb (byte +stock-bits+ +stock-shift+) decorated))

(defun pack-face-record (words offset decorated shape-word)
  "Write one face record into WORDS, a (u32) array, at word OFFSET."
  (setf (aref words (+ offset 0)) (ldb (byte 32 0) decorated)
        (aref words (+ offset 1)) (ldb (byte 32 32) decorated)
        (aref words (+ offset 2)) shape-word
        (aref words (+ offset 3)) 0)
  words)

(defun unpack-face-record (words offset)
  "Return (VALUES DECORATED-SITE SHAPE-WORD RESERVED) from WORDS at OFFSET."
  (values (logior (aref words (+ offset 0))
                  (ash (aref words (+ offset 1)) 32))
          (aref words (+ offset 2))
          (aref words (+ offset 3))))

(defun materialize-surface (world w &key (stock 0))
  "Materialize WORLD's exposed surface as (VALUES RECORDS FACES): a dense
u32 face-record array and the corresponding vector of oriented face sites.

This is the CPU end of the conceptual pipeline: occupancy -> solid 3-chain
-> oriented surface 2-chain -> per-face classification -> shape word ->
dense face records.  W participates only in later realization; it is
accepted here to keep the call shape stable."
  (declare (ignore w))
  (let* ((domain (solid-world-domain world))
         (occupancy (solid-world-occupancy world))
         (surface (surface-chain (solid-chain world)))
         (faces (chain-sites surface))
         (records (make-array (* +face-record-words+ (length faces))
                              :element-type '(unsigned-byte 32))))
    (loop for face across faces
          for index from 0
          do (pack-face-record records (* +face-record-words+ index)
                               (decorated-site face stock)
                               (face-shape-word domain face occupancy)))
    (values records faces)))

;;; ========================================================================
;;; 10. Canonical raster topology
;;;
;;; Local point numbering is fixed once:
;;;
;;;     local-index(i,j) = 4*i + j
;;;
;;; For each of the nine grid quads, the corners in cyclic positive order:
;;;
;;;     c00 = (i,   j)      c10 = (i+1, j)
;;;     c11 = (i+1, j+1)    c01 = (i,   j+1)
;;;
;;; The templates split every quad along the c00-c11 diagonal:
;;;
;;;     positive winding:  (c00 c10 c11) (c00 c11 c01)
;;;
;;; Negative winding swaps the final two indices of every triangle.  The
;;; templates are generated from explicit coordinates, never copied; the
;;; tests verify winding and coverage.  Each template is 54 u16 indices --
;;; 108 permanent bytes -- shared by every face it draws.

(defun build-face-index-template (winding)
  "Generate the 54-entry u16 index template with WINDING :POSITIVE or
:NEGATIVE."
  (check-type winding (member :positive :negative))
  (let ((indices (make-array 54 :element-type '(unsigned-byte 16)))
        (out 0))
    (flet ((triangle (a b c)
             (multiple-value-bind (b c)
                 (if (eq winding :positive) (values b c) (values c b))
               (setf (aref indices out) a
                     (aref indices (+ out 1)) b
                     (aref indices (+ out 2)) c)
               (incf out 3))))
      (dotimes (i 3)
        (dotimes (j 3)
          (let ((c00 (local-point-index i j))
                (c10 (local-point-index (1+ i) j))
                (c11 (local-point-index (1+ i) (1+ j)))
                (c01 (local-point-index i (1+ j))))
            (triangle c00 c10 c11)
            (triangle c00 c11 c01)))))
    indices))

(defparameter +positive-face-indices+ (build-face-index-template :positive)
  "The permanent positive-winding index template: 18 triangles, 54 indices.")

(defparameter +negative-face-indices+ (build-face-index-template :negative)
  "The permanent negative-winding index template: 18 triangles, 54 indices.")

;;; ========================================================================
;;; 11. Tests
;;;
;;; A directly callable entry point exercising the invariants of every
;;; section.  Failures signal informative errors.

(defvar *test-count* 0)

(defmacro check (form &optional (message nil) &rest arguments)
  `(progn
     (incf *test-count*)
     (unless ,form
       (error "Test failed: ~S~@[~%  ~?~]"
              ',form ,message (list ,@arguments)))))

(defun test-domain ()
  (make-world-domain :x-bits 4 :y-bits 4))

;;; ------------------------------------------------------------------------
;;; Packed sites and topology

(defun test-site-round-trips ()
  (let ((domain (test-domain)))
    (dolist (extent (list +vertex-extent+ +x-edge-extent+ +y-edge-extent+
                          +z-edge-extent+ +xy-face-extent+ +xz-face-extent+
                          +yz-face-extent+ +cell-extent+))
      (dolist (polarity '(1 -1))
        (let ((site (make-site domain 5 11 7 extent polarity)))
          (check (= (site-x site) 5))
          (check (= (site-y site) 11))
          (check (= (site-z site) 7))
          (check (= (site-extent site) extent))
          (check (= (site-polarity site) polarity))
          (check (= (site-dimension site) (logcount extent)))
          (check (site-valid-p domain site))
          (check (= (site-geometry site) (site-geometry (opposite-site site))))
          (check (= (opposite-site (opposite-site site)) site)))))
    ;; X and Y wrap under the domain masks.
    (check (= (site-x (make-site domain 19 0 0)) 3))
    (check (= (site-y (make-site domain 0 -1 0)) 15))
    ;; Z rejects extension from the top plane and out-of-range anchors.
    (check (nth-value 1 (ignore-errors
                          (make-site domain 0 0 +top-z+ +z-edge-extent+))))
    (check (nth-value 1 (ignore-errors (make-site domain 0 0 -1))))
    (check (nth-value 1 (ignore-errors (make-site domain 0 0 256))))
    (check (site-valid-p domain (make-site domain 0 0 +top-z+)))
    ;; Stepping off the vertical world returns NIL.
    (check (null (step-site domain (make-site domain 0 0 0) :z -1)))
    (check (null (step-site domain
                            (make-site domain 0 0 (1- +top-z+)
                                       +cell-extent+)
                            :z 1)))
    ;; A value with inactive upper X bits set is noncanonical here.
    (let ((wide (make-world-domain :x-bits 24 :y-bits 24)))
      (check (not (site-valid-p domain (make-site wide 100 0 0)))))))

(defun representative-sites (domain)
  (loop for extent in (list +vertex-extent+ +x-edge-extent+ +y-edge-extent+
                            +z-edge-extent+ +xy-face-extent+ +xz-face-extent+
                            +yz-face-extent+ +cell-extent+)
        append (list (make-site domain 3 5 7 extent 1)
                     (make-site domain 3 5 7 extent -1)
                     (make-site domain 0 0 0 extent 1))))

(defun test-boundary-and-cofaces ()
  (let ((domain (test-domain)))
    (dolist (site (representative-sites domain))
      ;; Every boundary part has exactly one lower dimension, and opposite
      ;; parent polarity reverses every boundary polarity.
      (map-site-boundary
       (lambda (part axis side)
         (check (= (site-dimension part) (1- (site-dimension site))))
         (let ((mirror (if (eq side :low)
                           (site-boundary-low domain (opposite-site site) axis)
                           (site-boundary-high domain (opposite-site site)
                                               axis))))
           (check (= mirror (opposite-site part)))))
       domain site)
      ;; boundary(boundary(site)) normalizes to zero.
      (check (chain-empty-p
              (boundary-chain (boundary-chain (chain-of domain site))))
             "d(d ~S) is not zero" site)
      ;; Cofaces reproduce the requested signed boundary.
      (dolist (axis '(:x :y :z))
        (unless (site-extends-p site axis)
          (let ((forward (site-coface-forward domain site axis))
                (backward (site-coface-backward domain site axis)))
            (when forward
              (check (= (site-boundary-low domain forward axis) site)
                     "forward coface of ~S along ~S" site axis))
            (when backward
              (check (= (site-boundary-high domain backward axis) site)
                     "backward coface of ~S along ~S" site axis))))))
    ;; A cell produces six faces, a face four edges, an edge two vertices.
    (flet ((boundary-size (extent)
             (chain-count
              (boundary-chain
               (chain-of domain (make-site domain 1 1 1 extent))))))
      (check (= 6 (boundary-size +cell-extent+)))
      (check (= 4 (boundary-size +xy-face-extent+)))
      (check (= 2 (boundary-size +x-edge-extent+))))
    ;; Neighbouring positive cells annihilate their shared face.
    (let* ((a (make-site domain 0 0 0 +cell-extent+))
           (b (make-site domain 1 0 0 +cell-extent+))
           (surface (boundary-chain (chain-of domain a b))))
      (check (= 10 (chain-count surface)))
      (check (= (site-boundary-high domain a :x)
                (opposite-site (site-boundary-low domain b :x))))
      (check (zerop (chain-site-count
                     surface
                     (site-geometry (site-boundary-high domain a :x))))))))

;;; ------------------------------------------------------------------------
;;; Chains

(defun shuffled (list)
  (let ((vector (coerce list 'vector)))
    (loop for i from (1- (length vector)) downto 1
          do (rotatef (aref vector i) (aref vector (random (1+ i)))))
    (coerce vector 'list)))

(defun test-chains ()
  (let* ((domain (test-domain))
         (a (make-site domain 1 2 3 +cell-extent+ 1))
         (b (make-site domain 2 2 3 +cell-extent+ 1))
         (c (make-site domain 3 2 3 +xy-face-extent+ -1)))
    ;; Builder normalization is independent of input order.
    (let ((sites (list a a b c (opposite-site b) c)))
      (let ((first (apply #'chain-of domain sites))
            (second (apply #'chain-of domain (shuffled sites)))
            (third (apply #'chain-of domain (reverse sites))))
        (check (chain-equal first second))
        (check (chain-equal first third))
        ;; a twice, b annihilated, c twice.
        (check (= 4 (chain-count first)))
        (check (= 2 (chain-site-count first a)))
        (check (= 0 (chain-site-count first b)))
        (check (= 0 (chain-site-count first (opposite-site b))))
        (check (= 2 (chain-site-count first c)))
        (check (= 0 (chain-site-count first (opposite-site c))))
        (check (chain-site-p first a))
        (check (not (chain-site-p first b)))))
    ;; Normalized order is geometry-first.
    (let ((chain (chain-of domain c a (opposite-site c) b c)))
      (let ((keys (map 'list #'site-order-key (chain-sites chain))))
        (check (equal keys (sort (copy-seq keys) #'<)))))
    ;; Merging normalized chains agrees with normalizing the concatenation.
    (let* ((left (chain-of domain a b (opposite-site c)))
           (right (chain-of domain (opposite-site a) c c b))
           (merged (chain-add left right))
           (concatenated (apply #'chain-of domain
                                (concatenate 'list
                                             (chain-sites left)
                                             (chain-sites right)))))
      (check (chain-equal merged concatenated))
      (check (= 0 (chain-site-count merged a)))
      (check (= 2 (chain-site-count merged b)))
      (check (= 1 (chain-site-count merged c))))
    ;; Mapping visits every occurrence in order; equality respects
    ;; multiplicity and domain.
    (let ((chain (chain-of domain a a b))
          (visited '()))
      (map-chain (lambda (site) (push site visited)) chain)
      (check (= 3 (length visited)))
      (check (equalp (coerce (reverse visited) 'vector) (chain-sites chain)))
      (check (not (chain-equal chain (chain-of domain a b)))))
    ;; Chains over different domains refuse to mix.
    (check (nth-value 1 (ignore-errors
                          (chain-add
                           (chain-of domain a)
                           (chain-of (make-world-domain :x-bits 5 :y-bits 4)
                                     a)))))))

;;; ------------------------------------------------------------------------
;;; Strict-minority classification, exhaustively at the mask level
;;;
;;; A star pattern for r absent axes is a 2^r-bit mask whose bit index b
;;; encodes the direction signs: component i of d is +1 when bit i of b is
;;; set, else -1.  These helpers recompute the rule at the mask level so
;;; the site-level classifier can be checked against an independent path.

(defun mask-direction (bit r)
  (loop for i below r
        collect (if (logbitp i bit) 1 -1)))

(defun mask-moment (mask r)
  "The strict-minority moment of a 2^r-bit occupancy MASK, as a list."
  (let* ((n (ash 1 r))
         (k (logcount mask))
         (minority (cond ((< (* 2 k) n) :solid)
                         ((> (* 2 k) n) :air)
                         (t nil)))
         (m (make-list r :initial-element 0)))
    (when minority
      (dotimes (b n)
        (when (eq (if (logbitp b mask) :solid :air) minority)
          (setf m (mapcar #'+ m (mask-direction b r))))))
    m))

(defun permute-mask (mask permutation)
  "Relabel the axes of a vertex MASK by PERMUTATION, a list where new axis
i reads old axis (nth i permutation)."
  (let ((result 0))
    (dotimes (b 8 result)
      (when (logbitp b mask)
        (let ((image 0))
          (dotimes (i 3)
            (when (logbitp (nth i permutation) b)
              (setf image (logior image (ash 1 i)))))
          (setf result (logior result (ash 1 image))))))))

(defun reflect-mask (mask axis)
  "Reflect a vertex MASK along AXIS by flipping that direction bit."
  (let ((result 0))
    (dotimes (b 8 result)
      (when (logbitp b mask)
        (setf result (logior result (ash 1 (logxor b (ash 1 axis)))))))))

(defun test-strict-minority ()
  ;; Edge stars: all 16 patterns.
  (dotimes (mask 16)
    (let ((m (mask-moment mask 2))
          (k (logcount mask)))
      (when (= k 2)
        (check (equal m '(0 0)) "balanced edge mask ~B" mask))
      ;; Complementing occupancy leaves the moment (and displacement)
      ;; unchanged: the same physical cells stay the minority.
      (check (equal m (mask-moment (logxor mask #b1111) 2))
             "edge complement of ~B" mask)))
  ;; Vertex stars: all 256 patterns.
  (dotimes (mask 256)
    (let* ((m (mask-moment mask 3))
           (k (logcount mask))
           (unit (every (lambda (c) (= 1 (abs c))) m)))
      (when (= k 4)
        (check (equal m '(0 0 0)) "balanced vertex mask ~B" mask))
      (check (equal m (mask-moment (logxor mask #b11111111) 3))
             "vertex complement of ~B" mask)
      ;; Singleton minorities and their complements are unit diagonals.
      (when (or (= k 1) (= k 7))
        (check unit "singleton minority mask ~B" mask))
      ;; The two-thirds predicate is exactly unit-diagonality, never
      ;; "singleton minority": some three-cell minorities qualify too.
      (check (eq unit (apply #'unit-diagonal-p m)))))
  ;; A three-cell minority with a unit-diagonal moment exists.
  (let ((mask (logior (ash 1 #b001) (ash 1 #b010) (ash 1 #b100))))
    (check (= 3 (logcount mask)))
    (check (equal '(-1 -1 -1) (mask-moment mask 3))))
  ;; Permutation and reflection equivariance over all vertex masks.
  (dolist (permutation '((0 1 2) (1 0 2) (0 2 1) (2 1 0) (1 2 0) (2 0 1)))
    (dotimes (mask 256)
      (let ((m (mask-moment mask 3))
            (pm (mask-moment (permute-mask mask permutation) 3)))
        ;; New axis i of the permuted moment reads old axis p_i.
        (check (equal pm
                      (loop for i below 3
                            collect (nth (nth i permutation) m)))
               "permutation ~S of mask ~B" permutation mask))))
  (dotimes (axis 3)
    (dotimes (mask 256)
      (let ((m (mask-moment mask 3))
            (rm (mask-moment (reflect-mask mask axis) 3)))
        (check (equal rm (loop for i below 3
                               for c in m
                               collect (if (= i axis) (- c) c)))
               "reflection along ~D of mask ~B" axis mask))))
  ;; The site-level classifier agrees with the mask-level recomputation for
  ;; every vertex pattern, exercised through a synthetic occupancy.
  (let* ((domain (test-domain))
         (vertex (make-site domain 2 2 2 +vertex-extent+)))
    (dotimes (mask 256)
      (let ((occupancy
              (lambda (x y z)
                (let ((bit (+ (- x 1) (* 2 (- y 1)) (* 4 (- z 1)))))
                  (logbitp bit mask)))))
        (multiple-value-bind (mx my mz k n)
            (site-minority-moment vertex occupancy)
          (check (= n 8))
          (check (= k (logcount mask)))
          (check (equal (list mx my mz) (mask-moment mask 3))
                 "site classifier vs mask ~B" mask)
          (multiple-value-bind (qx qy qz num den) (site-shape vertex occupancy)
            (check (= qx (signum mx)))
            (check (= qy (signum my)))
            (check (= qz (signum mz)))
            (check (eq (= num 2)
                       (unit-diagonal-p mx my mz)))
            (check (member (/ num den) '(1/2 2/3)))))))
    ;; And the analogous check for every edge pattern, on a Z edge whose
    ;; absent axes are X and Y.
    (let ((edge (make-site domain 2 2 2 +z-edge-extent+)))
      (dotimes (mask 16)
        (let ((occupancy
                (lambda (x y z)
                  (declare (ignore z))
                  (let ((bit (+ (- x 1) (* 2 (- y 1)))))
                    (logbitp bit mask)))))
          (multiple-value-bind (mx my mz k n)
              (site-minority-moment edge occupancy)
            (check (= n 4))
            (check (= k (logcount mask)))
            (check (zerop mz))
            (check (equal (list mx my) (mask-moment mask 2))
                   "edge site classifier vs mask ~B" mask)))))))

;;; ------------------------------------------------------------------------
;;; Shape words, face records, and watertight closure

(defun test-shape-word-packing ()
  ;; Round trips over representative codes.
  (dolist (edges '((0 0 0 0) (1 2 0 1) (2 2 2 2)))
    (dolist (corners '((13 13 13 13)
                       (0 26 45 58)))   ; 45 = 13|32, 58 = 26|32
      (multiple-value-bind (eul euh evl evh cll clh chl chh)
          (unpack-shape-word (apply #'pack-shape-word
                                    (append edges corners)))
        (check (equal (list eul euh evl evh) edges))
        (check (equal (list cll clh chl chh) corners)))))
  ;; Reserved encodings are rejected on both sides.
  (check (nth-value 1 (ignore-errors
                        (pack-shape-word 3 0 0 0 13 13 13 13))))
  (check (nth-value 1 (ignore-errors
                        (pack-shape-word 0 0 0 0 27 13 13 13))))
  (check (nth-value 1 (ignore-errors (unpack-shape-word #b11))))
  (check (nth-value 1 (ignore-errors
                        (unpack-shape-word (ash 27 +corner-ll-shift+)))))
  ;; Corner code round trips over the full q cube.
  (dolist (two-thirds '(nil t))
    (loop for qx from -1 to 1
          do (loop for qy from -1 to 1
                   do (loop for qz from -1 to 1
                            do (multiple-value-bind (rx ry rz r23)
                                   (decode-corner-code
                                    (corner-code qx qy qz two-thirds))
                                 (check (= rx qx))
                                 (check (= ry qy))
                                 (check (= rz qz))
                                 (check (eq r23 two-thirds))))))))

(defun make-test-solid (cells)
  "A small solid world containing CELLS, a list of (x y z) triples."
  (let ((world (make-solid-world (test-domain))))
    (dolist (cell cells world)
      (destructuring-bind (x y z) cell
        (setf (solid-cell-p world x y z) t)))))

(defun test-solids ()
  (let* ((world (make-test-solid '((1 1 1))))
         (solid (solid-chain world))
         (surface (surface-chain solid)))
    (check (= 1 (chain-count solid)))
    (check (= 6 (chain-count surface)))
    (check (solid-cell-p world 1 1 1))
    (check (not (solid-cell-p world 0 1 1)))
    ;; The centralized vertical convention: out-of-range Z is air.
    (check (not (solid-cell-p world 1 1 -1)))
    (check (not (solid-cell-p world 1 1 255)))
    ;; Horizontal wrap reaches the same cell.
    (check (solid-cell-p world 17 17 1))
    ;; Two neighbouring cells expose ten faces.
    (check (= 10 (chain-count
                  (surface-chain
                   (solid-chain (make-test-solid '((1 1 1) (2 1 1))))))))))

(defun collect-face-realizations (world w)
  "Realize every exposed face of WORLD through the shape-word path, after
verifying it against the direct reference.  Return a list of
(FACE SHAPE-WORD POINTS)."
  (let* ((domain (solid-world-domain world))
         (occupancy (solid-world-occupancy world))
         (surface (surface-chain (solid-chain world)))
         (results '()))
    (map-chain
     (lambda (face)
       (let* ((shape (face-shape-word domain face occupancy))
              (reference (reference-face-patch domain face occupancy w))
              (realized (realize-face-patch domain face shape w)))
         ;; One oriented face plus one shape word reproduces the sixteen
         ;; reference positions bit for bit.
         (check (equalp reference realized)
                "shape-word realization of face ~S diverges" face)
         (push (list face shape realized) results)))
     surface)
    (nreverse results)))

(defun face-edge-decoded-displacement (face shape which w)
  (multiple-value-bind (eul euh evl evh) (unpack-shape-word shape)
    (decode-edge-displacement
     face which
     (ecase which (:u-low eul) (:u-high euh) (:v-low evl) (:v-high evh))
     w)))

(defun test-watertight-closure (cells)
  "The heart of watertightness: every incidence of one canonical edge or
vertex site decodes the same displacement, and every shared realized point
is bit-identical across the faces that contain it."
  (let* ((world (make-test-solid cells))
         (domain (solid-world-domain world))
         (occupancy (solid-world-occupancy world))
         (w 0.25d0)
         (realizations (collect-face-realizations world w))
         (edge-displacements (make-hash-table))
         (corner-codes (make-hash-table))
         (point-coordinates (make-hash-table :test #'equal)))
    (dolist (entry realizations)
      (destructuring-bind (face shape points) entry
        ;; Every incidence of a canonical edge decodes the same q and
        ;; displacement, from each face's own normal and tangent.
        (dolist (which '(:u-low :u-high :v-low :v-high))
          (let ((geometry (face-edge-geometry domain face which))
                (displacement
                  (multiple-value-list
                   (face-edge-decoded-displacement face shape which w))))
            ;; The decode must also match the generic classifier.
            (check (equal displacement
                          (multiple-value-list
                           (site-displacement geometry occupancy w)))
                   "edge decode vs generic classifier at ~S of ~S"
                   which face)
            (multiple-value-bind (stored present)
                (gethash geometry edge-displacements)
              (if present
                  (check (equal stored displacement)
                         "edge ~S decodes differently across faces" geometry)
                  (setf (gethash geometry edge-displacements)
                        displacement)))))
        ;; Every incidence of a canonical vertex decodes the same corner
        ;; code, which must also match the generic classifier.
        (multiple-value-bind (eul euh evl evh cll clh chl chh)
            (unpack-shape-word shape)
          (declare (ignore eul euh evl evh))
          (loop for (u-high-p v-high-p code) in (list (list nil nil cll)
                                                      (list nil t clh)
                                                      (list t nil chl)
                                                      (list t t chh))
                do (let ((geometry (face-corner-geometry
                                    domain face u-high-p v-high-p)))
                     (multiple-value-bind (qx qy qz num den)
                         (site-shape geometry occupancy)
                       (declare (ignore den))
                       (check (= code (corner-code qx qy qz (= num 2)))
                              "corner code vs generic classifier at ~S"
                              geometry))
                     (multiple-value-bind (stored present)
                         (gethash geometry corner-codes)
                       (if present
                           (check (= stored code)
                                  "vertex ~S coded differently" geometry)
                           (setf (gethash geometry corner-codes) code))))))
        ;; Shared realized coordinates are bit-identical across faces.
        ;; Key each boundary point by its canonical site and its position
        ;; along that site (corners have one position; each edge has two,
        ;; distinguished by the parameter index along the edge axis).
        (dotimes (i 4)
          (dotimes (j 4)
            (multiple-value-bind (role which u-high-p v-high-p)
                (grid-point-role i j)
              (let ((key
                      (ecase role
                        (:interior nil)
                        (:edge
                         (let* ((geometry (face-edge-geometry
                                           domain face which))
                                (parameter (if (member which '(:u-low :u-high))
                                               j i)))
                           ;; Parameter 1 is the point at w from the edge
                           ;; anchor, 2 the point at 1-w.
                           (list (site-geometry geometry) parameter)))
                        (:corner
                         (list (site-geometry
                                (face-corner-geometry
                                 domain face u-high-p v-high-p))
                               0)))))
                (when key
                  (let* ((base (* 3 (local-point-index i j)))
                         (coordinate (list (aref points base)
                                           (aref points (+ base 1))
                                           (aref points (+ base 2)))))
                    (multiple-value-bind (stored present)
                        (gethash key point-coordinates)
                      (if present
                          (check (and (eql (first stored)
                                           (first coordinate))
                                      (eql (second stored)
                                           (second coordinate))
                                      (eql (third stored)
                                           (third coordinate)))
                                 "point ~S realized differently: ~S vs ~S"
                                 key stored coordinate)
                          (setf (gethash key point-coordinates)
                                coordinate)))))))))))
    (values (hash-table-count edge-displacements)
            (hash-table-count corner-codes))))

(defun test-face-closure ()
  ;; An isolated cell: all edges convex, all corners unit-diagonal
  ;; two-thirds.
  (let* ((world (make-test-solid '((2 2 2))))
         (domain (solid-world-domain world))
         (occupancy (solid-world-occupancy world)))
    (map-chain
     (lambda (face)
       (multiple-value-bind (eul euh evl evh cll clh chl chh)
           (unpack-shape-word (face-shape-word domain face occupancy))
         (dolist (edge (list eul euh evl evh))
           (check (= edge +edge-convex+)))
         (dolist (corner (list cll clh chl chh))
           (check (logbitp +corner-two-thirds-bit+ corner)
                  "isolated-cell corner should take the centroid reach"))))
     (surface-chain (solid-chain world))))
  ;; Watertightness across representative solids: an isolated cell, a bar,
  ;; a step (mixing convex, balanced, and concave edges), and a plate with
  ;; a pit (concave-type vertices).
  (test-watertight-closure '((2 2 2)))
  (test-watertight-closure '((1 2 2) (2 2 2) (3 2 2)))
  (test-watertight-closure '((1 1 1) (2 1 1) (2 1 2)))
  (test-watertight-closure
   ;; A 3x3 plate with the centre cell of the top layer removed.
   (let ((cells '()))
     (dotimes (x 3)
       (dotimes (y 3)
         (push (list (1+ x) (1+ y) 1) cells)
         (unless (and (= x 1) (= y 1))
           (push (list (1+ x) (1+ y) 2) cells))))
     cells))
  ;; A face that is not exposed refuses classification.
  (let* ((world (make-test-solid '((2 2 2) (3 2 2))))
         (domain (solid-world-domain world))
         (buried (site-boundary-high
                  domain (make-site domain 2 2 2 +cell-extent+) :x)))
    (check (nth-value 1 (ignore-errors
                          (face-shape-word domain buried
                                           (solid-world-occupancy world)))))))

(defun test-face-records ()
  (let* ((world (make-test-solid '((1 1 1) (2 1 1))))
         (domain (solid-world-domain world)))
    (multiple-value-bind (records faces) (materialize-surface world 0.25d0
                                                              :stock 5)
      (check (= (length faces) 10))
      (check (= (length records) (* +face-record-words+ 10)))
      (loop for face across faces
            for index from 0
            do (multiple-value-bind (decorated shape reserved)
                   (unpack-face-record records (* +face-record-words+ index))
                 (check (zerop reserved))
                 (check (= 5 (decorated-site-stock decorated)))
                 ;; Masking the stock nibble recovers the topological site.
                 (check (= face (decorated-site-site decorated)))
                 (check (site-valid-p domain (decorated-site-site decorated)))
                 (unpack-shape-word shape))))))

(defun triangle-signed-area2 (indices offset)
  "Twice the signed area of one template triangle in (i,j) grid coordinates."
  (flet ((coordinates (index)
           (values (floor index 4) (mod index 4))))
    (multiple-value-bind (x0 y0) (coordinates (aref indices offset))
      (multiple-value-bind (x1 y1) (coordinates (aref indices (+ offset 1)))
        (multiple-value-bind (x2 y2) (coordinates (aref indices (+ offset 2)))
          (- (* (- x1 x0) (- y2 y0))
             (* (- x2 x0) (- y1 y0))))))))

(defun test-index-templates ()
  (dolist (entry (list (list +positive-face-indices+ 1)
                       (list +negative-face-indices+ -1)))
    (destructuring-bind (indices expected-sign) entry
      (check (= 54 (length indices)))
      (loop for index across indices
            do (check (<= 0 index 15)))
      ;; Consistent winding for all eighteen triangles.
      (loop for offset from 0 below 54 by 3
            do (check (= expected-sign
                         (signum (triangle-signed-area2 indices offset)))
                      "triangle at offset ~D winds the wrong way" offset))
      ;; The two triangles of every quad share the c00-c11 diagonal, cover
      ;; the quad exactly, and never repeat a corner.
      (loop for quad from 0 below 9
            do (let* ((offset (* 6 quad))
                      (first (list (aref indices offset)
                                   (aref indices (+ offset 1))
                                   (aref indices (+ offset 2))))
                      (second (list (aref indices (+ offset 3))
                                    (aref indices (+ offset 4))
                                    (aref indices (+ offset 5))))
                      (i (floor quad 3))
                      (j (mod quad 3))
                      (c00 (local-point-index i j))
                      (c10 (local-point-index (1+ i) j))
                      (c11 (local-point-index (1+ i) (1+ j)))
                      (c01 (local-point-index i (1+ j))))
                 (check (= 3 (length (remove-duplicates first))))
                 (check (= 3 (length (remove-duplicates second))))
                 ;; Shared diagonal c00-c11: both triangles contain it.
                 (check (and (member c00 first) (member c11 first)))
                 (check (and (member c00 second) (member c11 second)))
                 ;; Together the two triangles use exactly the quad's four
                 ;; corners, so they partition it without crossing or gaps.
                 (check (null (set-exclusive-or
                               (union first second)
                               (list c00 c10 c11 c01))))
                 ;; Areas: each triangle covers half the quad.
                 (check (= 1 (abs (triangle-signed-area2 indices offset))))
                 (check (= 1 (abs (triangle-signed-area2
                                   indices (+ offset 3))))))))))

(defun run-tests ()
  "Run every foundation test; return the number of checks passed."
  (let ((*test-count* 0))
    (test-site-round-trips)
    (test-boundary-and-cofaces)
    (test-chains)
    (test-strict-minority)
    (test-shape-word-packing)
    (test-solids)
    (test-face-closure)
    (test-face-records)
    (test-index-templates)
    (format t "~&All ~D checks passed.~%" *test-count*)
    *test-count*))
