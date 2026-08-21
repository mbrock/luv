;;; An atelier in a green field.
;;;
;;; We are weary of three-slot coordinate structures.  We want a more
;;; efficient and more thoughtful account of what those coordinates denote.
;;; Let this be LUFT, a complement to LUV.

(in-package #:luft)

;;; ------------------------------------------------------------------------
;;; Sites

;;; A site consists of a lattice-point anchor, an extent mask, and a polarity.
;;; #KE4P5F
;;;
;;; Each spatial extent bit says that the site spans the unit interval
;;; forward from its anchor along that axis.  Thus:
;;;
;;;   no spatial extent      is a vertex,
;;;   one spatial extent     is an edge,
;;;   two                    is a face,
;;;   all three              is a voxel cell.
;;;
;;; This makes incidence canonical.  The high X boundary of the cell anchored
;;; at (x,y,z) is the very same YZ face as the low X boundary of the cell
;;; anchored at (x+1,y,z).  A face is not one of six decorations owned by a
;;; voxel; it is a site shared by its incident cells.
;;;
;;; The fourth low bit is polarity.  A positive and a negative site have the
;;; same anchor and extent but opposite orientations.  They are the geometric
;;; equivalent of a particle and antiparticle: when both reach a chain they
;;; annihilate.  Boundary operations put the polarity directly on each
;;; resulting face, edge, or vertex, so no separate coefficient travels beside
;;; the packed site.
;;;
;;; X is west/east and Y is south/north.  Each world chooses independently how
;;; many of the 24 available X and Y bits are significant.  Both active fields
;;; wrap, so a horizontal slice is a possibly rectangular torus.  The packed
;;; fields describe representational capacity; a WORLD-DOMAIN supplies the
;;; actual topology and its canonical coordinate masks.
;;;
;;; Z does not wrap.  Eight Z bits identify 256 horizontal lattice planes and
;;; therefore 255 unit intervals between them; a Z-extended site cannot be
;;; anchored on the top plane.
;;;
;;; A site occupies 60 bits:
;;;
;;;       8 bits  Z anchor
;;;      24 bits  Y anchor
;;;      24 bits  X anchor
;;;       1 bit   polarity (clear is positive, set is negative)
;;;       3 bits  extent mask XYZ
;;;
;;; The extent and polarity live in the low bits so their operations are direct
;;; fixnum arithmetic.  On 64-bit SBCL, 60-bit unsigned values are immediate
;;; fixnums with two payload bits still unused.

(deftype site ()
  '(unsigned-byte 60))

(deftype extent-mask ()
  '(unsigned-byte 3))

(deftype axis ()
  '(member :x :y :z))

(deftype side ()
  '(member :low :high))

(defconstant +vertex-extent+ #b0000)
(defconstant +x-edge-extent+ #b0001)
(defconstant +y-edge-extent+ #b0010)
(defconstant +z-edge-extent+ #b0100)
(defconstant +xy-face-extent+ #b0011)
(defconstant +xz-face-extent+ #b0101)
(defconstant +yz-face-extent+ #b0110)
(defconstant +cell-extent+ #b0111)

(defconstant +extent-bits+ 3)
(defconstant +site-sign-bit+ 3)
(defconstant +negative-site-mask+ (ash 1 +site-sign-bit+))
(defconstant +site-tag-bits+ 4)
(defconstant +horizontal-capacity-bits+ 24)
(defconstant +vertical-coordinate-bits+ 8)
(defconstant +x-shift+ +site-tag-bits+)
(defconstant +y-shift+ (+ +x-shift+ +horizontal-capacity-bits+))
(defconstant +z-shift+ (+ +y-shift+ +horizontal-capacity-bits+))
(defconstant +top-z+ (1- (ash 1 +vertical-coordinate-bits+)))

;;; A world domain realizes the topology admitted by the packed coordinate
;;; capacity.  Widths remain inspectable configuration; masks are the form
;;; consumed by construction and stepping.  Sites have canonical fixnum
;;; equality only inside one such domain.

(defstruct (world-domain
             (:constructor %make-world-domain
                 (x-bits y-bits x-mask y-mask)))
  (x-bits 24 :type (integer 1 24) :read-only t)
  (y-bits 24 :type (integer 1 24) :read-only t)
  (x-mask #xffffff :type (unsigned-byte 24) :read-only t)
  (y-mask #xffffff :type (unsigned-byte 24) :read-only t))

(defun make-world-domain
    (&key (horizontal-bits 24)
          (x-bits horizontal-bits)
          (y-bits horizontal-bits))
  "Make a toroidal world domain with power-of-two X and Y periods.

HORIZONTAL-BITS supplies the convenient common default.  X-BITS and Y-BITS
may override it independently.  Each width is between one and the 24-bit
capacity of the packed site ABI."
  (check-type horizontal-bits (integer 1 #.+horizontal-capacity-bits+))
  (check-type x-bits (integer 1 #.+horizontal-capacity-bits+))
  (check-type y-bits (integer 1 #.+horizontal-capacity-bits+))
  (%make-world-domain x-bits y-bits
                      (1- (ash 1 x-bits))
                      (1- (ash 1 y-bits))))

(declaim (inline world-domain-x-period world-domain-y-period))
(defun world-domain-x-period (domain)
  (check-type domain world-domain)
  (1+ (world-domain-x-mask domain)))

(defun world-domain-y-period (domain)
  (check-type domain world-domain)
  (1+ (world-domain-y-mask domain)))

(declaim (inline axis-bit))
(defun axis-bit (axis)
  (ecase axis
    (:x #b0001)
    (:y #b0010)
    (:z #b0100)))

(defun make-extent (&rest axes)
  "Return the extent mask containing AXES."
  (reduce #'logior axes :key #'axis-bit :initial-value 0))

(declaim (inline site-extent site-x site-y site-z site-anchor
                 site-negative-p site-positive-p site-polarity
                 site-geometry opposite-site))
(defun site-extent (site)
  (check-type site site)
  (ldb (byte +extent-bits+ 0) site))

(defun site-x (site)
  (check-type site site)
  (ldb (byte +horizontal-capacity-bits+ +x-shift+) site))

(defun site-y (site)
  (check-type site site)
  (ldb (byte +horizontal-capacity-bits+ +y-shift+) site))

(defun site-z (site)
  (check-type site site)
  (ldb (byte +vertical-coordinate-bits+ +z-shift+) site))

(defun site-anchor (site)
  "Return SITE's X, Y, and Z lattice coordinates as three values."
  (values (site-x site) (site-y site) (site-z site)))

(defun site-negative-p (site)
  "Whether SITE has negative polarity."
  (check-type site site)
  (logbitp +site-sign-bit+ site))

(defun site-positive-p (site)
  "Whether SITE has positive polarity."
  (not (site-negative-p site)))

(defun site-polarity (site)
  "Return SITE's polarity, either +1 or -1."
  (if (site-negative-p site) -1 1))

(defun site-geometry (site)
  "Return SITE's anchor and extent with positive polarity.

This is the common geometric identity shared by the positive and negative
versions of a site."
  (check-type site site)
  (logandc2 site +negative-site-mask+))

(defun opposite-site (site)
  "Return the same geometric site with the opposite polarity."
  (check-type site site)
  (logxor site +negative-site-mask+))

(defun site-with-polarity (site polarity)
  "Return SITE's geometry with POLARITY, either +1 or -1."
  (check-type site site)
  (check-type polarity (member 1 -1))
  (if (minusp polarity)
      (logior (site-geometry site) +negative-site-mask+)
      (site-geometry site)))

(defun site-valid-p (domain thing)
  "Whether THING is a canonical packed site in DOMAIN.

Inactive upper X and Y bits make an otherwise representable value
noncanonical for this domain."
  (check-type domain world-domain)
  (and (typep thing 'site)
       (let ((x (site-x thing))
             (y (site-y thing)))
         (and (= x (logand x (world-domain-x-mask domain)))
              (= y (logand y (world-domain-y-mask domain)))
              (not (and (= (site-z thing) +top-z+)
                        (logbitp 2 (site-extent thing))))))))

(defun make-site (domain x y z &optional (extent +vertex-extent+) (polarity 1))
  "Make DOMAIN's canonical site at X,Y,Z with EXTENT and POLARITY.

X and Y are reduced with DOMAIN's independent power-of-two masks.  Z must name
one of the 256 non-wrapping lattice planes.  A site extending along Z must
leave room for its high boundary and therefore cannot begin on the top plane.
POLARITY is +1 or -1; positive and negative sites share one geometry."
  (check-type domain world-domain)
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

(defun site-extends-p (site axis)
  "Whether SITE spans the unit interval along AXIS."
  (logtest (axis-bit axis) (site-extent site)))

(defun site-dimension (site)
  "Return SITE's dimension: zero for a vertex through three for a cell."
  (logcount (site-extent site)))

(defun site-with-extent (domain site extent)
  (make-site domain (site-x site) (site-y site) (site-z site)
             extent (site-polarity site)))

;;; ------------------------------------------------------------------------
;;; Adjacency

(defun step-site (domain site axis delta)
  "Translate DOMAIN's SITE along AXIS by DELTA.

The translated site keeps SITE's polarity.  A step outside the non-wrapping
vertical world returns NIL."
  (checked-site domain site)
  (check-type delta integer)
  (ecase axis
    (:x (make-site domain (+ (site-x site) delta)
                   (site-y site) (site-z site) (site-extent site)
                   (site-polarity site)))
    (:y (make-site domain (site-x site)
                   (+ (site-y site) delta)
                   (site-z site) (site-extent site)
                   (site-polarity site)))
    (:z (let ((z (+ (site-z site) delta)))
          (if (or (< z 0)
                  (> z +top-z+)
                  (and (= z +top-z+)
                       (logbitp 2 (site-extent site))))
              nil
              (make-site domain (site-x site) (site-y site)
                         z (site-extent site) (site-polarity site)))))))

(defun site-forward (domain site axis)
  "Move DOMAIN's SITE one unit forward along AXIS."
  (step-site domain site axis 1))

(defun site-backward (domain site axis)
  "Move DOMAIN's SITE one unit backward along AXIS."
  (step-site domain site axis -1))

;;; ------------------------------------------------------------------------
;;; Incidence

;;; Given a site (x,y,z, extent=XYZ) and its Z extent, removing that bit
;;; produces exactly two boundary sites:
;;;
;;;   low:   (x,y,z,   XY)
;;;   high:  (x,y,z+1, XY)
;;;
;;; The boundary inherits the site's polarity and may flip it according to the
;;; fixed orientation of the low or high side.  Thus the boundary of a
;;; negative site is exactly the opposite of the boundary of its positive
;;; counterpart.

(defun require-site-extent (site axis present-p)
  (check-type site site)
  (let ((present (site-extends-p site axis)))
    (unless (eq present present-p)
      (error "Site ~S ~:[already extends~;does not extend~] along ~S."
             site present-p axis))))

(defun site-boundary-low (domain site axis)
  "Return DOMAIN's oriented low AXIS boundary of SITE."
  (checked-site domain site)
  (require-site-extent site axis t)
  (site-with-polarity
   (site-with-extent domain site
                     (logandc2 (site-extent site) (axis-bit axis)))
   (site-boundary-polarity site axis :low)))

(defun site-boundary-high (domain site axis)
  "Return DOMAIN's oriented high AXIS boundary of SITE."
  (checked-site domain site)
  (require-site-extent site axis t)
  (site-with-polarity
   (site-forward domain
                 (site-with-extent
                  domain site
                  (logandc2 (site-extent site) (axis-bit axis)))
                 axis)
   (site-boundary-polarity site axis :high)))

(defun site-boundary-polarity (site axis side)
  "Return the polarity of SITE's low or high AXIS boundary.

Axes are ordered X,Y,Z.  The fixed orientation alternates as the present axes
are crossed; SITE's own polarity reverses the whole result."
  (require-site-extent site axis t)
  (check-type side side)
  (let* ((bit (axis-bit axis))
         (earlier-axes (logand (site-extent site) (1- bit)))
         (high-sign (if (evenp (logcount earlier-axes)) 1 -1)))
    (* (site-polarity site)
       (if (eq side :high) high-sign (- high-sign)))))

(defun map-site-boundary (function domain site)
  "Call FUNCTION for every oriented boundary piece of DOMAIN's SITE.

FUNCTION receives the signed BOUNDARY site, AXIS, and SIDE."
  (checked-site domain site)
  (dolist (axis '(:x :y :z) site)
    (when (site-extends-p site axis)
      (dolist (side '(:low :high))
        (funcall function
                 (if (eq side :low)
                     (site-boundary-low domain site axis)
                     (site-boundary-high domain site axis))
                 axis side)))))

;;; A boundary ordinarily has two cofaces along an axis.  The forward coface
;;; is anchored at the boundary itself, which is its low side.  The backward
;;; coface is anchored one unit earlier, making the original site its high
;;; side.  At the top or bottom of the vertical world one of these is absent.
;;; The coface receives whichever polarity makes the original site its exact
;;; signed boundary, so boundary and coface remain inverses.

(defun site-coface-forward (domain site axis)
  "Return DOMAIN's coface extending forward from SITE along AXIS.

NIL means that the coface would extend above the vertical world."
  (checked-site domain site)
  (require-site-extent site axis nil)
  (let ((extent (logior (site-extent site) (axis-bit axis))))
    (if (and (eq axis :z) (= (site-z site) +top-z+))
        nil
        (let ((coface (site-with-extent domain site extent)))
          (site-with-polarity
           coface
           (* (site-polarity site)
              (site-boundary-polarity
               (site-with-polarity coface 1) axis :low)))))))

(defun site-coface-backward (domain site axis)
  "Return DOMAIN's coface whose high AXIS boundary is SITE.

NIL means that the coface would extend below the vertical world."
  (checked-site domain site)
  (require-site-extent site axis nil)
  (let ((anchor (site-backward domain site axis)))
    (when anchor
      (let ((coface
              (site-with-extent
               domain anchor (logior (site-extent anchor) (axis-bit axis)))))
        (site-with-polarity
         coface
         (* (site-polarity site)
            (site-boundary-polarity
             (site-with-polarity coface 1) axis :high)))))))
