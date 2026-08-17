;;; An atelier in a green field.
;;;
;;; We are weary of three-slot coordinate structures.  We want a more
;;; efficient and more thoughtful account of what those coordinates denote.
;;; Let this be LUFT, a complement to LUV.

(in-package #:luft)

;;; ------------------------------------------------------------------------
;;; Sites

;;; A site consists of a lattice-point anchor and an extent mask.  #KE4P5F
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
;;; The temporal extent bit says analogously that the site spans the current
;;; simulation interval [t,t+1].  The packed value deliberately contains no
;;; temporal coordinate: "now" is an ambient origin supplied by a simulation
;;; step, transaction, or history.  Temporal operations therefore return a
;;; secondary offset relative to that origin.  LUFT is not a four-dimensional
;;; worm space; it merely treats the futural time quantum as a fundamental
;;; extrusion.
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
;;;       4 bits  extent mask XYZT
;;;
;;; The extent mask lives in the low bits so testing, adding, and removing an
;;; axis is direct fixnum arithmetic.  On 64-bit SBCL, 60-bit unsigned values
;;; are immediate fixnums with two payload bits still unused.  Canonical
;;; incidence orientation does not need one of those bits: its sign follows
;;; from the ordered axes already present in the extent mask.  A future use for
;;; the spare bits should therefore be earned independently.

(deftype site ()
  '(unsigned-byte 60))

(deftype extent-mask ()
  '(unsigned-byte 4))

(deftype axis ()
  '(member :x :y :z :t))

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
(defconstant +temporal-extent+ #b1000)
(defconstant +spatial-extent+ #b0111)

(defconstant +extent-bits+ 4)
(defconstant +horizontal-capacity-bits+ 24)
(defconstant +vertical-coordinate-bits+ 8)
(defconstant +x-shift+ +extent-bits+)
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
    (:z #b0100)
    (:t #b1000)))

(defun make-extent (&rest axes)
  "Return the extent mask containing AXES."
  (reduce #'logior axes :key #'axis-bit :initial-value 0))

(declaim (inline site-extent site-x site-y site-z site-anchor))
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

(defun make-site (domain x y z &optional (extent +vertex-extent+))
  "Make DOMAIN's canonical site at lattice anchor X,Y,Z with EXTENT.

X and Y are reduced with DOMAIN's independent power-of-two masks.  Z must name
one of the 256 non-wrapping lattice planes.  A site extending along Z must
leave room for its high boundary and therefore cannot begin on the top plane."
  (check-type domain world-domain)
  (check-type x integer)
  (check-type y integer)
  (check-type z (integer 0 #.+top-z+))
  (check-type extent extent-mask)
  (when (and (= z +top-z+) (logbitp 2 extent))
    (error "A Z-extended site cannot be anchored on top plane ~D." +top-z+))
  (logior extent
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

(defun site-spatial-dimension (site)
  "Return SITE's dimension after ignoring temporal extent."
  (logcount (logand +spatial-extent+ (site-extent site))))

(defun site-dimension (site)
  "Return SITE's dimension, including temporal extent."
  (logcount (site-extent site)))

(defun site-with-extent (domain site extent)
  (make-site domain (site-x site) (site-y site) (site-z site) extent))

;;; ------------------------------------------------------------------------
;;; Adjacency

(defun step-site (domain site axis delta)
  "Translate DOMAIN's SITE along AXIS by DELTA.

Return the translated packed site and a temporal-origin offset.  Spatial
steps return zero as the second value.  Temporal steps leave the packed site
unchanged and return DELTA, because actual time belongs to the ambient
simulation history.  A spatial step outside the non-wrapping vertical world
returns NIL and zero."
  (checked-site domain site)
  (check-type delta integer)
  (ecase axis
    (:x (values (make-site domain (+ (site-x site) delta)
                           (site-y site) (site-z site) (site-extent site))
                0))
    (:y (values (make-site domain (site-x site)
                           (+ (site-y site) delta)
                           (site-z site) (site-extent site))
                0))
    (:z (let ((z (+ (site-z site) delta)))
          (if (or (< z 0)
                  (> z +top-z+)
                  (and (= z +top-z+)
                       (logbitp 2 (site-extent site))))
              (values nil 0)
              (values (make-site domain (site-x site) (site-y site)
                                 z (site-extent site))
                      0))))
    (:t (values site delta))))

(defun site-forward (domain site axis)
  "Move DOMAIN's SITE one unit forward along AXIS.

The second value is the change in ambient temporal origin; see STEP-SITE."
  (step-site domain site axis 1))

(defun site-backward (domain site axis)
  "Move DOMAIN's SITE one unit backward along AXIS.

The second value is the change in ambient temporal origin; see STEP-SITE."
  (step-site domain site axis -1))

;;; ------------------------------------------------------------------------
;;; Incidence

;;; Given a site (x,y,z, extent=XYZ) and its Z extent, removing that bit
;;; produces exactly two boundary sites:
;;;
;;;   low:   (x,y,z,   XY)
;;;   high:  (x,y,z+1, XY)
;;;
;;; For a temporal extent both packed boundaries have the same spatial site.
;;; Their secondary temporal offsets, zero and one, distinguish the boundary
;;; at now from the boundary one simulation quantum from now.

(defun require-site-extent (site axis present-p)
  (check-type site site)
  (let ((present (site-extends-p site axis)))
    (unless (eq present present-p)
      (error "Site ~S ~:[already extends~;does not extend~] along ~S."
             site present-p axis))))

(defun site-boundary-low (domain site axis)
  "Return DOMAIN's low boundary of SITE and its temporal-origin offset."
  (checked-site domain site)
  (require-site-extent site axis t)
  (values (site-with-extent domain site
                            (logandc2 (site-extent site) (axis-bit axis)))
          0))

(defun site-boundary-high (domain site axis)
  "Return DOMAIN's high boundary of SITE and its temporal-origin offset."
  (checked-site domain site)
  (require-site-extent site axis t)
  (site-forward domain
   (site-with-extent domain site
                     (logandc2 (site-extent site) (axis-bit axis)))
   axis))

(defun site-boundary-sign (site axis side)
  "Return the canonical oriented incidence coefficient, either -1 or +1.

Axes are ordered X,Y,Z,T.  The high boundary along axis i has sign (-1)^i
among the axes present in SITE; the low boundary has the opposite sign."
  (require-site-extent site axis t)
  (check-type side side)
  (let* ((bit (axis-bit axis))
         (earlier-axes (logand (site-extent site) (1- bit)))
         (high-sign (if (evenp (logcount earlier-axes)) 1 -1)))
    (if (eq side :high) high-sign (- high-sign))))

(defun map-site-boundary (function domain site)
  "Call FUNCTION for every oriented codimension-one boundary of DOMAIN's SITE.

FUNCTION receives BOUNDARY, SIGN, TEMPORAL-OFFSET, AXIS, and SIDE.  The
temporal offset is relative to the ambient origin of SITE."
  (checked-site domain site)
  (dolist (axis '(:x :y :z :t) site)
    (when (site-extends-p site axis)
      (dolist (side '(:low :high))
        (multiple-value-bind (boundary temporal-offset)
            (if (eq side :low)
                (site-boundary-low domain site axis)
                (site-boundary-high domain site axis))
          (funcall function boundary
                   (site-boundary-sign site axis side)
                   temporal-offset axis side))))))

;;; A boundary ordinarily has two cofaces along an axis.  The forward coface
;;; is anchored at the boundary itself, which is its low side.  The backward
;;; coface is anchored one unit earlier, making the original site its high
;;; side.  At the top or bottom of the vertical world one of these is absent.
;;; Along T, the packed coface is the same in both cases, while its temporal
;;; origin is respectively now or one tick in the past.

(defun site-coface-forward (domain site axis)
  "Return DOMAIN's coface extending forward from SITE along AXIS.

The second value is its temporal-origin offset.  NIL means that the coface
would extend above the vertical world."
  (checked-site domain site)
  (require-site-extent site axis nil)
  (let ((extent (logior (site-extent site) (axis-bit axis))))
    (if (and (eq axis :z) (= (site-z site) +top-z+))
        (values nil 0)
        (values (site-with-extent domain site extent) 0))))

(defun site-coface-backward (domain site axis)
  "Return DOMAIN's coface whose high AXIS boundary is SITE.

The second value is its temporal-origin offset.  NIL means that the coface
would extend below the vertical world."
  (checked-site domain site)
  (require-site-extent site axis nil)
  (multiple-value-bind (anchor temporal-offset)
      (site-backward domain site axis)
    (if anchor
        (values (site-with-extent domain
                 anchor (logior (site-extent anchor) (axis-bit axis)))
                temporal-offset)
        (values nil temporal-offset))))
