;;;; LUFT -- canonical cubical topology
;;;;
;;;; Packed sites own topology and normalized vectors own chains.  Surface
;;;; realization lives in MESH.LISP; mutable occupancy and renderer backends
;;;; remain outside this package.

(defpackage #:luft
  (:use #:cl)
  (:export
   ;; Domains and sites.
   #:world-domain #:make-world-domain #:world-domain=
   #:world-domain-x-bits #:world-domain-y-bits
   #:world-domain-x-mask #:world-domain-y-mask
   #:world-domain-x-period #:world-domain-y-period
   #:site #:extent-mask #:axis #:side
   #:+vertex-extent+ #:+x-edge-extent+ #:+y-edge-extent+ #:+z-edge-extent+
   #:+xy-face-extent+ #:+xz-face-extent+ #:+yz-face-extent+ #:+cell-extent+
   #:+extent-bits+ #:+site-sign-bit+ #:+site-tag-bits+
   #:+horizontal-capacity-bits+ #:+vertical-coordinate-bits+
   #:+x-shift+ #:+y-shift+ #:+z-shift+ #:+site-mask+ #:+top-z+
   #:axis-index #:index-axis #:axis-bit #:make-extent
   #:make-site #:checked-site #:site-valid-p
   #:site-extent #:site-x #:site-y #:site-z #:site-anchor #:site-dimension
   #:site-extends-p #:site-negative-p #:site-positive-p #:site-polarity
   #:site-geometry #:opposite-site #:site-with-polarity
   #:step-site #:site-forward #:site-backward
   #:site-boundary-polarity #:site-boundary-low #:site-boundary-high
   #:map-site-boundary #:site-coface-forward #:site-coface-backward
   ;; Chains.
   #:chain #:chain-domain #:make-chain #:chain-count #:chain-empty-p
   #:chain-sites #:chain-site-count #:chain-site-p #:map-chain #:chain=
   #:chain-builder #:make-chain-builder #:chain-builder-add-site
   #:chain-builder-add-chain #:finish-chain-builder
   #:chain+ #:boundary-chain #:surface-chain #:chain-cell-occupancy-bit
   ;; Stars.
   #:cell-occupancy-bit #:site-star-occupancy-mask
   ;; Face topology.
   #:local-edge #:local-corner
   #:face-tangent-axes #:face-oriented-normal #:orient-face-outward
   #:face-edge-site #:face-corner-site
   ;; Manifold-sheet mesh.
   #:+mesh-cell-size+ #:+mesh-bevel-width+ #:+mesh-instance-word-count+
   #:+mesh-template-vertex-word-count+ #:+mesh-template-coordinate-bias+
   #:surface-mesh #:surface-mesh-domain
   #:surface-mesh-template-vertex-words #:surface-mesh-template-ranges
   #:surface-mesh-template-count
   #:surface-mesh-face-instance-words #:surface-mesh-face-instance-count
   #:surface-mesh-face-draws
   #:surface-mesh-band-instance-words #:surface-mesh-band-instance-count
   #:surface-mesh-band-draws
   #:surface-mesh-fan-instance-words #:surface-mesh-fan-instance-count
   #:surface-mesh-fan-draws
   #:surface-mesh-triangle-count #:surface-mesh-face-triangle-count
   #:surface-mesh-band-triangle-count #:surface-mesh-fan-triangle-count
   #:surface-mesh-singular-star-count
   #:star-singular-p #:decompose-star-mask #:make-surface-mesh
   ;; Tests.
   #:run-luft-tests))

(in-package #:luft)

(declaim (optimize (speed 2) (safety 3) (debug 2)))

;;; ---------------------------------------------------------------------------
;;; Packed sites and domains

(deftype site () '(unsigned-byte 60))
(deftype extent-mask () '(unsigned-byte 3))
(deftype axis () '(member :x :y :z))
(deftype side () '(member :low :high))
(deftype local-edge () '(member :u-low :u-high :v-low :v-high))
(deftype local-corner () '(member :low-low :low-high :high-low :high-high))

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
(defconstant +top-z+ (1- (ash 1 +vertical-coordinate-bits+)))
(defconstant +site-mask+ (1- (ash 1 60)))

(defstruct (world-domain
             (:constructor %make-world-domain (x-bits y-bits x-mask y-mask))
             (:copier nil))
  (x-bits 24 :type (integer 1 24) :read-only t)
  (y-bits 24 :type (integer 1 24) :read-only t)
  (x-mask #xffffff :type (unsigned-byte 24) :read-only t)
  (y-mask #xffffff :type (unsigned-byte 24) :read-only t))

(defun make-world-domain (&key (horizontal-bits 24)
                               (x-bits horizontal-bits)
                               (y-bits horizontal-bits))
  "Make a domain with independent power-of-two X and Y periods."
  (check-type horizontal-bits (integer 1 24))
  (check-type x-bits (integer 1 24))
  (check-type y-bits (integer 1 24))
  (%make-world-domain x-bits y-bits
                      (1- (ash 1 x-bits))
                      (1- (ash 1 y-bits))))

(defun world-domain= (a b)
  (check-type a world-domain)
  (check-type b world-domain)
  (and (= (world-domain-x-mask a) (world-domain-x-mask b))
       (= (world-domain-y-mask a) (world-domain-y-mask b))))

(declaim (inline world-domain-x-period world-domain-y-period))
(defun world-domain-x-period (domain)
  (1+ (world-domain-x-mask domain)))
(defun world-domain-y-period (domain)
  (1+ (world-domain-y-mask domain)))

(declaim (inline axis-index index-axis axis-bit))
(defun axis-index (axis)
  (ecase axis (:x 0) (:y 1) (:z 2)))
(defun index-axis (index)
  (ecase index (0 :x) (1 :y) (2 :z)))
(defun axis-bit (axis)
  (ash 1 (axis-index axis)))

(defun make-extent (&rest axes)
  (reduce #'logior axes :key #'axis-bit :initial-value 0))

(declaim (inline site-extent site-x site-y site-z site-negative-p
                 site-positive-p site-polarity site-geometry opposite-site))
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
  (values (site-x site) (site-y site) (site-z site)))
(defun site-negative-p (site)
  (check-type site site)
  (logbitp +site-sign-bit+ site))
(defun site-positive-p (site)
  (not (site-negative-p site)))
(defun site-polarity (site)
  (if (site-negative-p site) -1 1))
(defun site-geometry (site)
  (check-type site site)
  (logandc2 site +negative-site-mask+))
(defun opposite-site (site)
  (check-type site site)
  (logxor site +negative-site-mask+))

(defun site-with-polarity (site polarity)
  (check-type site site)
  (check-type polarity (member 1 -1))
  (if (minusp polarity)
      (logior (site-geometry site) +negative-site-mask+)
      (site-geometry site)))

(defun site-valid-p (domain thing)
  (check-type domain world-domain)
  (and (typep thing 'site)
       (= thing (logand thing +site-mask+))
       (= (site-x thing)
          (logand (site-x thing) (world-domain-x-mask domain)))
       (= (site-y thing)
          (logand (site-y thing) (world-domain-y-mask domain)))
       (not (and (= (site-z thing) +top-z+)
                 (logbitp 2 (site-extent thing))))))

(defun make-site (domain x y z &optional (extent +vertex-extent+) (polarity 1))
  "Pack a canonical site.  X/Y wrap; Z and Z extent do not."
  (check-type domain world-domain)
  (check-type x integer)
  (check-type y integer)
  (check-type z (integer 0 255))
  (check-type extent extent-mask)
  (check-type polarity (member 1 -1))
  (when (and (= z +top-z+) (logbitp 2 extent))
    (error "A Z-extended site cannot begin on plane ~D." +top-z+))
  (logior extent
          (if (minusp polarity) +negative-site-mask+ 0)
          (ash (logand x (world-domain-x-mask domain)) +x-shift+)
          (ash (logand y (world-domain-y-mask domain)) +y-shift+)
          (ash z +z-shift+)))

(defun checked-site (domain site)
  (unless (site-valid-p domain site)
    (error "~S is not canonical in ~S." site domain))
  site)

(defun site-extends-p (site axis)
  (logbitp (axis-index axis) (site-extent site)))
(defun site-dimension (site)
  (logcount (site-extent site)))

(defun %site-with-extent (domain site extent)
  (make-site domain (site-x site) (site-y site) (site-z site)
             extent (site-polarity site)))

(defun step-site (domain site axis delta)
  "Translate SITE; return NIL only when a Z step leaves the valid domain."
  (checked-site domain site)
  (check-type delta integer)
  (ecase axis
    (:x (make-site domain (+ (site-x site) delta)
                   (site-y site) (site-z site)
                   (site-extent site) (site-polarity site)))
    (:y (make-site domain (site-x site)
                   (+ (site-y site) delta) (site-z site)
                   (site-extent site) (site-polarity site)))
    (:z (let ((z (+ (site-z site) delta)))
          (if (or (< z 0) (> z +top-z+)
                  (and (= z +top-z+)
                       (logbitp 2 (site-extent site))))
              nil
              (make-site domain (site-x site) (site-y site) z
                         (site-extent site) (site-polarity site)))))))

(declaim (inline site-forward site-backward))
(defun site-forward (domain site axis) (step-site domain site axis 1))
(defun site-backward (domain site axis) (step-site domain site axis -1))

(defun %require-extent (site axis present-p)
  (unless (eq (site-extends-p site axis) present-p)
    (error "Site ~S has the wrong extent status along ~S." site axis)))

(defun %boundary-incidence-sign (extent axis side)
  (let* ((bit (axis-bit axis))
         (earlier (logand extent (1- bit)))
         (high (if (evenp (logcount earlier)) 1 -1)))
    (if (eq side :high) high (- high))))

(defun site-boundary-polarity (site axis side)
  (%require-extent site axis t)
  (* (site-polarity site)
     (%boundary-incidence-sign (site-extent site) axis side)))

(defun site-boundary-low (domain site axis)
  (checked-site domain site)
  (%require-extent site axis t)
  (site-with-polarity
   (%site-with-extent domain site
                      (logandc2 (site-extent site) (axis-bit axis)))
   (site-boundary-polarity site axis :low)))

(defun site-boundary-high (domain site axis)
  (checked-site domain site)
  (%require-extent site axis t)
  (let* ((part (%site-with-extent
                domain site (logandc2 (site-extent site) (axis-bit axis))))
         (moved (site-forward domain part axis)))
    (unless moved (error "Missing high boundary of valid site ~S." site))
    (site-with-polarity moved (site-boundary-polarity site axis :high))))

(defun map-site-boundary (function domain site)
  (checked-site domain site)
  (dotimes (axis-number 3 site)
    (let ((axis (index-axis axis-number)))
      (when (site-extends-p site axis)
        (funcall function (site-boundary-low domain site axis) axis :low)
        (funcall function (site-boundary-high domain site axis) axis :high)))))

(defun site-coface-forward (domain site axis)
  "Return the coface whose signed low boundary is SITE, or NIL above Z."
  (checked-site domain site)
  (%require-extent site axis nil)
  (when (and (eq axis :z) (= (site-z site) +top-z+))
    (return-from site-coface-forward nil))
  (let* ((extent (logior (site-extent site) (axis-bit axis)))
         (geometry (%site-with-extent domain (site-geometry site) extent)))
    (site-with-polarity
     geometry
     (* (site-polarity site)
        (%boundary-incidence-sign extent axis :low)))))

(defun site-coface-backward (domain site axis)
  "Return the coface whose signed high boundary is SITE, or NIL below Z."
  (checked-site domain site)
  (%require-extent site axis nil)
  (let ((anchor (site-backward domain (site-geometry site) axis)))
    (when anchor
      (let* ((extent (logior (site-extent site) (axis-bit axis)))
             (geometry (%site-with-extent domain anchor extent)))
        (site-with-polarity
         geometry
         (* (site-polarity site)
            (%boundary-incidence-sign extent axis :high)))))))

;;; ---------------------------------------------------------------------------
;;; Normalized immutable chains

(deftype site-vector () '(simple-array (unsigned-byte 64) (*)))

(defstruct (chain
             (:constructor %make-chain (domain sites))
             (:conc-name %chain-)
             (:copier nil))
  (domain nil :type world-domain :read-only t)
  ;; UB60 site values live in an unboxed UB64 machine-word array on SBCL.
  (sites (make-array 0 :element-type '(unsigned-byte 64))
         :type site-vector :read-only t))

(defun chain-domain (chain) (%chain-domain chain))
(defun make-chain (domain)
  (%make-chain domain (make-array 0 :element-type '(unsigned-byte 64))))
(defun chain-count (chain) (length (%chain-sites chain)))
(defun chain-empty-p (chain) (zerop (chain-count chain)))

(defun chain-sites (chain)
  "Return a fresh copy; the chain's stored normalized vector stays immutable."
  (let* ((source (%chain-sites chain))
         (copy (make-array (length source) :element-type '(unsigned-byte 64))))
    (replace copy source)
    copy))

(defun %site-order< (a b)
  (let ((ga (site-geometry a)) (gb (site-geometry b)))
    (or (< ga gb)
        (and (= ga gb) (site-positive-p a) (site-negative-p b)))))

(defstruct (chain-builder
             (:constructor %make-chain-builder (domain buffer))
             (:conc-name %builder-)
             (:copier nil))
  (domain nil :type world-domain :read-only t)
  (buffer (make-array 0 :element-type '(unsigned-byte 64)
                        :adjustable t :fill-pointer 0)
          :type (vector (unsigned-byte 64))))

(defun make-chain-builder (domain &key (initial-capacity 0))
  (check-type domain world-domain)
  (check-type initial-capacity (integer 0 *))
  (%make-chain-builder
   domain
   (make-array initial-capacity :element-type '(unsigned-byte 64)
                                :adjustable t :fill-pointer 0)))

(defun chain-builder-add-site (builder site)
  (checked-site (%builder-domain builder) site)
  (vector-push-extend site (%builder-buffer builder))
  site)

(defun chain-builder-add-chain (builder chain)
  (unless (world-domain= (%builder-domain builder) (chain-domain chain))
    (error "Cannot combine chains over different domains."))
  (loop for site across (%chain-sites chain)
        do (vector-push-extend site (%builder-buffer builder)))
  builder)

(defun %normalize-vector (domain sites)
  (setf sites (sort sites #'%site-order<))
  (let ((read 0) (write 0) (n (length sites)))
    (loop while (< read n) do
      (let ((geometry (site-geometry (aref sites read)))
            (positive 0) (negative 0))
        (loop while (and (< read n)
                         (= geometry (site-geometry (aref sites read))))
              do (if (site-negative-p (aref sites read))
                     (incf negative)
                     (incf positive))
                 (incf read))
        (let ((net (- positive negative)))
          (unless (zerop net)
            (let ((site (site-with-polarity geometry (if (plusp net) 1 -1))))
              (dotimes (i (abs net))
                (declare (ignore i))
                (setf (aref sites write) site)
                (incf write)))))))
    (let ((result (make-array write :element-type '(unsigned-byte 64))))
      (replace result sites :end2 write)
      (%make-chain domain result))))

(defun finish-chain-builder (builder)
  (let* ((buffer (%builder-buffer builder))
         (sites (make-array (length buffer) :element-type '(unsigned-byte 64))))
    (replace sites buffer)
    (%normalize-vector (%builder-domain builder) sites)))

(defun %run-end (sites start)
  (let ((geometry (site-geometry (aref sites start)))
        (i (1+ start)) (n (length sites)))
    (loop while (and (< i n)
                     (= geometry (site-geometry (aref sites i))))
          do (incf i))
    i))

(defun %copy-run (source start end destination write)
  (replace destination source :start1 write :start2 start :end2 end)
  (+ write (- end start)))

(defun chain+ (a b)
  "Linear merge of two normalized chains, including run cancellation."
  (unless (world-domain= (chain-domain a) (chain-domain b))
    (error "Cannot add chains over different domains."))
  (let* ((av (%chain-sites a)) (bv (%chain-sites b))
         (an (length av)) (bn (length bv))
         (out (make-array (+ an bn) :element-type '(unsigned-byte 64)))
         (ai 0) (bi 0) (write 0))
    (loop while (or (< ai an) (< bi bn)) do
      (cond
        ((= ai an)
         (setf write (%copy-run bv bi bn out write) bi bn))
        ((= bi bn)
         (setf write (%copy-run av ai an out write) ai an))
        (t
         (let ((ag (site-geometry (aref av ai)))
               (bg (site-geometry (aref bv bi))))
           (cond
             ((< ag bg)
              (let ((end (%run-end av ai)))
                (setf write (%copy-run av ai end out write) ai end)))
             ((> ag bg)
              (let ((end (%run-end bv bi)))
                (setf write (%copy-run bv bi end out write) bi end)))
             (t
              (let* ((ae (%run-end av ai)) (be (%run-end bv bi))
                     (net (+ (* (site-polarity (aref av ai)) (- ae ai))
                             (* (site-polarity (aref bv bi)) (- be bi)))))
                (unless (zerop net)
                  (let ((site (site-with-polarity ag (if (plusp net) 1 -1))))
                    (dotimes (i (abs net))
                      (declare (ignore i))
                      (setf (aref out write) site)
                      (incf write))))
                (setf ai ae bi be))))))))
    (let ((result (make-array write :element-type '(unsigned-byte 64))))
      (replace result out :end2 write)
      (%make-chain (chain-domain a) result))))

(defun %lower-bound-geometry (sites geometry)
  (let ((lo 0) (hi (length sites)))
    (loop while (< lo hi) do
      (let* ((mid (floor (+ lo hi) 2))
             (g (site-geometry (aref sites mid))))
        (if (< g geometry) (setf lo (1+ mid)) (setf hi mid))))
    lo))

(defun chain-site-count (chain site)
  (checked-site (chain-domain chain) site)
  (let* ((sites (%chain-sites chain))
         (geometry (site-geometry site))
         (start (%lower-bound-geometry sites geometry)))
    (if (or (= start (length sites))
            (/= geometry (site-geometry (aref sites start)))
            (/= (site-polarity site) (site-polarity (aref sites start))))
        0
        (- (%run-end sites start) start))))

(defun chain-site-p (chain site) (plusp (chain-site-count chain site)))
(defun map-chain (function chain)
  (loop for site across (%chain-sites chain) do (funcall function site))
  chain)

(defun chain= (a b)
  (and (typep a 'chain) (typep b 'chain)
       (world-domain= (chain-domain a) (chain-domain b))
       (let ((av (%chain-sites a)) (bv (%chain-sites b)))
         (and (= (length av) (length bv))
              (loop for i below (length av)
                    always (= (aref av i) (aref bv i)))))))

(defun boundary-chain (chain)
  (let* ((domain (chain-domain chain))
         (builder (make-chain-builder
                   domain :initial-capacity (* 6 (chain-count chain)))))
    (map-chain
     (lambda (site)
       (map-site-boundary
        (lambda (part axis side)
          (declare (ignore axis side))
          (chain-builder-add-site builder part))
        domain site))
     chain)
    (finish-chain-builder builder)))

(defun surface-chain (solid-chain)
  "Return the normalized boundary of an ordinary solid three-chain."
  (boundary-chain solid-chain))

(defun chain-cell-occupancy-bit (chain x y z)
  "Treat positive cubic-site occurrences in CHAIN as Boolean occupancy."
  (if (or (< z 0) (>= z +top-z+))
      0
      (if (chain-site-p
           chain (make-site (chain-domain chain) x y z +cell-extent+ 1))
          1 0)))

;;; ---------------------------------------------------------------------------
;;; Occupancy stars and strict-minority moment classification

(defun %occupancy-bit (value)
  (cond ((null value) 0)
        ((eq value t) 1)
        ((typep value 'bit) value)
        (t (error "Occupancy callback returned ~S, not NIL, T, 0, or 1."
                  value))))

(defun cell-occupancy-bit (domain occupancy x y z)
  "Central occupancy convention: wrap X/Y; cells outside Z=0..254 are air.
OCCUPANCY must return a stable NIL, T, 0, or 1 for each canonical cell."
  (if (or (< z 0) (>= z +top-z+))
      0
      (%occupancy-bit
       (funcall occupancy
                (logand x (world-domain-x-mask domain))
                (logand y (world-domain-y-mask domain))
                z))))

(defun site-star-occupancy-mask (domain site occupancy)
  "Pack SITE's complete incident-cell star.

Absent axes are enumerated in X,Y,Z order.  Zero means the cell one unit below
the site on that axis (direction -1); one means the cell anchored at the site
(direction +1)."
  (checked-site domain site)
  (let* ((normal-mask (logandc2 +cell-extent+ (site-extent site)))
         (rank (logcount normal-mask))
         (samples (ash 1 rank))
         (mask 0))
    (dotimes (sample samples mask)
      (let ((x (site-x site)) (y (site-y site)) (z (site-z site))
            (normal-position 0))
        (dotimes (axis-number 3)
          (when (logbitp axis-number normal-mask)
            (unless (logbitp normal-position sample)
              (ecase axis-number
                (0 (decf x)) (1 (decf y)) (2 (decf z))))
            (incf normal-position)))
        (when (= 1 (cell-occupancy-bit domain occupancy x y z))
          (setf mask (logior mask (ash 1 sample))))))))

;;; ---------------------------------------------------------------------------
;;; Canonical face-local topology

(defun %require-face (domain face)
  (checked-site domain face)
  (unless (= (site-dimension face) 2)
    (error "Expected an oriented face site, not ~S." face))
  face)

(defun %face-tangent-indices (face)
  (unless (= (site-dimension face) 2)
    (error "Expected a face site, not ~S." face))
  (let ((u nil) (v nil))
    (dotimes (axis-number 3)
      (when (logbitp axis-number (site-extent face))
        (if u (setf v axis-number) (setf u axis-number))))
    (values u v)))

(defun face-tangent-axes (face)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (values (index-axis u) (index-axis v))))

(defun %canonical-face-normal (face)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (cond ((and (= u 0) (= v 1)) (values 0 0 1))
          ((and (= u 0) (= v 2)) (values 0 -1 0))
          ((and (= u 1) (= v 2)) (values 1 0 0))
          (t (error "Invalid face extent ~3,'0B." (site-extent face))))))

(defun face-oriented-normal (face)
  (multiple-value-bind (nx ny nz) (%canonical-face-normal face)
    (let ((p (site-polarity face)))
      (values (* p nx) (* p ny) (* p nz)))))

(defun %axis-offset (axis-number amount)
  (ecase axis-number
    (0 (values amount 0 0))
    (1 (values 0 amount 0))
    (2 (values 0 0 amount))))

(defun %site-at-offset (domain source dx dy dz extent)
  (make-site domain (+ (site-x source) dx)
                    (+ (site-y source) dy)
                    (+ (site-z source) dz)
                    extent 1))

(defun face-edge-site (domain face edge)
  "Return the canonical positive edge geometry at FACE's local EDGE."
  (%require-face domain face)
  (check-type edge local-edge)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (ecase edge
      (:u-low (%site-at-offset domain face 0 0 0 (ash 1 v)))
      (:u-high
       (multiple-value-bind (dx dy dz) (%axis-offset u 1)
         (%site-at-offset domain face dx dy dz (ash 1 v))))
      (:v-low (%site-at-offset domain face 0 0 0 (ash 1 u)))
      (:v-high
       (multiple-value-bind (dx dy dz) (%axis-offset v 1)
         (%site-at-offset domain face dx dy dz (ash 1 u)))))))

(defun face-corner-site (domain face corner)
  "Return the canonical positive vertex at local CORNER (U word first)."
  (%require-face domain face)
  (check-type corner local-corner)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (let ((du (if (member corner '(:high-low :high-high)) 1 0))
          (dv (if (member corner '(:low-high :high-high)) 1 0)))
      (multiple-value-bind (ux uy uz) (%axis-offset u du)
        (multiple-value-bind (vx vy vz) (%axis-offset v dv)
          (%site-at-offset domain face (+ ux vx) (+ uy vy) (+ uz vz)
                           +vertex-extent+))))))

(defun %face-normal-side-occupancy (domain face occupancy direction)
  ;; DIRECTION is +/-1 relative to the oriented normal.
  (multiple-value-bind (nx ny nz) (face-oriented-normal face)
    (let ((dx (* direction nx))
          (dy (* direction ny))
          (dz (* direction nz)))
      (cell-occupancy-bit
       domain occupancy
       (+ (site-x face) (if (minusp dx) -1 0))
       (+ (site-y face) (if (minusp dy) -1 0))
       (+ (site-z face) (if (minusp dz) -1 0))))))

(defun orient-face-outward (domain geometric-face occupancy)
  "Orient a face from solid to air; return NIL when it is not exposed."
  (%require-face domain geometric-face)
  (let* ((positive (site-with-polarity geometric-face 1))
         (minus (%face-normal-side-occupancy domain positive occupancy -1))
         (plus (%face-normal-side-occupancy domain positive occupancy 1)))
    (cond ((and (= minus 1) (= plus 0)) positive)
          ((and (= minus 0) (= plus 1)) (opposite-site positive))
          (t nil))))
