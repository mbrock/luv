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
   #:world-domain-x-limit #:world-domain-y-limit
   #:site #:extent-mask #:axis #:side
   #:+vertex-extent+ #:+x-edge-extent+ #:+y-edge-extent+ #:+z-edge-extent+
   #:+xy-face-extent+ #:+xz-face-extent+ #:+yz-face-extent+ #:+cell-extent+
   #:+extent-bits+ #:+site-sign-bit+ #:+site-tag-bits+
   #:+vertical-coordinate-bits+
   #:+z-shift+ #:+x-local-shift+ #:+y-local-shift+
   #:+chunk-bits+ #:+chunk-size+ #:+chunk-morton-shift+
   #:+site-mask+ #:+top-z+
   #:axis-index #:index-axis #:axis-bit #:make-extent
   #:make-site #:checked-site #:site-valid-p
   #:site-extent #:site-x #:site-y #:site-z #:site-anchor #:site-dimension
   #:site-extends-p #:site-negative-p #:site-positive-p #:site-polarity
   #:site-geometry #:opposite-site #:site-with-polarity
   #:step-site #:site-forward #:site-backward
   #:site-boundary-polarity #:site-boundary-low #:site-boundary-high
   #:map-site-boundary #:site-coface-forward #:site-coface-backward
   ;; Chunks.
   #:chunk-key #:site-chunk-key #:site-chunk-local #:chunk-key-at
   #:chunk-key-x #:chunk-key-y #:chunk-origin-x #:chunk-origin-y
   #:map-chain-chunks
   ;; Boundary conditions.
   #:outside-domain #:outside-domain-domain
   #:outside-domain-x #:outside-domain-y #:outside-domain-z
   #:outside-domain-occupancy #:treat-as-air #:treat-as-solid
   #:missing-chunk #:missing-chunk-domain #:missing-chunk-key #:use-chunk
   ;; Chains.
   #:chain #:chain-domain #:make-chain #:chain-count #:chain-empty-p
   #:chain-sites #:chain-site-count #:chain-site-p #:map-chain #:chain=
   #:chain-builder #:make-chain-builder #:chain-builder-add-site
   #:chain-builder-add-chain #:finish-chain-builder
   #:chain+ #:boundary-chain #:surface-chain #:chain-cell-occupancy-bit
   ;; Stars.
   #:cell-occupancy-bit #:site-star-occupancy-mask
   #:star-triangles #:star-face-triangles
   #:star-band-triangles #:star-junction-triangles
   #:star-rotations #:star-reflections #:transform-star
   #:transform-star-triangles #:star-orbit
   ;; Face topology.
   #:local-edge #:local-corner
   #:face-tangent-axes #:face-oriented-normal #:orient-face-outward
   #:face-edge-site #:face-corner-site
   ;; Sparse voxel light.
   #:+maximum-voxel-light-level+
   #:pack-voxel-light #:voxel-light-red #:voxel-light-green
   #:voxel-light-blue #:voxel-light-componentwise-max
   #:voxel-light-field #:voxel-light-field-domain
   #:voxel-light-field-revision #:voxel-light-field-page-count
   #:voxel-light-field-visits #:voxel-light-field-pushes
   #:voxel-light-field-stale-pops
   #:make-voxel-light-source #:solve-voxel-light
   #:voxel-light-at #:voxel-light-at-site
   #:voxel-light-at-lattice-point #:voxel-light-at-mesh-point
   ;; Manifold-sheet mesh.
   #:+mesh-cell-size+ #:+mesh-bevel-width+ #:+mesh-instance-word-count+
   #:+mesh-instance-stock-bit-count+
   #:+mesh-template-vertex-word-count+ #:+mesh-template-coordinate-bias+
   #:surface-mesh #:surface-mesh-domain #:surface-mesh-bevel-width
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
   #:surface-mesh-voxel-light #:surface-mesh-companions
   #:surface-mesh-attachments
   #:surface-attachment-frame #:surface-attachment-frame-origin
   #:surface-attachment-frame-normal #:surface-attachment-frame-tangent
   #:surface-attachment-frame-primitive-kinds
   #:surface-attachment-frame-stocks
   #:resolve-surface-attachment-frame
   #:star-singular-p #:decompose-star-mask
   #:compiled-chamfer-algebra #:make-compiled-chamfer-algebra
   #:width-one-material-source #:make-width-one-material-source
   #:map-chain-facts-cells-ranked
   #:with-surface-mesh-workspace #:make-surface-mesh #:mesh-chunk
   #:coplanar-compressed-surface-mesh
           #:select-surface-mesh-stocks
           #:surface-mesh-with-triangle-ink
           #:surface-mesh-split-neighborhood
           #:vary-surface-mesh-bevel-widths
           #:vary-surface-mesh-bevel-widths-from-stock-masks
           #:vary-uncontracted-surface-mesh-bevel-widths-diagnostic
           #:vary-uncontracted-surface-mesh-bevel-widths-from-stock-masks-diagnostic
           #:vary-surface-mesh-cohort-bevel-widths-from-stock-masks
   ;; Tests.
   #:run-luft-tests))

(in-package #:luft)

(declaim (optimize (speed 2) (safety 3) (debug 2)))

;;; ---------------------------------------------------------------------------
;;; Packed sites and domains
;;;
;;; A site packs, from the least significant bit up: the extent mask (3),
;;; the polarity sign (1), Z (8), the within-chunk X and Y (6 each), and the
;;; Morton-interleaved chunk coordinates (12+12).  Numeric order is therefore
;;; chunk-major with hierarchical (Morton) chunk locality, then column-major
;;; within a chunk: every (x, y) column's Z run is contiguous, and every
;;; power-of-two block of chunks is a contiguous range of any sorted vector.
;;;
;;; Coordinates do not wrap.  A domain is a box; probing beyond it is an
;;; explicit OUTSIDE-DOMAIN condition with boundary restarts, so the policy
;;; for what lies past an edge belongs to the caller (a chunk store, a whole
;;; world, a test harness), never to the coordinate arithmetic.

(deftype site () '(unsigned-byte 48))
(deftype extent-mask () '(unsigned-byte 3))
(deftype axis () '(member :x :y :z))
(deftype side () '(member :low :high))
(deftype local-edge () '(member :u-low :u-high :v-low :v-high))
(deftype local-corner () '(member :low-low :low-high :high-low :high-high))
(deftype chunk-key () '(unsigned-byte 24))

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
(defconstant +vertical-coordinate-bits+ 8)
(defconstant +chunk-bits+ 6)
(defconstant +chunk-size+ (ash 1 +chunk-bits+))
(defconstant +chunk-axis-bits+ 12)
(defconstant +z-shift+ +site-tag-bits+)
(defconstant +x-local-shift+ (+ +z-shift+ +vertical-coordinate-bits+))
(defconstant +y-local-shift+ (+ +x-local-shift+ +chunk-bits+))
(defconstant +chunk-morton-shift+ (+ +y-local-shift+ +chunk-bits+))
(defconstant +top-z+ (1- (ash 1 +vertical-coordinate-bits+)))
(defconstant +site-mask+ (1- (ash 1 48)))

(declaim (inline %spread-chunk-axis %compact-chunk-axis %chunk-morton))
(defun %spread-chunk-axis (value)
  "Spread a 12-bit chunk coordinate onto the even bit positions."
  (let ((v (logand value #xfff)))
    (setf v (logand (logior v (ash v 8)) #x00ff00ff)
          v (logand (logior v (ash v 4)) #x0f0f0f0f)
          v (logand (logior v (ash v 2)) #x33333333)
          v (logand (logior v (ash v 1)) #x55555555))
    v))

(defun %compact-chunk-axis (value)
  "Compact the even bit positions back into a 12-bit chunk coordinate."
  (let ((v (logand value #x555555)))
    (setf v (logand (logior v (ash v -1)) #x33333333)
          v (logand (logior v (ash v -2)) #x0f0f0f0f)
          v (logand (logior v (ash v -4)) #x00ff00ff)
          v (logand (logior v (ash v -8)) #x0000ffff))
    v))

(defun %chunk-morton (chunk-x chunk-y)
  (logior (%spread-chunk-axis chunk-x)
          (ash (%spread-chunk-axis chunk-y) 1)))

(defstruct (world-domain
             (:constructor %make-world-domain (x-bits y-bits))
             (:copier nil))
  (x-bits 6 :type (integer 1 17) :read-only t)
  (y-bits 6 :type (integer 1 17) :read-only t))

(defun make-world-domain (&key (horizontal-bits 6)
                               (x-bits horizontal-bits)
                               (y-bits horizontal-bits))
  "Make a boxed domain with power-of-two X and Y cell extents."
  (check-type x-bits (integer 1 17))
  (check-type y-bits (integer 1 17))
  (%make-world-domain x-bits y-bits))

(defun world-domain= (a b)
  (check-type a world-domain)
  (check-type b world-domain)
  (and (= (world-domain-x-bits a) (world-domain-x-bits b))
       (= (world-domain-y-bits a) (world-domain-y-bits b))))

(declaim (inline world-domain-x-limit world-domain-y-limit))
(defun world-domain-x-limit (domain)
  "The domain's cell count along X; anchors range over [0, limit]."
  (ash 1 (world-domain-x-bits domain)))
(defun world-domain-y-limit (domain)
  (ash 1 (world-domain-y-bits domain)))

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
                 site-positive-p site-polarity site-geometry opposite-site
                 site-chunk-key site-chunk-local))
(defun site-extent (site)
  (check-type site site)
  (ldb (byte +extent-bits+ 0) site))
(defun site-chunk-key (site)
  "The Morton-interleaved chunk coordinates of SITE."
  (check-type site site)
  (ldb (byte (* 2 +chunk-axis-bits+) +chunk-morton-shift+) site))
(defun site-chunk-local (site)
  "SITE with its chunk bits cleared: a valid site of the chunk-local box."
  (check-type site site)
  (ldb (byte +chunk-morton-shift+ 0) site))
(defun site-x (site)
  (check-type site site)
  (logior (ash (%compact-chunk-axis (site-chunk-key site)) +chunk-bits+)
          (ldb (byte +chunk-bits+ +x-local-shift+) site)))
(defun site-y (site)
  (check-type site site)
  (logior (ash (%compact-chunk-axis (ash (site-chunk-key site) -1))
               +chunk-bits+)
          (ldb (byte +chunk-bits+ +y-local-shift+) site)))
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
       (let ((extent (site-extent thing)))
         (and (<= (site-x thing)
                  (- (world-domain-x-limit domain)
                     (if (logbitp 0 extent) 1 0)))
              (<= (site-y thing)
                  (- (world-domain-y-limit domain)
                     (if (logbitp 1 extent) 1 0)))
              (not (and (= (site-z thing) +top-z+)
                        (logbitp 2 extent)))))))

(defun make-site (domain x y z &optional (extent +vertex-extent+) (polarity 1))
  "Pack a canonical site inside DOMAIN's box.  No coordinate wraps: anchors
range over [0, limit] per horizontal axis, and a site extending along an
axis cannot begin on that axis's far boundary."
  (check-type domain world-domain)
  (check-type x integer)
  (check-type y integer)
  (check-type z (integer 0 255))
  (check-type extent extent-mask)
  (check-type polarity (member 1 -1))
  (unless (and (<= 0 x (- (world-domain-x-limit domain)
                          (if (logbitp 0 extent) 1 0)))
               (<= 0 y (- (world-domain-y-limit domain)
                          (if (logbitp 1 extent) 1 0))))
    (error "Site anchor (~D ~D ~D) with extent ~3,'0B lies outside the ~
            ~Dx~D-cell domain."
           x y z extent
           (world-domain-x-limit domain) (world-domain-y-limit domain)))
  (when (and (= z +top-z+) (logbitp 2 extent))
    (error "A Z-extended site cannot begin on plane ~D." +top-z+))
  (logior extent
          (if (minusp polarity) +negative-site-mask+ 0)
          (ash z +z-shift+)
          (ash (ldb (byte +chunk-bits+ 0) x) +x-local-shift+)
          (ash (ldb (byte +chunk-bits+ 0) y) +y-local-shift+)
          (ash (%chunk-morton (ash x (- +chunk-bits+))
                              (ash y (- +chunk-bits+)))
               +chunk-morton-shift+)))

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
  "Translate SITE; return NIL when the step leaves DOMAIN's box."
  (checked-site domain site)
  (check-type delta integer)
  (let ((x (site-x site)) (y (site-y site)) (z (site-z site))
        (extent (site-extent site)))
    (ecase axis
      (:x (incf x delta))
      (:y (incf y delta))
      (:z (incf z delta)))
    (if (or (minusp x) (minusp y) (minusp z)
            (> x (- (world-domain-x-limit domain)
                    (if (logbitp 0 extent) 1 0)))
            (> y (- (world-domain-y-limit domain)
                    (if (logbitp 1 extent) 1 0)))
            (> z +top-z+)
            (and (= z +top-z+) (logbitp 2 extent)))
        nil
        (make-site domain x y z extent (site-polarity site)))))

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
  "Return the coface whose signed low boundary is SITE, or NIL at the box."
  (checked-site domain site)
  (%require-extent site axis nil)
  (when (or (and (eq axis :z) (= (site-z site) +top-z+))
            (and (eq axis :x)
                 (= (site-x site) (world-domain-x-limit domain)))
            (and (eq axis :y)
                 (= (site-y site) (world-domain-y-limit domain))))
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

(define-condition outside-domain (error)
  ((domain :initarg :domain :reader outside-domain-domain)
   (x :initarg :x :reader outside-domain-x)
   (y :initarg :y :reader outside-domain-y)
   (z :initarg :z :reader outside-domain-z))
  (:report (lambda (condition stream)
             (format stream "Cell (~D ~D ~D) lies outside the domain ~S."
                     (outside-domain-x condition)
                     (outside-domain-y condition)
                     (outside-domain-z condition)
                     (outside-domain-domain condition))))
  (:documentation
   "A cell probe left its box.  The signaling probe offers the boundary
restarts TREAT-AS-AIR, TREAT-AS-SOLID, and USE-VALUE, so the caller's
handler decides what lies past the edge: a world treats it as air, a chunk
store answers from the neighboring chunk or defers, a test refuses."))

(defun outside-domain-occupancy (domain x y z)
  "Signal OUTSIDE-DOMAIN for one probe, offering the boundary restarts."
  (restart-case (error 'outside-domain :domain domain :x x :y y :z z)
    (treat-as-air ()
      :report "Treat the missing cell as air."
      0)
    (treat-as-solid ()
      :report "Treat the missing cell as solid."
      1)
    (use-value (bit)
      :report "Supply the occupancy bit."
      bit)))

(define-condition missing-chunk (error)
  ((domain :initarg :domain :reader missing-chunk-domain)
   (key :initarg :key :reader missing-chunk-key))
  (:report (lambda (condition stream)
             (let ((key (missing-chunk-key condition)))
               (format stream "Chunk (~D ~D) [key ~D] is not resident."
                       (chunk-key-x key) (chunk-key-y key) key))))
  (:documentation
   "A probe crossed into a chunk that is not resident in the probing field.
The signaling probe offers USE-CHUNK (supply the chunk's chain), plus the
TREAT-AS-AIR and TREAT-AS-SOLID boundary restarts for the whole chunk, so a
streaming store answers with data, defers, or fills with a constant."))

(defun chain-cell-occupancy-bit (chain x y z)
  "Treat positive cubic-site occurrences in CHAIN as Boolean occupancy.
Cells above and below the Z range are air; probes beyond the horizontal
box signal OUTSIDE-DOMAIN with the boundary restarts."
  (let ((domain (chain-domain chain)))
    (cond ((or (< z 0) (>= z +top-z+)) 0)
          ((or (< x 0) (>= x (world-domain-x-limit domain))
               (< y 0) (>= y (world-domain-y-limit domain)))
           (outside-domain-occupancy domain x y z))
          ((chain-site-p chain (make-site domain x y z +cell-extent+ 1)) 1)
          (t 0))))

;;; ---------------------------------------------------------------------------
;;; Chunk vocabulary
;;;
;;; A chunk is the aligned 64x64-cell full-height column block named by the
;;; Morton-interleaved chunk coordinates in a site's top bits.  Because those
;;; bits are the most significant, a normalized chain is chunk-contiguous:
;;; each chunk, and each power-of-two block of chunks, is one contiguous run.

(defun chunk-key-at (x y)
  "The chunk key of the cell column at world coordinates X, Y."
  (%chunk-morton (ash x (- +chunk-bits+)) (ash y (- +chunk-bits+))))

(defun chunk-key-x (key)
  "The chunk-grid X coordinate of KEY."
  (%compact-chunk-axis key))
(defun chunk-key-y (key)
  (%compact-chunk-axis (ash key -1)))

(defun chunk-origin-x (key)
  "The world X coordinate of KEY's low corner."
  (ash (chunk-key-x key) +chunk-bits+))
(defun chunk-origin-y (key)
  (ash (chunk-key-y key) +chunk-bits+))

(defun map-chain-chunks (function chain)
  "Call FUNCTION with each (chunk-key chunk-chain) run of CHAIN in order."
  (let* ((sites (%chain-sites chain))
         (count (length sites))
         (start 0))
    (loop while (< start count) do
      (let ((key (site-chunk-key (aref sites start)))
            (end (1+ start)))
        (loop while (and (< end count)
                         (= key (site-chunk-key (aref sites end))))
              do (incf end))
        (let ((run (make-array (- end start)
                               :element-type '(unsigned-byte 64))))
          (replace run sites :start2 start :end2 end)
          (funcall function key (%make-chain (chain-domain chain) run)))
        (setf start end)))
    chain))

;;; ---------------------------------------------------------------------------
;;; Occupancy stars and strict-minority moment classification

(defun %occupancy-bit (value)
  (cond ((null value) 0)
        ((eq value t) 1)
        ((typep value 'bit) value)
        (t (error "Occupancy callback returned ~S, not NIL, T, 0, or 1."
                  value))))

(defun cell-occupancy-bit (domain occupancy x y z)
  "Central occupancy convention: cells outside Z=0..254 are air; probes
beyond the horizontal box signal OUTSIDE-DOMAIN with boundary restarts.
OCCUPANCY must return a stable NIL, T, 0, or 1 for each in-domain cell."
  (cond ((or (< z 0) (>= z +top-z+)) 0)
        ((or (< x 0) (>= x (world-domain-x-limit domain))
             (< y 0) (>= y (world-domain-y-limit domain)))
         (outside-domain-occupancy domain x y z))
        (t (%occupancy-bit (funcall occupancy x y z)))))

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
