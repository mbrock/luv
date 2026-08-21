;;;; LUFT -- canonical cubical topology and compact face realization
;;;;
;;;; A single executable specification.  Packed sites own topology; normalized
;;;; vectors own chains; site-local kernels own chamfer meaning; four-u32 face
;;;; records are the renderer ABI.  Mutable occupancy and renderer backends are
;;;; deliberately outside this file.
;;;;
;;;; Load this file and call (LUFT:RUN-LUFT-TESTS).

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
   ;; Stars and chamfer classification.
   #:cell-occupancy-bit #:site-star-occupancy-mask
   #:classify-star-mask #:classify-site-star #:site-displacement
   ;; Face topology and shape words.
   #:local-edge #:local-corner
   #:+edge-balanced+ #:+edge-convex+ #:+edge-concave+
   #:+shape-corner-field-bits+
   #:+shape-low-low-corner-shift+ #:+shape-low-high-corner-shift+
   #:+shape-high-low-corner-shift+ #:+shape-high-high-corner-shift+
   #:+corner-direction-mask+ #:+corner-two-thirds-mask+
   #:face-tangent-axes #:face-oriented-normal #:orient-face-outward
   #:face-edge-site #:face-corner-site
   #:vertex-star-half-edge-mask #:star-miter-arc-p
   #:encode-corner-code #:decode-corner-code
   #:pack-shape-word #:unpack-shape-word #:shape-word-valid-p
   #:shape-edge-code #:shape-corner-code #:shape-corner-star-mask
   #:decode-face-edge-direction #:face-shape-word
   ;; Reference realization.
   #:+face-point-count+ #:local-point-index #:local-corner-half-point-index
   #:realize-face-point #:realize-face-local-point #:realize-face-patch
   ;; Face-record ABI.
   #:+face-record-word-count+ #:+face-record-byte-size+
   #:+face-record-site-low-word+ #:+face-record-site-high-word+
   #:+face-record-shape-word+ #:+face-record-construction-word+
   #:+decorated-site-stock-shift+
   #:decorate-site #:undecorated-site #:decorated-site-stock
   #:make-face-record-array #:store-face-record #:load-face-record
   #:make-face-record
   ;; Raster templates.
   #:+face-index-count+ #:make-face-index-template #:face-construction-mask
   #:quad-diagonal-toward-corner
   #:positive-face-index-template #:negative-face-index-template
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

(defun %star-sample-direction (normal-mask sample)
  (let ((dx 0) (dy 0) (dz 0) (position 0))
    (dotimes (axis-number 3)
      (when (logbitp axis-number normal-mask)
        (let ((component (if (logbitp position sample) 1 -1)))
          (ecase axis-number
            (0 (setf dx component))
            (1 (setf dy component))
            (2 (setf dz component))))
        (incf position)))
    (values dx dy dz)))

(defun classify-star-mask (normal-mask occupancy-mask)
  "Return MX,MY,MZ,QX,QY,QZ,REACH,K for an edge or vertex star mask."
  (check-type normal-mask extent-mask)
  (check-type occupancy-mask (integer 0 *))
  (let* ((rank (logcount normal-mask))
         (sample-count (ash 1 rank)))
    (unless (member rank '(2 3))
      (error "Chamfer classification requires rank two or three, not ~S."
             normal-mask))
    (unless (< occupancy-mask (ash 1 sample-count))
      (error "Occupancy mask ~S exceeds a ~D-cell star."
             occupancy-mask sample-count))
    (let* ((k (logcount occupancy-mask))
           (balanced (= (* 2 k) sample-count))
           (minority-solid-p (< (* 2 k) sample-count))
           (mx 0) (my 0) (mz 0))
      (unless balanced
        (dotimes (sample sample-count)
          (let ((solid-p (logbitp sample occupancy-mask)))
            (when (if minority-solid-p solid-p (not solid-p))
              (multiple-value-bind (dx dy dz)
                  (%star-sample-direction normal-mask sample)
                (incf mx dx) (incf my dy) (incf mz dz))))))
      (let ((qx (signum mx)) (qy (signum my)) (qz (signum mz)))
        (values mx my mz qx qy qz
                (if (and (= rank 3)
                         (= (abs mx) 1) (= (abs my) 1) (= (abs mz) 1))
                    2/3
                    1/2)
                k)))))

(defun classify-site-star (domain site occupancy)
  "Enumerate SITE and return MX,MY,MZ,QX,QY,QZ,REACH,K,OCCUPANCY-MASK."
  (checked-site domain site)
  (unless (member (site-dimension site) '(0 1))
    (error "Only edges and vertices are chamfer-classified: ~S." site))
  (let* ((normal-mask (logandc2 +cell-extent+ (site-extent site)))
         (mask (site-star-occupancy-mask domain site occupancy)))
    (multiple-value-bind (mx my mz qx qy qz reach k)
        (classify-star-mask normal-mask mask)
      (values mx my mz qx qy qz reach k mask))))

(defun vertex-star-half-edge-mask (occupancy-mask axis direction)
  "Project a vertex OCCUPANCY-MASK onto one incident half-edge star.

DIRECTION is -1 or +1 along AXIS.  The four retained samples are repacked in
the canonical order of the other two axes, so CLASSIFY-STAR-MASK can consume
the result directly.  Complementing the vertex mask complements this mask."
  (check-type occupancy-mask (unsigned-byte 8))
  (check-type axis axis)
  (check-type direction (member -1 1))
  (let ((axis-number (axis-index axis))
        (result 0))
    (dotimes (sample 8 result)
      (when (eq (logbitp axis-number sample) (plusp direction))
        (let ((projected 0)
              (position 0))
          (dotimes (other-axis 3)
            (unless (= other-axis axis-number)
              (when (logbitp other-axis sample)
                (setf projected (logior projected (ash 1 position))))
              (incf position)))
          (when (logbitp sample occupancy-mask)
            (setf result (logior result (ash 1 projected)))))))))

(defun %miter-dominant-axis (mx my mz k)
  "Return the dominant axis of a planar three-cell minority L, or NIL."
  (when (= (min k (- 8 k)) 3)
    (let ((ax (abs mx)) (ay (abs my)) (az (abs mz)))
      (cond ((and (= ax 3) (= ay 1) (= az 1)) 0)
            ((and (= ax 1) (= ay 3) (= az 1)) 1)
            ((and (= ax 1) (= ay 1) (= az 3)) 2)))))

(defun star-miter-arc-p (occupancy-mask)
  "Whether OCCUPANCY-MASK is the complement-symmetric planar L miter star.

Three strict-minority cells in one octant layer leave one missing quadrant in
that layer.  Its two constant-width chamfer strips form exactly Blender's
outer-miter situation: their sharp offset lines meet, but a round join has two
endpoints and an arc between them."
  (check-type occupancy-mask (unsigned-byte 8))
  (multiple-value-bind (mx my mz qx qy qz reach k)
      (classify-star-mask #b111 occupancy-mask)
    (declare (ignore qx qy qz reach))
    (not (null (%miter-dominant-axis mx my mz k)))))

(defun %classified-star-center-displacement
    (mx my mz qx qy qz reach k width)
  (let ((dominant (%miter-dominant-axis mx my mz k)))
    (if dominant
        (let* ((radius (* width 1/2))
               (diagonal (* radius 0.7071067811865476d0)))
          (values (* qx (if (= dominant 0) radius diagonal))
                  (* qy (if (= dominant 1) radius diagonal))
                  (* qz (if (= dominant 2) radius diagonal))))
        (let ((scale (* width reach)))
          (values (* scale qx) (* scale qy) (* scale qz))))))

(defun %star-center-displacement (occupancy-mask width)
  "Return the shared site point displacement for a vertex star.

Ordinary stars retain the strict-minority half/centroid rule.  A planar L
uses the midpoint of the minimal circular outer miter: half width along the
arc-plane normal and half width divided by sqrt(2) along its two arc axes."
  (multiple-value-bind (mx my mz qx qy qz reach k)
      (classify-star-mask #b111 occupancy-mask)
    (%classified-star-center-displacement
     mx my mz qx qy qz reach k width)))

(defun site-displacement (domain site occupancy width)
  (unless (and (realp width) (> width 0) (< width 1/2))
    (error "Chamfer width must satisfy 0 < w < 1/2, not ~S." width))
  (multiple-value-bind (mx my mz qx qy qz reach k mask)
      (classify-site-star domain site occupancy)
    (declare (ignore mask))
    (if (= (site-extent site) +vertex-extent+)
        (%classified-star-center-displacement
         mx my mz qx qy qz reach k width)
        (let ((scale (* width reach)))
          (values (* scale qx) (* scale qy) (* scale qz))))))

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

;;; ---------------------------------------------------------------------------
;;; One 32-bit shape word per oriented face

(defconstant +edge-balanced+ 0)
(defconstant +edge-convex+ 1)
(defconstant +edge-concave+ 2)
(defconstant +edge-reserved+ 3)

;; The renderer-visible u32 shape ABI is the four complete vertex stars.  A
;; face therefore carries canonical occupancy facts rather than a lossy cache
;; of their edge and corner classifications.  Edge decisions, the ordinary
;; site point, and the miter half-points are all derived from these same bytes.
(defconstant +shape-corner-field-bits+ 8)
(defconstant +shape-low-low-corner-shift+ 0)
(defconstant +shape-low-high-corner-shift+ 8)
(defconstant +shape-high-low-corner-shift+ 16)
(defconstant +shape-high-high-corner-shift+ 24)
(defconstant +corner-direction-mask+ #b11111)
(defconstant +corner-two-thirds-mask+ #b100000)

(defun %corner-shift (corner)
  (ecase corner
    (:low-low +shape-low-low-corner-shift+)
    (:low-high +shape-low-high-corner-shift+)
    (:high-low +shape-high-low-corner-shift+)
    (:high-high +shape-high-high-corner-shift+)))

(defun %check-edge-code (code)
  (unless (and (integerp code) (<= 0 code 2))
    (error "~S is not a valid boundary-edge code." code))
  code)

(defun encode-corner-code (qx qy qz reach)
  (check-type qx (integer -1 1))
  (check-type qy (integer -1 1))
  (check-type qz (integer -1 1))
  (unless (member reach '(1/2 2/3))
    (error "Corner reach must be 1/2 or 2/3, not ~S." reach))
  (let ((direction (+ (1+ qx) (* 3 (1+ qy)) (* 9 (1+ qz)))))
    (logior direction (if (= reach 2/3) +corner-two-thirds-mask+ 0))))

(defun decode-corner-code (code)
  (check-type code (integer 0 63))
  (let ((direction (logand code +corner-direction-mask+)))
    (when (> direction 26)
      (error "Reserved corner direction code ~D." direction))
    (values (1- (mod direction 3))
            (1- (mod (floor direction 3) 3))
            (1- (floor direction 9))
            (if (logtest code +corner-two-thirds-mask+) 2/3 1/2))))

(defun %check-corner-code (code)
  (decode-corner-code code)
  code)

(defun pack-shape-word (low-low low-high high-low high-high)
  (check-type low-low (unsigned-byte 8))
  (check-type low-high (unsigned-byte 8))
  (check-type high-low (unsigned-byte 8))
  (check-type high-high (unsigned-byte 8))
  (logior (ash low-low (%corner-shift :low-low))
          (ash low-high (%corner-shift :low-high))
          (ash high-low (%corner-shift :high-low))
          (ash high-high (%corner-shift :high-high))))

(defun shape-corner-star-mask (word corner)
  (check-type word (unsigned-byte 32))
  (ldb (byte +shape-corner-field-bits+ (%corner-shift corner)) word))

(defun shape-corner-code (word corner)
  "Return the legacy compact Q/reach view derived from CORNER's full star."
  (multiple-value-bind (mx my mz qx qy qz reach)
      (classify-star-mask #b111 (shape-corner-star-mask word corner))
    (declare (ignore mx my mz))
    (encode-corner-code qx qy qz reach)))

(defun %face-edge-star-mask (face word edge)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (multiple-value-bind (corner axis-number)
        (ecase edge
          (:u-low (values :low-low v))
          (:u-high (values :high-low v))
          (:v-low (values :low-low u))
          (:v-high (values :low-high u)))
      (vertex-star-half-edge-mask
       (shape-corner-star-mask word corner) (index-axis axis-number) 1))))

(defun shape-edge-code (face word edge)
  "Derive FACE's EDGE classification from either endpoint's complete star."
  (check-type word (unsigned-byte 32))
  (check-type edge local-edge)
  (let ((k (logcount (%face-edge-star-mask face word edge))))
    (ecase k
      (1 +edge-convex+)
      (2 +edge-balanced+)
      (3 +edge-concave+))))

(defun unpack-shape-word (word)
  (values (shape-corner-star-mask word :low-low)
          (shape-corner-star-mask word :low-high)
          (shape-corner-star-mask word :high-low)
          (shape-corner-star-mask word :high-high)))

(defun shape-word-valid-p (thing)
  (typep thing '(unsigned-byte 32)))

(defun %edge-outward-vector (face edge)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (ecase edge
      (:u-low (%axis-offset u -1)) (:u-high (%axis-offset u 1))
      (:v-low (%axis-offset v -1)) (:v-high (%axis-offset v 1)))))

(defun decode-face-edge-direction (face edge edge-code)
  "Decode Q using outward normal N and in-plane outward tangent T."
  (%check-edge-code edge-code)
  (if (= edge-code +edge-balanced+)
      (values 0 0 0)
      (multiple-value-bind (nx ny nz) (face-oriented-normal face)
        (multiple-value-bind (tx ty tz) (%edge-outward-vector face edge)
          (let ((normal-sign (if (= edge-code +edge-convex+) -1 1)))
            (values (- (* normal-sign nx) tx)
                    (- (* normal-sign ny) ty)
                    (- (* normal-sign nz) tz)))))))

(defun face-shape-word (domain face occupancy)
  "Pack the complete stars of FACE's four canonical vertex sites."
  (%require-face domain face)
  (unless (and (= 1 (%face-normal-side-occupancy domain face occupancy -1))
               (= 0 (%face-normal-side-occupancy domain face occupancy 1)))
    (error "Face ~S is not oriented from solid toward air." face))
  (pack-shape-word
   (site-star-occupancy-mask
    domain (face-corner-site domain face :low-low) occupancy)
   (site-star-occupancy-mask
    domain (face-corner-site domain face :low-high) occupancy)
   (site-star-occupancy-mask
    domain (face-corner-site domain face :high-low) occupancy)
   (site-star-occupancy-mask
    domain (face-corner-site domain face :high-high) occupancy)))

;;; ---------------------------------------------------------------------------
;;; CPU reference realization of the twenty-four face points

(defconstant +face-point-count+ 24)

(defun local-point-index (i j)
  (check-type i (integer 0 3))
  (check-type j (integer 0 3))
  (+ (* 4 i) j))

(defun %corner-index (corner)
  (ecase corner
    (:low-low 0) (:low-high 1) (:high-low 2) (:high-high 3)))

(defun %index-corner (index)
  (ecase index
    (0 :low-low) (1 :low-high) (2 :high-low) (3 :high-high)))

(defun local-corner-half-point-index (corner tangent)
  "Index the site-owned half-point along CORNER's U or V half-edge."
  (check-type corner local-corner)
  (check-type tangent (member :u :v))
  (+ 16 (* 2 (%corner-index corner)) (if (eq tangent :v) 1 0)))

(defun %lambda-coordinate (index width)
  (ecase index (0 0d0) (1 width) (2 (- 1d0 width)) (3 1d0)))

(defun %double-anchor (site)
  (values (coerce (site-x site) 'double-float)
          (coerce (site-y site) 'double-float)
          (coerce (site-z site) 'double-float)))

(defun %offset-point (x y z axis-number amount)
  (ecase axis-number
    (0 (values (+ x amount) y z))
    (1 (values x (+ y amount) z))
    (2 (values x y (+ z amount)))))

(defun %edge-axis-index (edge-site)
  (cond ((logbitp 0 (site-extent edge-site)) 0)
        ((logbitp 1 (site-extent edge-site)) 1)
        ((logbitp 2 (site-extent edge-site)) 2)
        (t (error "Malformed edge site ~S." edge-site))))

(defun %edge-point-data (i j)
  (cond ((= i 0) (values :u-low j))
        ((= i 3) (values :u-high j))
        ((= j 0) (values :v-low i))
        ((= j 3) (values :v-high i))
        (t (error "(~D,~D) is not on exactly one edge." i j))))

(defun %corner-point-data (i j)
  (cond ((and (= i 0) (= j 0)) :low-low)
        ((and (= i 0) (= j 3)) :low-high)
        ((and (= i 3) (= j 0)) :high-low)
        ((and (= i 3) (= j 3)) :high-high)
        (t (error "(~D,~D) is not a corner." i j))))

(defun %corner-high-p (corner tangent)
  (ecase tangent
    (:u (member corner '(:high-low :high-high)))
    (:v (member corner '(:low-high :high-high)))))

(defun %corner-half-edge-data (face corner tangent)
  (multiple-value-bind (u v) (%face-tangent-indices face)
    (let ((high-p (%corner-high-p corner tangent)))
      (values (ecase tangent (:u u) (:v v))
              (if high-p -1 1)))))

(defun %star-half-point-displacement
    (occupancy-mask axis-number direction width)
  "Return the site-owned point between a vertex centre and one edge ring.

Outside an outer miter it bisects the old centre-to-ring segment exactly, so
the added topology leaves the old planar surface unchanged.  For the two
planar-L boundary rays that point into the missing quadrant it lands on a
circular-join endpoint, allowing the intervening face sectors to turn the
strip instead of pinching it.  The other four rays remain midpoints: they
cross an uninterrupted face or run along the layer normal."
  (multiple-value-bind (mx my mz qx qy qz reach k)
      (classify-star-mask #b111 occupancy-mask)
    (let* ((dominant (%miter-dominant-axis mx my mz k))
           (radius (* width 1/2)))
      (when (and dominant
                 (/= axis-number dominant)
                 (= direction
                    (- (ecase axis-number (0 qx) (1 qy) (2 qz)))))
        (let ((other (- 3 dominant axis-number))
              (dx 0d0) (dy 0d0) (dz 0d0))
          (let ((dominant-amount
                  (* radius (ecase dominant (0 qx) (1 qy) (2 qz)))))
            (ecase dominant
              (0 (setf dx dominant-amount))
              (1 (setf dy dominant-amount))
              (2 (setf dz dominant-amount))))
          (let ((bend-amount
                  (* radius (ecase other (0 qx) (1 qy) (2 qz)))))
            (ecase other
              (0 (setf dx bend-amount))
              (1 (setf dy bend-amount))
              (2 (setf dz bend-amount))))
          (return-from %star-half-point-displacement
            (values dx dy dz))))
      (multiple-value-bind (cx cy cz)
          (%classified-star-center-displacement
           mx my mz qx qy qz reach k width)
        (let* ((axis (index-axis axis-number))
               (edge-mask
                 (vertex-star-half-edge-mask occupancy-mask axis direction))
               (normal-mask (logandc2 +cell-extent+ (ash 1 axis-number))))
          (multiple-value-bind (mx my mz ex ey ez edge-reach)
              (classify-star-mask normal-mask edge-mask)
            (declare (ignore mx my mz edge-reach))
            (let ((rx (+ (* ex radius)
                         (if (= axis-number 0) (* direction width) 0)))
                  (ry (+ (* ey radius)
                         (if (= axis-number 1) (* direction width) 0)))
                  (rz (+ (* ez radius)
                         (if (= axis-number 2) (* direction width) 0))))
              (values (* 1/2 (+ cx rx))
                      (* 1/2 (+ cy ry))
                      (* 1/2 (+ cz rz))))))))))

(defun realize-face-point (domain face shape-word width i j)
  "Return one XYZ point as three DOUBLE-FLOAT values.

Edge and corner flat positions are evaluated from their canonical cubical site,
so every incident face uses the same floating operation sequence.  X/Y results
remain modulo the domain period; a renderer crossing a wrapped seam must lift the
whole patch consistently before Euclidean interpolation."
  (%require-face domain face)
  (unless (shape-word-valid-p shape-word)
    (error "Invalid shape word ~S." shape-word))
  (unless (and (realp width) (> width 0) (< width 1/2))
    (error "Chamfer width must satisfy 0 < w < 1/2, not ~S." width))
  (check-type i (integer 0 3))
  (check-type j (integer 0 3))
  (let* ((w (coerce width 'double-float))
         (ib (or (= i 0) (= i 3)))
         (jb (or (= j 0) (= j 3)))
         (bx 0d0) (by 0d0) (bz 0d0)
         (qx 0) (qy 0) (qz 0) (reach 1/2))
    (cond
      ((and (not ib) (not jb))
       (multiple-value-bind (u v) (%face-tangent-indices face)
         (multiple-value-setq (bx by bz) (%double-anchor face))
         (multiple-value-setq (bx by bz)
           (%offset-point bx by bz u (%lambda-coordinate i w)))
         (multiple-value-setq (bx by bz)
           (%offset-point bx by bz v (%lambda-coordinate j w)))))
      ((and ib jb)
       (let* ((corner (%corner-point-data i j))
              (vertex (face-corner-site domain face corner))
              (star (shape-corner-star-mask shape-word corner)))
         (multiple-value-setq (bx by bz) (%double-anchor vertex))
         (multiple-value-bind (dx dy dz)
             (%star-center-displacement star w)
           (return-from realize-face-point
             (values (+ bx dx) (+ by dy) (+ bz dz))))))
      (t
       (multiple-value-bind (edge parameter) (%edge-point-data i j)
         (let ((edge-site (face-edge-site domain face edge)))
           (multiple-value-setq (bx by bz) (%double-anchor edge-site))
           (multiple-value-setq (bx by bz)
             (%offset-point bx by bz (%edge-axis-index edge-site)
                            (%lambda-coordinate parameter w)))
           (multiple-value-setq (qx qy qz)
             (decode-face-edge-direction
              face edge (shape-edge-code face shape-word edge)))))))
    (let ((scale (* w (coerce reach 'double-float))))
      (values (+ bx (* scale qx))
              (+ by (* scale qy))
              (+ bz (* scale qz))))))

(defun realize-face-half-point (domain face shape-word width corner tangent)
  (let* ((w (coerce width 'double-float))
         (vertex (face-corner-site domain face corner))
         (star (shape-corner-star-mask shape-word corner)))
    (multiple-value-bind (axis-number direction)
        (%corner-half-edge-data face corner tangent)
      (multiple-value-bind (dx dy dz)
          (%star-half-point-displacement star axis-number direction w)
        (multiple-value-bind (x y z) (%double-anchor vertex)
          (values (+ x dx) (+ y dy) (+ z dz)))))))

(defun realize-face-local-point (domain face shape-word width point-index)
  "Return one of the fixed face patch's 24 implicit points."
  (%require-face domain face)
  (unless (shape-word-valid-p shape-word)
    (error "Invalid shape word ~S." shape-word))
  (unless (and (realp width) (> width 0) (< width 1/2))
    (error "Chamfer width must satisfy 0 < w < 1/2, not ~S." width))
  (check-type point-index (integer 0 23))
  (if (< point-index 16)
      (multiple-value-bind (i j) (floor point-index 4)
        (realize-face-point domain face shape-word width i j))
      (multiple-value-bind (corner-index tangent-index)
          (floor (- point-index 16) 2)
        (realize-face-half-point
         domain face shape-word width (%index-corner corner-index)
         (if (zerop tangent-index) :u :v)))))

(defun realize-face-patch (domain face shape-word width)
  "Return 24 XYZ points as one 72-element unboxed double vector."
  (%require-face domain face)
  (unless (shape-word-valid-p shape-word)
    (error "Invalid shape word ~S." shape-word))
  (let ((points (make-array (* 3 +face-point-count+)
                            :element-type 'double-float)))
    (dotimes (point-index +face-point-count+ points)
      (let ((base (* 3 point-index)))
        (multiple-value-bind (x y z)
            (realize-face-local-point
             domain face shape-word width point-index)
          (setf (aref points base) x
                (aref points (+ base 1)) y
                (aref points (+ base 2)) z))))))

;;; ---------------------------------------------------------------------------
;;; Fixed 16-byte renderer-facing face-record ABI

(defconstant +face-record-word-count+ 4)
(defconstant +face-record-byte-size+ 16)
(defconstant +face-record-site-low-word+ 0)
(defconstant +face-record-site-high-word+ 1)
(defconstant +face-record-shape-word+ 2)
(defconstant +face-record-construction-word+ 3)
(defconstant +decorated-site-stock-shift+ 60)

(defun decorate-site (site stock)
  (check-type site site)
  (check-type stock (unsigned-byte 4))
  (logior site (ash stock +decorated-site-stock-shift+)))
(defun undecorated-site (decorated-site)
  (check-type decorated-site (unsigned-byte 64))
  (logand decorated-site +site-mask+))
(defun decorated-site-stock (decorated-site)
  (check-type decorated-site (unsigned-byte 64))
  (ldb (byte 4 +decorated-site-stock-shift+) decorated-site))

(defun make-face-record-array (record-count)
  (check-type record-count (integer 0 *))
  (make-array (* record-count +face-record-word-count+)
              :element-type '(unsigned-byte 32)
              :initial-element 0))

(defun store-face-record (words record-index domain face shape-word
                          &optional (stock 0) (construction-mask 0))
  "Store the packed site, four vertex stars, stock, and construction mask."
  (check-type words (array (unsigned-byte 32) (*)))
  (check-type record-index (integer 0 *))
  (%require-face domain face)
  (unless (shape-word-valid-p shape-word)
    (error "Invalid shape word ~S." shape-word))
  (check-type stock (unsigned-byte 4))
  (check-type construction-mask (unsigned-byte 29))
  (let ((base (* record-index +face-record-word-count+)))
    (when (> (+ base +face-record-word-count+) (length words))
      (error "Record ~D is outside a ~D-word array." record-index (length words)))
    (let ((decorated (decorate-site face stock)))
      (setf (aref words (+ base +face-record-site-low-word+))
            (ldb (byte 32 0) decorated)
            (aref words (+ base +face-record-site-high-word+))
            (ldb (byte 32 32) decorated)
            (aref words (+ base +face-record-shape-word+)) shape-word
            (aref words (+ base +face-record-construction-word+))
            construction-mask)))
  words)

(defun load-face-record (words record-index domain)
  "Validate and return FACE, SHAPE-WORD, STOCK, CONSTRUCTION-MASK."
  (check-type words (array (unsigned-byte 32) (*)))
  (check-type record-index (integer 0 *))
  (let ((base (* record-index +face-record-word-count+)))
    (when (> (+ base +face-record-word-count+) (length words))
      (error "Record ~D is outside a ~D-word array." record-index (length words)))
    (let* ((decorated
             (logior (aref words (+ base +face-record-site-low-word+))
                     (ash (aref words
                                (+ base +face-record-site-high-word+))
                          32)))
           (face (undecorated-site decorated))
           (shape (aref words (+ base +face-record-shape-word+)))
           (stock (decorated-site-stock decorated))
           (construction-mask
             (aref words (+ base +face-record-construction-word+))))
      (%require-face domain face)
      (unless (shape-word-valid-p shape)
        (error "Record ~D has invalid shape word ~S." record-index shape))
      (unless (typep construction-mask '(unsigned-byte 29))
        (error "Record ~D has invalid construction mask ~S."
               record-index construction-mask))
      (values face shape stock construction-mask))))

(defun make-face-record (domain face shape-word
                         &optional (stock 0) (construction-mask 0))
  (let ((record (make-face-record-array 1)))
    (store-face-record record 0 domain face shape-word stock construction-mask)
    record))

;;; ---------------------------------------------------------------------------
;;; Canonical indexed raster topology

(defconstant +face-index-count+ 78)

(defun quad-diagonal-toward-corner (i j)
  "The diagonal of grid quad (I,J) that points at the nearest face corner.

A corner quad is not planar: its four realized points straddle a chamfer, so
the two ways of splitting it are two different surfaces.  Splitting along the
diagonal that reaches the face corner keeps the corner on the fold, which is
what makes a chamfer band close against its neighbours as a flat miter.  The
other split leaves the corner hanging off one triangle and the fold running
past it, and the band ends in a fan instead."
  (check-type i (integer 0 2))
  (check-type j (integer 0 2))
  (if (if (< i 2) (< j 2) (= j 2)) :c00-c11 :c10-c01))

(defun make-face-index-template (polarity &key (diagonal :toward-corner))
  "Generate 26 consistently wound triangles over the fixed 24-point patch.

The five non-corner quads retain their old splits.  Each corner is a hexagon:
the shared site centre, two half-edge-owned points, the two old ring points,
and the old inner point.  Four triangles replace the old two, which is the
smallest fixed topology that turns both incident chamfer strips into quads
while retaining a site-centred cap and indexed sharing."
  (check-type polarity (member 1 -1))
  (check-type diagonal (member :toward-corner :c00-c11 :c10-c01))
  (let ((indices (make-array +face-index-count+
                             :element-type '(unsigned-byte 16)))
        (write 0))
    (labels ((emit (a b c)
               (if (plusp polarity)
                   (setf (aref indices write) a
                         (aref indices (+ write 1)) b
                         (aref indices (+ write 2)) c)
                   (setf (aref indices write) a
                         (aref indices (+ write 1)) c
                         (aref indices (+ write 2)) b))
               (incf write 3)))
      (dotimes (i 3)
        (dotimes (j 3)
          (unless (and (member i '(0 2)) (member j '(0 2)))
            (let ((c00 (local-point-index i j))
                  (c10 (local-point-index (1+ i) j))
                  (c11 (local-point-index (1+ i) (1+ j)))
                  (c01 (local-point-index i (1+ j))))
              (ecase (if (eq diagonal :toward-corner)
                         (quad-diagonal-toward-corner i j)
                         diagonal)
                (:c00-c11
                 (emit c00 c10 c11)
                 (emit c00 c11 c01))
                (:c10-c01
                 (emit c00 c10 c01)
                 (emit c10 c11 c01)))))))
      (dolist (spec '((:low-low 0 0 1 1)
                      (:low-high 0 3 1 -1)
                      (:high-low 3 0 -1 1)
                      (:high-high 3 3 -1 -1)))
        (destructuring-bind (corner i j su sv) spec
          (let ((center (local-point-index i j))
                (half-u (local-corner-half-point-index corner :u))
                (half-v (local-corner-half-point-index corner :v))
                (ring-u (local-point-index (+ i su) j))
                (ring-v (local-point-index i (+ j sv)))
                (inner (local-point-index (+ i su) (+ j sv))))
            (flet ((corner-triangle (a b c)
                     (if (plusp (* su sv))
                         (emit a b c)
                         (emit a c b))))
              (corner-triangle center half-u inner)
              (corner-triangle half-u ring-u inner)
              (corner-triangle center inner half-v)
              (corner-triangle half-v inner ring-v)))))
      (unless (= write +face-index-count+)
        (error "Wrote ~D face indices, expected ~D."
               write +face-index-count+)))
    indices))

(defparameter *positive-face-index-template* (make-face-index-template 1))
(defparameter *negative-face-index-template* (make-face-index-template -1))

(defun %copy-u16-vector (source)
  (let ((copy (make-array (length source) :element-type '(unsigned-byte 16))))
    (replace copy source)
    copy))
(defun positive-face-index-template ()
  (%copy-u16-vector *positive-face-index-template*))
(defun negative-face-index-template ()
  (%copy-u16-vector *negative-face-index-template*))

;;; ---------------------------------------------------------------------------
;;; Construction edges

(defun %ordered-edge (a b)
  (list (min a b) (max a b)))

(defparameter *construction-noncorner-quads*
  '((0 1) (1 0) (1 1) (1 2) (2 1)))

(defun %make-construction-edge-pairs ()
  "Return the 29 internal patch edges in renderer bit order."
  (let ((pairs '()))
    ;; Bits 0..5: the two internal constant-U lines, three spans each.
    (dolist (i '(1 2))
      (dotimes (j 3)
        (push (%ordered-edge (local-point-index i j)
                             (local-point-index i (1+ j)))
              pairs)))
    ;; Bits 6..11: the two internal constant-V lines, three spans each.
    (dolist (j '(1 2))
      (dotimes (i 3)
        (push (%ordered-edge (local-point-index i j)
                             (local-point-index (1+ i) j))
              pairs)))
    ;; Bits 12..16: diagonals of the five unchanged quads.
    (dolist (cell *construction-noncorner-quads*)
      (destructuring-bind (i j) cell
        (let ((c00 (local-point-index i j))
              (c10 (local-point-index (1+ i) j))
              (c11 (local-point-index (1+ i) (1+ j)))
              (c01 (local-point-index i (1+ j))))
          (push (ecase (quad-diagonal-toward-corner i j)
                  (:c00-c11 (%ordered-edge c00 c11))
                  (:c10-c01 (%ordered-edge c10 c01)))
                pairs))))
    ;; Bits 17..28: centre and two half-point spokes in each corner.
    (dolist (spec '((:low-low 0 0 1 1)
                    (:low-high 0 3 1 -1)
                    (:high-low 3 0 -1 1)
                    (:high-high 3 3 -1 -1)))
      (destructuring-bind (corner i j su sv) spec
        (let ((center (local-point-index i j))
              (inner (local-point-index (+ i su) (+ j sv))))
          (push (%ordered-edge center inner) pairs)
          (push (%ordered-edge
                 (local-corner-half-point-index corner :u) inner)
                pairs)
          (push (%ordered-edge
                 (local-corner-half-point-index corner :v) inner)
                pairs))))
    (let ((answer (coerce (nreverse pairs) 'vector)))
      (unless (= 29 (length answer))
        (error "Expected 29 construction edges, got ~D." (length answer)))
      (unless (= 29 (length (remove-duplicates (coerce answer 'list)
                                               :test #'equal)))
        (error "Construction edge table contains duplicates."))
      answer)))

(defparameter *construction-edge-pairs*
  (%make-construction-edge-pairs))

(defun %make-construction-edge-adjacencies ()
  "Return BIT, TRIANGLE-A, TRIANGLE-B triples for all 29 internal edges."
  (let ((incidences (make-hash-table :test #'equal))
        (indices *positive-face-index-template*))
    (dotimes (triangle 26)
      (let* ((base (* triangle 3))
             (a (aref indices base))
             (b (aref indices (+ base 1)))
             (c (aref indices (+ base 2))))
        (dolist (edge (list (list a b) (list b c) (list c a)))
          (destructuring-bind (left right) edge
            (push triangle
                  (gethash (list (min left right) (max left right))
                           incidences))))))
    (let ((triples (make-array 29)))
      (dotimes (bit 29)
        (let* ((edge (aref *construction-edge-pairs* bit))
               (triangles (gethash edge incidences)))
          (unless (= 2 (length triangles))
            (error "Construction edge ~S has ~D incident triangles."
                   edge (length triangles)))
          (setf (aref triples bit)
                (list bit (first triangles) (second triangles)))))
      (let ((internal-count 0))
        (maphash (lambda (edge triangles)
                   (declare (ignore edge))
                   (when (= 2 (length triangles)) (incf internal-count)))
                 incidences)
        (unless (= internal-count 29)
          (error "Topology has ~D internal edges, expected 29."
                 internal-count)))
      triples)))

(defparameter *construction-edge-adjacencies*
  (%make-construction-edge-adjacencies))

(defun %triangle-unit-normal (points indices triangle)
  (labels ((coordinate (vertex component)
             (aref points (+ (* 3 vertex) component))))
    (let* ((base (* triangle 3))
           (a (aref indices base))
           (b (aref indices (+ base 1)))
           (c (aref indices (+ base 2)))
           (abx (- (coordinate b 0) (coordinate a 0)))
           (aby (- (coordinate b 1) (coordinate a 1)))
           (abz (- (coordinate b 2) (coordinate a 2)))
           (acx (- (coordinate c 0) (coordinate a 0)))
           (acy (- (coordinate c 1) (coordinate a 1)))
           (acz (- (coordinate c 2) (coordinate a 2)))
           (nx (- (* aby acz) (* abz acy)))
           (ny (- (* abz acx) (* abx acz)))
           (nz (- (* abx acy) (* aby acx)))
           (length (sqrt (+ (* nx nx) (* ny ny) (* nz nz)))))
      (when (zerop length)
        (error "Degenerate LUFT raster triangle ~D." triangle))
      (values (/ nx length) (/ ny length) (/ nz length)))))

(defun face-construction-mask (domain face shape-word width)
  "Mark each internal raster edge whose two realized facet normals differ.

The outer four rims are cubical-complex boundaries and are drawn separately.
Bits 0..5 describe the two internal U lines, 6..11 the internal V lines,
12..16 the five ordinary diagonals, and 17..28 the three spokes of each
corner hexagon.  A tiny tolerance rejects only numerical noise from coplanar
triangles; screen-space derivatives later set line width, never edge meaning."
  (let* ((points (realize-face-patch domain face shape-word width))
         (indices *positive-face-index-template*)
         (normals (make-array +face-index-count+
                              :element-type 'double-float)))
    (dotimes (triangle 26)
      (multiple-value-bind (x y z)
          (%triangle-unit-normal points indices triangle)
        (let ((base (* triangle 3)))
          (setf (aref normals base) x
                (aref normals (+ base 1)) y
                (aref normals (+ base 2)) z))))
    (loop with mask = 0
          for (bit left right) across *construction-edge-adjacencies*
          for left-base = (* left 3)
          for right-base = (* right 3)
          for dot = (+ (* (aref normals left-base)
                          (aref normals right-base))
                       (* (aref normals (+ left-base 1))
                          (aref normals (+ right-base 1)))
                       (* (aref normals (+ left-base 2))
                          (aref normals (+ right-base 2))))
          when (< dot 0.999999d0)
            do (setf mask (logior mask (ash 1 bit)))
          finally (return mask))))

;;; ---------------------------------------------------------------------------
;;; Focused executable tests

(defvar *luft-test-count* 0)
(defvar *luft-test-section* nil)

(defmacro %check (form &optional note)
  `(progn
     (incf *luft-test-count*)
     (unless ,form
       (error "LUFT test failed in ~A~@[ (~A)~]: ~S"
              *luft-test-section* ,note ',form))))

(defmacro %with-test-section ((name) &body body)
  `(let ((*luft-test-section* ,name)) ,@body))

(defun %signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun %chain-from-sites (domain sites)
  (let ((builder (make-chain-builder domain :initial-capacity (length sites))))
    (dolist (site sites) (chain-builder-add-site builder site))
    (finish-chain-builder builder)))

(defun %boundary-sites (domain site)
  (let ((parts '()))
    (map-site-boundary
     (lambda (part axis side)
       (declare (ignore axis side))
       (push part parts))
     domain site)
    (nreverse parts)))

(defun %test-sites-and-topology ()
  (%with-test-section ("packed sites and topology")
    (let* ((domain (make-world-domain :x-bits 3 :y-bits 2))
           (site (make-site domain 10 -1 7 +xz-face-extent+ -1)))
      (%check (= (site-x site) 2))
      (%check (= (site-y site) 3))
      (%check (= (site-z site) 7))
      (%check (= (site-extent site) +xz-face-extent+))
      (%check (= (site-polarity site) -1))
      (%check (= (opposite-site (opposite-site site)) site))
      (%check (= (site-geometry site) (site-geometry (opposite-site site))))
      (%check (site-valid-p domain site))
      (%check (= (site-x (site-forward domain
                                       (make-site domain 7 0 0) :x))
                 0))
      (%check (= (site-y (site-backward domain
                                        (make-site domain 0 0 0) :y))
                 3))
      (%check (null (site-backward domain (make-site domain 0 0 0) :z)))
      (%check (null (site-forward domain
                                  (make-site domain 0 0 +top-z+) :z)))
      (%check (%signals-error-p
               (lambda ()
                 (make-site domain 0 0 +top-z+ +z-edge-extent+))))
      (let* ((larger (make-world-domain :x-bits 4 :y-bits 3))
             (noncanonical (make-site larger 8 4 0)))
        (%check (not (site-valid-p domain noncanonical))))
      ;; Every boundary has one lower dimension; parent polarity reverses all.
      (loop for extent from 0 to 7 do
        (loop for polarity in '(1 -1) do
          (let* ((parent (make-site domain 2 1 10 extent polarity))
                 (parts (%boundary-sites domain parent))
                 (opposite-parts
                   (%boundary-sites domain (opposite-site parent))))
            (%check (= (length parts) (* 2 (site-dimension parent))))
            (%check (every (lambda (part)
                             (= (site-dimension part)
                                (1- (site-dimension parent))))
                           parts))
            (%check (every #'=
                           (mapcar #'opposite-site parts)
                           opposite-parts)))))
      ;; Boundary squared is zero for every representative site and polarity.
      (loop for extent from 0 to 7 do
        (loop for polarity in '(1 -1) do
          (let* ((one (%chain-from-sites
                       domain (list (make-site domain 2 1 10 extent polarity))))
                 (twice (boundary-chain (boundary-chain one))))
            (%check (chain-empty-p twice)))))
      ;; Cofaces reproduce the requested signed boundary exactly.
      (loop for extent from 0 to 7 do
        (loop for polarity in '(1 -1) do
          (let ((part (make-site domain 2 1 10 extent polarity)))
            (dotimes (axis-number 3)
              (unless (logbitp axis-number extent)
                (let* ((axis (index-axis axis-number))
                       (forward (site-coface-forward domain part axis))
                       (backward (site-coface-backward domain part axis)))
                  (%check (= (site-boundary-low domain forward axis) part))
                  (%check (= (site-boundary-high domain backward axis) part))))))))
      ;; Two neighbouring positive cells lose exactly their shared face.
      (let* ((a (make-site domain 2 1 10 +cell-extent+))
             (b (make-site domain 3 1 10 +cell-extent+))
             (solid (%chain-from-sites domain (list a b)))
             (surface (surface-chain solid))
             (shared (site-geometry (site-boundary-high domain a :x))))
        (%check (= (chain-count surface) 10))
        (%check (= (chain-site-count surface shared) 0))
        (%check (= (chain-site-count surface (opposite-site shared)) 0))))))

(defun %test-chains ()
  (%with-test-section ("normalized chains")
    (let* ((domain (make-world-domain :x-bits 4 :y-bits 4))
           (a (make-site domain 1 1 1 +vertex-extent+ 1))
           (b (make-site domain 2 1 1 +x-edge-extent+ -1))
           (c (make-site domain 3 1 1 +xy-face-extent+ 1))
           (input (list a a a (opposite-site a) b b c (opposite-site c)))
           (chain-a (%chain-from-sites domain input))
           (chain-b (%chain-from-sites domain (reverse input))))
      (%check (chain= chain-a chain-b))
      (%check (= (chain-count chain-a) 4))
      (%check (= (chain-site-count chain-a a) 2))
      (%check (= (chain-site-count chain-a (opposite-site a)) 0))
      (%check (= (chain-site-count chain-a b) 2))
      (%check (= (chain-site-count chain-a c) 0))
      (let ((sites (chain-sites chain-a)))
        (%check (loop for i from 1 below (length sites)
                      always (not (%site-order< (aref sites i)
                                                (aref sites (1- i))))))
        ;; A public copy cannot mutate the chain value.
        (when (plusp (length sites)) (setf (aref sites 0) 0))
        (%check (= (chain-site-count chain-a a) 2)))
      (let ((mapped '()))
        (map-chain (lambda (site) (push site mapped)) chain-a)
        (%check (equal (nreverse mapped) (coerce (chain-sites chain-a) 'list))))
      ;; Linear merge agrees with normalizing concatenated occurrences.
      (let* ((left (%chain-from-sites domain (list a a b c)))
             (right (%chain-from-sites
                     domain (list (opposite-site a) b (opposite-site c))))
             (merged (chain+ left right))
             (reference (%chain-from-sites
                         domain
                         (append (coerce (chain-sites left) 'list)
                                 (coerce (chain-sites right) 'list)))))
        (%check (chain= merged reference)))
      (%check (not (chain= chain-a
                           (make-chain (make-world-domain :x-bits 3 :y-bits 4))))))))

(defun %direction-index (normal-mask dx dy dz)
  (let ((index 0) (position 0))
    (dotimes (axis-number 3 index)
      (when (logbitp axis-number normal-mask)
        (let ((component (ecase axis-number (0 dx) (1 dy) (2 dz))))
          (unless (member component '(-1 1))
            (error "Bad transformed star direction."))
          (when (plusp component) (setf index (logior index (ash 1 position))))
          (incf position))))))

(defun %transform-components (x y z permutation reflection-mask)
  ;; New component A is reflected OLD[PERMUTATION[A]].
  (let ((old (vector x y z))
        (new (make-array 3 :initial-element 0)))
    (dotimes (axis-number 3)
      (setf (aref new axis-number)
            (* (if (logbitp axis-number reflection-mask) -1 1)
               (aref old (aref permutation axis-number)))))
    (values (aref new 0) (aref new 1) (aref new 2))))

(defun %transform-star (normal-mask occupancy-mask permutation reflection-mask)
  (let ((new-normal 0))
    (dotimes (new-axis 3)
      (when (logbitp (aref permutation new-axis) normal-mask)
        (setf new-normal (logior new-normal (ash 1 new-axis)))))
    (let ((new-occupancy 0)
          (sample-count (ash 1 (logcount normal-mask))))
      (dotimes (sample sample-count)
        (when (logbitp sample occupancy-mask)
          (multiple-value-bind (dx dy dz)
              (%star-sample-direction normal-mask sample)
            (multiple-value-bind (nx ny nz)
                (%transform-components dx dy dz permutation reflection-mask)
              (let ((new-sample (%direction-index new-normal nx ny nz)))
                (setf new-occupancy
                      (logior new-occupancy (ash 1 new-sample))))))))
      (values new-normal new-occupancy))))

(defun %test-classifier ()
  (%with-test-section ("strict-minority classification")
    ;; Edge and vertex truth tables, balance, complement symmetry, and reach.
    (dolist (normal-mask '(#b011 #b101 #b110 #b111))
      (let* ((rank (logcount normal-mask))
             (samples (ash 1 rank))
             (pattern-count (ash 1 samples))
             (full-mask (1- pattern-count)))
        (dotimes (occupancy pattern-count)
          (multiple-value-bind (mx my mz qx qy qz reach k)
              (classify-star-mask normal-mask occupancy)
            (when (= (* 2 k) samples)
              (%check (and (zerop mx) (zerop my) (zerop mz)
                           (zerop qx) (zerop qy) (zerop qz))))
            (multiple-value-bind (cmx cmy cmz cqx cqy cqz creach ck)
                (classify-star-mask normal-mask (logxor occupancy full-mask))
              (%check (and (= mx cmx) (= my cmy) (= mz cmz)
                           (= qx cqx) (= qy cqy) (= qz cqz)
                           (= reach creach) (= (+ k ck) samples))))
            (when (= rank 3)
              (%check (eq (= reach 2/3)
                          (and (= (abs mx) 1)
                               (= (abs my) 1)
                               (= (abs mz) 1)))))))))
    ;; Every singleton minority and complement has unit diagonal moment/reach.
    (dotimes (sample 8)
      (dolist (mask (list (ash 1 sample) (logxor #xff (ash 1 sample))))
        (multiple-value-bind (mx my mz qx qy qz reach)
            (classify-star-mask #b111 mask)
          (declare (ignore qx qy qz))
          (%check (and (= (abs mx) 1) (= (abs my) 1) (= (abs mz) 1)
                       (= reach 2/3))))))
    ;; The three-cell warning is live: some such minorities also get 2/3.
    (let ((three-cell-unit-diagonals 0))
      (dotimes (mask 256)
        (when (= (logcount mask) 3)
          (multiple-value-bind (mx my mz qx qy qz reach)
              (classify-star-mask #b111 mask)
            (declare (ignore qx qy qz))
            (when (and (= (abs mx) 1) (= (abs my) 1) (= (abs mz) 1))
              (incf three-cell-unit-diagonals)
              (%check (= reach 2/3))))))
      (%check (plusp three-cell-unit-diagonals)))
    ;; Exactly the planar three-cell L stars and their complements acquire an
    ;; arc.  The motivating wall termination is one member of that orbit.
    (let ((miter-count 0))
      (dotimes (mask 256)
        (when (star-miter-arc-p mask) (incf miter-count)))
      (%check (= miter-count 48))
      (%check (star-miter-arc-p #xcd))
      (%check (star-miter-arc-p (logxor #xff #xcd))))
    ;; #xCD has moment (1,-3,1).  Its two strip endpoints and circular
    ;; midpoint all lie at radius W/2 in the XZ arc plane and Y=-W/2.
    (let* ((width 0.2d0)
           (radius (* width 1/2))
           (diagonal (* radius 0.7071067811865476d0)))
      (multiple-value-bind (cx cy cz)
          (%star-center-displacement #xcd width)
        (%check (< (abs (- cx diagonal)) 1d-15))
        (%check (< (abs (+ cy radius)) 1d-15))
        (%check (< (abs (- cz diagonal)) 1d-15))
        (%check (< (abs (- (sqrt (+ (* cx cx) (* cz cz))) radius))
                   1d-15)))
      (let ((x-ray-end
              (multiple-value-list
               (%star-half-point-displacement #xcd 0 -1 width)))
            (z-ray-end
              (multiple-value-list
               (%star-half-point-displacement #xcd 2 -1 width)))
            (uninterrupted-x
              (multiple-value-list
               (%star-half-point-displacement #xcd 0 1 width))))
        (%check (equal x-ray-end (list 0d0 (- radius) radius)))
        (%check (equal z-ray-end (list radius (- radius) 0d0)))
        (%check
         (equal uninterrupted-x
                (list (* 1/2 (+ diagonal width))
                      (* -1/2 radius)
                      (* 1/2 diagonal))))))
    ;; Full equivariance under all six permutations and eight reflections.
    (let ((permutations
            #(#(0 1 2) #(0 2 1) #(1 0 2)
              #(1 2 0) #(2 0 1) #(2 1 0))))
      (dolist (normal-mask '(#b011 #b101 #b110 #b111))
        (let* ((samples (ash 1 (logcount normal-mask)))
               (pattern-count (ash 1 samples)))
          (dotimes (occupancy pattern-count)
            (multiple-value-bind (mx my mz qx qy qz reach k)
                (classify-star-mask normal-mask occupancy)
              (loop for permutation across permutations do
                (dotimes (reflection 8)
                  (multiple-value-bind (new-normal new-occupancy)
                      (%transform-star normal-mask occupancy
                                       permutation reflection)
                    (multiple-value-bind (nmx nmy nmz nqx nqy nqz nreach nk)
                        (classify-star-mask new-normal new-occupancy)
                      (multiple-value-bind (tmx tmy tmz)
                          (%transform-components mx my mz permutation reflection)
                        (multiple-value-bind (tqx tqy tqz)
                            (%transform-components qx qy qz
                                                   permutation reflection)
                          (%check (and (= nmx tmx) (= nmy tmy) (= nmz tmz)
                                       (= nqx tqx) (= nqy tqy) (= nqz tqz)
                                       (= nreach reach) (= nk k))))))))))))))
    ;; The new derived geometry is itself complement invariant and equivariant
    ;; under the whole cubical symmetry group, including its half-edge key.
    (let ((permutations
            #(#(0 1 2) #(0 2 1) #(1 0 2)
              #(1 2 0) #(2 0 1) #(2 1 0)))
          (width 0.2d0))
      (dotimes (mask 256)
        (let ((complement (logxor #xff mask)))
          (%check (eq (star-miter-arc-p mask)
                      (star-miter-arc-p complement)))
          (%check
           (equal (multiple-value-list
                   (%star-center-displacement mask width))
                  (multiple-value-list
                   (%star-center-displacement complement width))))
          (dotimes (axis-number 3)
            (dolist (direction '(-1 1))
              (%check
               (equal
                (multiple-value-list
                 (%star-half-point-displacement
                  mask axis-number direction width))
                (multiple-value-list
                 (%star-half-point-displacement
                  complement axis-number direction width))))))
          (loop for permutation across permutations do
            (dotimes (reflection 8)
              (multiple-value-bind (new-normal transformed-mask)
                  (%transform-star #b111 mask permutation reflection)
                (%check (= new-normal #b111))
                (%check (eq (star-miter-arc-p mask)
                            (star-miter-arc-p transformed-mask)))
                (multiple-value-bind (dx dy dz)
                    (%star-center-displacement mask width)
                  (multiple-value-bind (tdx tdy tdz)
                      (%transform-components
                       dx dy dz permutation reflection)
                    (multiple-value-bind (ndx ndy ndz)
                        (%star-center-displacement transformed-mask width)
                      (%check (and (= ndx tdx) (= ndy tdy) (= ndz tdz))))))
                (dotimes (axis-number 3)
                  (dolist (direction '(-1 1))
                    (let* ((new-axis (position axis-number permutation))
                           (new-direction
                             (* direction
                                (if (logbitp new-axis reflection) -1 1))))
                      (multiple-value-bind (dx dy dz)
                          (%star-half-point-displacement
                           mask axis-number direction width)
                        (multiple-value-bind (tdx tdy tdz)
                            (%transform-components
                             dx dy dz permutation reflection)
                          (multiple-value-bind (ndx ndy ndz)
                              (%star-half-point-displacement
                               transformed-mask new-axis new-direction width)
                            (%check
                             (and (= ndx tdx) (= ndy tdy) (= ndz tdz)))))))))))))))
    ;; Missing vertical cells are centralized as air and never call OCCUPANCY.
    (let ((calls 0)
          (domain (make-world-domain :x-bits 2 :y-bits 2)))
      (%check (= 0 (cell-occupancy-bit
                    domain (lambda (&rest args)
                             (declare (ignore args)) (incf calls) 1)
                    0 0 -1)))
      (%check (= 0 calls))
      (%check (= 0 (cell-occupancy-bit
                    domain (lambda (&rest args)
                             (declare (ignore args)) (incf calls) 1)
                    0 0 +top-z+)))
      (%check (= 0 calls))
      (%check (%signals-error-p
               (lambda ()
                 (cell-occupancy-bit domain
                                     (lambda (x y z)
                                       (declare (ignore x y z))
                                       2)
                                     0 0 0)))))))

(defun %star-cell-index (site x y z)
  "Return the sample index of an incident cell, or NIL.  Test helper."
  (block absent
    (let ((coordinates (vector x y z))
          (anchors (vector (site-x site) (site-y site) (site-z site)))
          (normal-mask (logandc2 +cell-extent+ (site-extent site)))
          (sample 0) (position 0))
      (dotimes (axis-number 3 sample)
        (let ((coordinate (aref coordinates axis-number))
              (anchor (aref anchors axis-number)))
          (if (logbitp axis-number normal-mask)
              (cond ((= coordinate (1- anchor)))
                    ((= coordinate anchor)
                     (setf sample (logior sample (ash 1 position))))
                    (t (return-from absent nil)))
              (unless (= coordinate anchor)
                (return-from absent nil))))
        (when (logbitp axis-number normal-mask) (incf position))))))

(defun %star-occupancy-function (site occupancy-mask)
  (lambda (x y z)
    (let ((sample (%star-cell-index site x y z)))
      (if (and sample (logbitp sample occupancy-mask)) 1 0))))

(defun %find-local-edge (domain face target-edge)
  (dolist (edge '(:u-low :u-high :v-low :v-high))
    (when (= (site-geometry (face-edge-site domain face edge))
             (site-geometry target-edge))
      (return edge))))

(defun %find-local-corner (domain face target-vertex)
  (dolist (corner '(:low-low :low-high :high-low :high-high))
    (when (= (site-geometry (face-corner-site domain face corner))
             (site-geometry target-vertex))
      (return corner))))

(defun %faces-containing-vertex (domain vertex)
  (let ((faces '()))
    (dolist (extent (list +xy-face-extent+ +xz-face-extent+
                          +yz-face-extent+))
      (let ((prototype (make-site domain (site-x vertex) (site-y vertex)
                                  (site-z vertex) extent 1)))
        (multiple-value-bind (u v) (%face-tangent-indices prototype)
          (dotimes (u-side 2)
            (dotimes (v-side 2)
              (multiple-value-bind (ux uy uz)
                  (%axis-offset u (if (zerop u-side) 0 -1))
                (multiple-value-bind (vx vy vz)
                    (%axis-offset v (if (zerop v-side) 0 -1))
                  (push (make-site domain
                                   (+ (site-x vertex) ux vx)
                                   (+ (site-y vertex) uy vy)
                                   (+ (site-z vertex) uz vz)
                                   extent 1)
                        faces))))))))
    faces))

(defun %test-shape-and-closure ()
  (%with-test-section ("shape words, incidences, and face closure")
    ;; Corner ternary coding is exact for every direction and both reaches.
    (loop for qx from -1 to 1 do
      (loop for qy from -1 to 1 do
        (loop for qz from -1 to 1 do
          (dolist (reach '(1/2 2/3))
            (let ((code (encode-corner-code qx qy qz reach)))
              (multiple-value-bind (rx ry rz rr) (decode-corner-code code)
                (%check (and (= qx rx) (= qy ry) (= qz rz)
                             (= reach rr)))))))))
    (let* ((stars '(#x00 #x5a #xa5 #xff))
           (word (apply #'pack-shape-word stars)))
      (%check (shape-word-valid-p word))
      (%check (equal (multiple-value-list (unpack-shape-word word))
                     stars))
      (%check (shape-word-valid-p #xffffffff))
      (%check (not (shape-word-valid-p -1))))
    (let ((domain (make-world-domain :x-bits 6 :y-bits 6)))
      ;; Every exposed incidence of an edge reconstructs the literal moment Q.
      (dolist (extent (list +x-edge-extent+ +y-edge-extent+ +z-edge-extent+))
        (let ((edge (make-site domain 20 20 20 extent 1)))
          (dotimes (mask 16)
            (let ((k (logcount mask)))
              (when (<= 1 k 3)
                (let ((occupancy (%star-occupancy-function edge mask))
                      (incidences 0))
                  (multiple-value-bind (mx my mz qx qy qz reach)
                      (classify-site-star domain edge occupancy)
                    (declare (ignore mx my mz))
                    (multiple-value-bind (dx dy dz)
                        (site-displacement domain edge occupancy 1/5)
                      (%check (and (= dx (* qx reach 1/5))
                                   (= dy (* qy reach 1/5))
                                   (= dz (* qz reach 1/5)))))
                    (dotimes (axis-number 3)
                      (unless (logbitp axis-number extent)
                        (let ((axis (index-axis axis-number)))
                          (dolist (coface
                                   (list (site-coface-forward domain edge axis)
                                         (site-coface-backward domain edge axis)))
                            (let ((face (orient-face-outward
                                         domain (site-geometry coface) occupancy)))
                              (when face
                                (incf incidences)
                                (let* ((local-edge
                                         (%find-local-edge domain face edge))
                                       (shape
                                         (face-shape-word
                                          domain face occupancy))
                                       (code
                                         (shape-edge-code
                                          face shape local-edge)))
                                  (%check local-edge)
                                  (%check (= code (ecase k
                                                    (1 +edge-convex+)
                                                    (2 +edge-balanced+)
                                                    (3 +edge-concave+))))
                                  (multiple-value-bind (dqx dqy dqz)
                                      (decode-face-edge-direction
                                       face local-edge code)
                                    (%check (and (= qx dqx)
                                                 (= qy dqy)
                                                 (= qz dqz))))))))))))
                    (%check (plusp incidences))))))))
      ;; Every exposed face incidence of a vertex stores the same world code.
      (let ((vertex (make-site domain 20 20 20 +vertex-extent+ 1)))
        (loop for mask from 1 below 255 do
          (let ((occupancy (%star-occupancy-function vertex mask))
                (incidences 0))
            (multiple-value-bind (mx my mz qx qy qz reach)
                (classify-site-star domain vertex occupancy)
              (declare (ignore mx my mz))
              (let ((expected (encode-corner-code qx qy qz reach)))
                (dolist (geometric-face (%faces-containing-vertex domain vertex))
                  (let ((face (orient-face-outward
                               domain geometric-face occupancy)))
                    (when face
                      (incf incidences)
                      (let* ((corner (%find-local-corner domain face vertex))
                             (shape (face-shape-word domain face occupancy)))
                        (%check corner)
                        (%check (= mask
                                   (shape-corner-star-mask shape corner)))
                        (%check (= expected
                                   (shape-corner-code shape corner)))))))
                (%check (plusp incidences)))))))
      ;; Face records round-trip the exact four-u32 ABI, including the compact
      ;; normal-discontinuity mask consumed by construction rendering.
      (let* ((face (make-site domain 3 4 5 +xy-face-extent+ -1))
             (shape (pack-shape-word #x12 #x34 #x56 #x78))
             (record (make-face-record domain face shape 13 #x15555)))
        (%check (= (length record) +face-record-word-count+))
        (%check (= (* (length record) 4) +face-record-byte-size+))
        (multiple-value-bind
              (loaded-face loaded-shape loaded-stock loaded-construction)
            (load-face-record record 0 domain)
          (%check (and (= face loaded-face)
                       (= shape loaded-shape)
                       (= 13 loaded-stock)
                       (= #x15555 loaded-construction))))
        (setf (aref record +face-record-construction-word+) (ash 1 29))
        (%check (%signals-error-p
                 (lambda () (load-face-record record 0 domain))))))))

(defun %boundary-point-key (domain face point-index)
  (if (< point-index 16)
      (multiple-value-bind (i j) (floor point-index 4)
        (let ((ib (or (= i 0) (= i 3)))
              (jb (or (= j 0) (= j 3))))
          (cond
            ((and ib jb)
             (list :vertex-center
                   (site-geometry
                    (face-corner-site
                     domain face (%corner-point-data i j)))))
            ((or ib jb)
             (multiple-value-bind (edge parameter) (%edge-point-data i j)
               (list :edge
                     (site-geometry (face-edge-site domain face edge))
                     parameter)))
            (t nil))))
      (multiple-value-bind (corner-index tangent-index)
          (floor (- point-index 16) 2)
        (let* ((corner (%index-corner corner-index))
               (tangent (if (zerop tangent-index) :u :v))
               (vertex (face-corner-site domain face corner)))
          (multiple-value-bind (axis-number direction)
              (%corner-half-edge-data face corner tangent)
            (list :vertex-half (site-geometry vertex)
                  axis-number direction))))))

(defun %test-realization-closure ()
  (%with-test-section ("reference face realization")
    ;; Exhaust every occupancy star around one vertex.  Any boundary point
    ;; requested by more than one exposed face must be bit-identical.
    (let* ((domain (make-world-domain :x-bits 6 :y-bits 6))
           (vertex (make-site domain 20 20 20 +vertex-extent+ 1))
           (width 1/5)
           (duplicate-count 0))
      (dotimes (mask 256)
        (let ((occupancy (%star-occupancy-function vertex mask))
              (seen '()))
          (dolist (geometric-face (%faces-containing-vertex domain vertex))
            (let ((face (orient-face-outward
                         domain geometric-face occupancy)))
              (when face
                (let* ((shape (face-shape-word domain face occupancy))
                       (points (realize-face-patch
                                domain face shape width)))
                  (dotimes (point-index +face-point-count+)
                    (let ((key (%boundary-point-key
                                domain face point-index)))
                      (when key
                        (let* ((base (* 3 point-index))
                               (position
                                 (list (aref points base)
                                       (aref points (+ base 1))
                                       (aref points (+ base 2))))
                               (old (assoc key seen :test #'equal)))
                          (if old
                              (progn
                                (incf duplicate-count)
                                (%check (every #'eql position (cdr old))))
                              (push (cons key position) seen))))))))))))
      (%check (plusp duplicate-count)))
    ;; Exhaust the actual 3D triangles, not merely the parameter template.
    ;; Every exposed incidence of every vertex star must remain nondegenerate
    ;; and outward-wound after the arc endpoints move.
    (let* ((domain (make-world-domain :x-bits 6 :y-bits 6))
           (vertex (make-site domain 20 20 20 +vertex-extent+ 1))
           (width 1/5)
           (face-count 0)
           (triangle-count 0))
      (dotimes (mask 256)
        (let ((occupancy (%star-occupancy-function vertex mask)))
          (dolist (geometric-face (%faces-containing-vertex domain vertex))
            (let ((face (orient-face-outward
                         domain geometric-face occupancy)))
              (when face
                (incf face-count)
                (let* ((shape (face-shape-word domain face occupancy))
                       (points (realize-face-patch
                                domain face shape width))
                       (indices (if (site-positive-p face)
                                    *positive-face-index-template*
                                    *negative-face-index-template*)))
                  (%check (typep (face-construction-mask
                                  domain face shape width)
                                 '(unsigned-byte 29)))
                  (multiple-value-bind (fx fy fz)
                      (face-oriented-normal face)
                    (dotimes (triangle 26)
                      (incf triangle-count)
                      (multiple-value-bind (nx ny nz)
                          (%triangle-unit-normal points indices triangle)
                        (let ((facing (+ (* nx fx) (* ny fy) (* nz fz))))
                          (%check (plusp facing)
                                  (format nil
                                          "mask #x~2,'0X face ~S triangle ~D facing ~F"
                                          mask face triangle facing))))))))))))
      (%check (plusp face-count))
      (%check (= triangle-count (* face-count 26))))
    ;; Integrate the same closure rule with a six-face surface chain.
    (let* ((domain (make-world-domain :x-bits 6 :y-bits 6))
           (cx 20) (cy 20) (cz 20)
           (cell (make-site domain cx cy cz +cell-extent+ 1))
           (solid (%chain-from-sites domain (list cell)))
           (surface (surface-chain solid))
           (occupancy (lambda (x y z)
                        (if (and (= x cx) (= y cy) (= z cz)) 1 0)))
           (seen '())
           (duplicate-count 0)
           (width 1/5))
      (%check (= (chain-count surface) 6))
      (map-chain
       (lambda (face)
         (let* ((shape (face-shape-word domain face occupancy))
                (points (realize-face-patch domain face shape width)))
           (dolist (edge '(:u-low :u-high :v-low :v-high))
             (%check (= (shape-edge-code face shape edge) +edge-convex+)))
           (dolist (corner '(:low-low :low-high :high-low :high-high))
             (multiple-value-bind (qx qy qz reach)
                 (decode-corner-code (shape-corner-code shape corner))
               (declare (ignore qx qy qz))
               (%check (= reach 2/3))))
           (dotimes (point-index +face-point-count+)
             (let ((key (%boundary-point-key domain face point-index)))
               (when key
                 (let* ((base (* 3 point-index))
                        (position (list (aref points base)
                                        (aref points (+ base 1))
                                        (aref points (+ base 2))))
                        (old (assoc key seen :test #'equal)))
                   (if old
                       (progn
                         (incf duplicate-count)
                         (%check (every #'eql position (cdr old))))
                       (push (cons key position) seen))))))))
       surface)
      (%check (plusp duplicate-count))
      (%check (%signals-error-p
               (lambda ()
                 (realize-face-patch
                  domain (aref (chain-sites surface) 0)
                  (face-shape-word domain
                                   (aref (chain-sites surface) 0)
                                   occupancy)
                  1/2)))))))

(defun %point-param-coordinates (index)
  "Return exact doubled patch coordinates for topology tests."
  (if (< index 16)
      (multiple-value-bind (i j) (floor index 4)
        (values (* 2 i) (* 2 j)))
      (multiple-value-bind (corner-index tangent-index)
          (floor (- index 16) 2)
        (let* ((corner (%index-corner corner-index))
               (tangent (if (zerop tangent-index) :u :v))
               (u-high (%corner-high-p corner :u))
               (v-high (%corner-high-p corner :v)))
          (values (if (eq tangent :u)
                      (if u-high 5 1)
                      (if u-high 6 0))
                  (if (eq tangent :v)
                      (if v-high 5 1)
                      (if v-high 6 0)))))))

(defun %triangle-area2 (a b c)
  (multiple-value-bind (au av) (%point-param-coordinates a)
    (multiple-value-bind (bu bv) (%point-param-coordinates b)
      (multiple-value-bind (cu cv) (%point-param-coordinates c)
        (- (* (- bu au) (- cv av))
           (* (- bv av) (- cu au)))))))

(defun %test-index-templates ()
  (%with-test-section ("raster index templates")
    (let ((positive (positive-face-index-template))
          (negative (negative-face-index-template)))
      (%check (= (length positive) +face-index-count+))
      (%check (= (length negative) +face-index-count+))
      (%check (loop for index across positive
                    always (< -1 index +face-point-count+)))
      (%check (loop for index across negative
                    always (< -1 index +face-point-count+)))
      (dotimes (triangle 26)
        (let ((base (* triangle 3)))
          (%check (= (aref negative base) (aref positive base)))
          (%check (= (aref negative (+ base 1))
                     (aref positive (+ base 2))))
          (%check (= (aref negative (+ base 2))
                     (aref positive (+ base 1))))
          (%check (plusp (%triangle-area2
                          (aref positive base)
                          (aref positive (+ base 1))
                          (aref positive (+ base 2)))))
          (%check (minusp (%triangle-area2
                           (aref negative base)
                           (aref negative (+ base 1))
                           (aref negative (+ base 2)))))))
      (let* ((domain (make-world-domain :x-bits 6 :y-bits 6))
             (face (make-site domain 10 10 10 +xy-face-extent+ 1))
             (flat-shape (pack-shape-word #x0f #x0f #x0f #x0f))
             (bent-shape (pack-shape-word #x0e #x0f #x0f #x0f)))
        (%check (zerop (face-construction-mask
                        domain face flat-shape 0.11d0)))
        (%check (plusp (face-construction-mask
                        domain face bent-shape 0.11d0))))
      ;; The indexed disk uses every point, 29 internal edges, and 20 boundary
      ;; edges.  No triangle repeats a corner or overlaps by reversed winding.
      (let ((incidences (make-hash-table :test #'equal))
            (used '()))
        (dotimes (triangle 26)
          (let* ((base (* triangle 3))
                 (a (aref positive base))
                 (b (aref positive (+ base 1)))
                 (c (aref positive (+ base 2))))
            (%check (= 3 (length (remove-duplicates (list a b c)))))
            (setf used (list* a b c used))
            (dolist (edge (list (%ordered-edge a b)
                                (%ordered-edge b c)
                                (%ordered-edge c a)))
              (incf (gethash edge incidences 0)))))
        (%check (= +face-point-count+ (length (remove-duplicates used))))
        (%check (= 29 (loop for count being the hash-values of incidences
                            count (= count 2))))
        (%check (= 20 (loop for count being the hash-values of incidences
                            count (= count 1))))
        (%check (loop for edge across *construction-edge-pairs*
                      always (= 2 (gethash edge incidences 0))))))))

(defun run-luft-tests (&key (stream *standard-output*))
  "Run the executable topology, representation, classification, and ABI tests.

Signals immediately on failure.  On success, return T and the check count."
  (let ((*luft-test-count* 0)
        (*luft-test-section* nil))
    (%test-sites-and-topology)
    (%test-chains)
    (%test-classifier)
    (%test-shape-and-closure)
    (%test-realization-closure)
    (%test-index-templates)
    (when stream
      (format stream "~&LUFT: ~D checks passed.~%" *luft-test-count*))
    (values t *luft-test-count*)))
