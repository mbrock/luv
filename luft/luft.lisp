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
   #:+shape-edge-field-bits+ #:+shape-corner-field-bits+
   #:+shape-u-low-edge-shift+ #:+shape-u-high-edge-shift+
   #:+shape-v-low-edge-shift+ #:+shape-v-high-edge-shift+
   #:+shape-low-low-corner-shift+ #:+shape-low-high-corner-shift+
   #:+shape-high-low-corner-shift+ #:+shape-high-high-corner-shift+
   #:+corner-direction-mask+ #:+corner-two-thirds-mask+
   #:face-tangent-axes #:face-oriented-normal #:orient-face-outward
   #:face-edge-site #:face-corner-site
   #:encode-corner-code #:decode-corner-code
   #:pack-shape-word #:unpack-shape-word #:shape-word-valid-p
   #:shape-edge-code #:shape-corner-code
   #:decode-face-edge-direction #:face-shape-word
   ;; Reference realization.
   #:local-point-index #:realize-face-point #:realize-face-patch
   ;; Face-record ABI.
   #:+face-record-word-count+ #:+face-record-byte-size+
   #:+face-record-site-low-word+ #:+face-record-site-high-word+
   #:+face-record-shape-word+ #:+face-record-reserved-word+
   #:+decorated-site-stock-shift+
   #:decorate-site #:undecorated-site #:decorated-site-stock
   #:make-face-record-array #:store-face-record #:load-face-record
   #:make-face-record
   ;; Raster templates.
   #:+face-index-count+ #:make-face-index-template
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

(defun site-displacement (domain site occupancy width)
  (unless (and (realp width) (> width 0) (< width 1/2))
    (error "Chamfer width must satisfy 0 < w < 1/2, not ~S." width))
  (multiple-value-bind (mx my mz qx qy qz reach)
      (classify-site-star domain site occupancy)
    (declare (ignore mx my mz))
    (let ((scale (* width reach)))
      (values (* scale qx) (* scale qy) (* scale qz)))))

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

;; These constants are the renderer-visible u32 shape ABI.
(defconstant +shape-edge-field-bits+ 2)
(defconstant +shape-corner-field-bits+ 6)
(defconstant +shape-u-low-edge-shift+ 0)
(defconstant +shape-u-high-edge-shift+ 2)
(defconstant +shape-v-low-edge-shift+ 4)
(defconstant +shape-v-high-edge-shift+ 6)
(defconstant +shape-low-low-corner-shift+ 8)
(defconstant +shape-low-high-corner-shift+ 14)
(defconstant +shape-high-low-corner-shift+ 20)
(defconstant +shape-high-high-corner-shift+ 26)
(defconstant +corner-direction-mask+ #b11111)
(defconstant +corner-two-thirds-mask+ #b100000)

(defun %edge-shift (edge)
  (ecase edge
    (:u-low +shape-u-low-edge-shift+)
    (:u-high +shape-u-high-edge-shift+)
    (:v-low +shape-v-low-edge-shift+)
    (:v-high +shape-v-high-edge-shift+)))
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

(defun pack-shape-word (u-low u-high v-low v-high
                        low-low low-high high-low high-high)
  (%check-edge-code u-low) (%check-edge-code u-high)
  (%check-edge-code v-low) (%check-edge-code v-high)
  (%check-corner-code low-low) (%check-corner-code low-high)
  (%check-corner-code high-low) (%check-corner-code high-high)
  (logior (ash u-low (%edge-shift :u-low))
          (ash u-high (%edge-shift :u-high))
          (ash v-low (%edge-shift :v-low))
          (ash v-high (%edge-shift :v-high))
          (ash low-low (%corner-shift :low-low))
          (ash low-high (%corner-shift :low-high))
          (ash high-low (%corner-shift :high-low))
          (ash high-high (%corner-shift :high-high))))

(defun shape-edge-code (word edge)
  (check-type word (unsigned-byte 32))
  (%check-edge-code (ldb (byte +shape-edge-field-bits+ (%edge-shift edge)) word)))
(defun shape-corner-code (word corner)
  (check-type word (unsigned-byte 32))
  (%check-corner-code (ldb (byte +shape-corner-field-bits+ (%corner-shift corner)) word)))

(defun unpack-shape-word (word)
  (values (shape-edge-code word :u-low)
          (shape-edge-code word :u-high)
          (shape-edge-code word :v-low)
          (shape-edge-code word :v-high)
          (shape-corner-code word :low-low)
          (shape-corner-code word :low-high)
          (shape-corner-code word :high-low)
          (shape-corner-code word :high-high)))

(defun shape-word-valid-p (thing)
  (and (typep thing '(unsigned-byte 32))
       (handler-case (progn (unpack-shape-word thing) t)
         (error () nil))))

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

(defun %classify-edge-code (domain edge occupancy)
  (multiple-value-bind (mx my mz qx qy qz reach k)
      (classify-site-star domain edge occupancy)
    (declare (ignore mx my mz qx qy qz reach))
    (ecase k (1 +edge-convex+) (2 +edge-balanced+) (3 +edge-concave+))))

(defun %classify-corner-code (domain vertex occupancy)
  (multiple-value-bind (mx my mz qx qy qz reach)
      (classify-site-star domain vertex occupancy)
    (declare (ignore mx my mz))
    (encode-corner-code qx qy qz reach)))

(defun face-shape-word (domain face occupancy)
  "Classify exactly four edge sites and four vertex sites for FACE."
  (%require-face domain face)
  (unless (and (= 1 (%face-normal-side-occupancy domain face occupancy -1))
               (= 0 (%face-normal-side-occupancy domain face occupancy 1)))
    (error "Face ~S is not oriented from solid toward air." face))
  (pack-shape-word
   (%classify-edge-code domain (face-edge-site domain face :u-low) occupancy)
   (%classify-edge-code domain (face-edge-site domain face :u-high) occupancy)
   (%classify-edge-code domain (face-edge-site domain face :v-low) occupancy)
   (%classify-edge-code domain (face-edge-site domain face :v-high) occupancy)
   (%classify-corner-code domain (face-corner-site domain face :low-low) occupancy)
   (%classify-corner-code domain (face-corner-site domain face :low-high) occupancy)
   (%classify-corner-code domain (face-corner-site domain face :high-low) occupancy)
   (%classify-corner-code domain (face-corner-site domain face :high-high) occupancy)))

;;; ---------------------------------------------------------------------------
;;; CPU reference realization of the sixteen face points

(defun local-point-index (i j)
  (check-type i (integer 0 3))
  (check-type j (integer 0 3))
  (+ (* 4 i) j))

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
              (vertex (face-corner-site domain face corner)))
         (multiple-value-setq (bx by bz) (%double-anchor vertex))
         (multiple-value-setq (qx qy qz reach)
           (decode-corner-code (shape-corner-code shape-word corner)))))
      (t
       (multiple-value-bind (edge parameter) (%edge-point-data i j)
         (let ((edge-site (face-edge-site domain face edge)))
           (multiple-value-setq (bx by bz) (%double-anchor edge-site))
           (multiple-value-setq (bx by bz)
             (%offset-point bx by bz (%edge-axis-index edge-site)
                            (%lambda-coordinate parameter w)))
           (multiple-value-setq (qx qy qz)
             (decode-face-edge-direction
              face edge (shape-edge-code shape-word edge)))))))
    (let ((scale (* w (coerce reach 'double-float))))
      (values (+ bx (* scale qx))
              (+ by (* scale qy))
              (+ bz (* scale qz))))))

(defun realize-face-patch (domain face shape-word width)
  "Return sixteen XYZ points as one 48-element unboxed double vector."
  (%require-face domain face)
  (unless (shape-word-valid-p shape-word)
    (error "Invalid shape word ~S." shape-word))
  (let ((points (make-array 48 :element-type 'double-float)))
    (dotimes (i 4 points)
      (dotimes (j 4)
        (let ((base (* 3 (local-point-index i j))))
          (multiple-value-bind (x y z)
              (realize-face-point domain face shape-word width i j)
            (setf (aref points base) x
                  (aref points (+ base 1)) y
                  (aref points (+ base 2)) z)))))))

;;; ---------------------------------------------------------------------------
;;; Fixed 16-byte renderer-facing face-record ABI

(defconstant +face-record-word-count+ 4)
(defconstant +face-record-byte-size+ 16)
(defconstant +face-record-site-low-word+ 0)
(defconstant +face-record-site-high-word+ 1)
(defconstant +face-record-shape-word+ 2)
(defconstant +face-record-reserved-word+ 3)
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
                          &optional (stock 0))
  "Store low site word, high site word, shape word, and zero reserved word."
  (check-type words (array (unsigned-byte 32) (*)))
  (check-type record-index (integer 0 *))
  (%require-face domain face)
  (unless (shape-word-valid-p shape-word)
    (error "Invalid shape word ~S." shape-word))
  (check-type stock (unsigned-byte 4))
  (let ((base (* record-index +face-record-word-count+)))
    (when (> (+ base +face-record-word-count+) (length words))
      (error "Record ~D is outside a ~D-word array." record-index (length words)))
    (let ((decorated (decorate-site face stock)))
      (setf (aref words (+ base +face-record-site-low-word+))
            (ldb (byte 32 0) decorated)
            (aref words (+ base +face-record-site-high-word+))
            (ldb (byte 32 32) decorated)
            (aref words (+ base +face-record-shape-word+)) shape-word
            (aref words (+ base +face-record-reserved-word+)) 0)))
  words)

(defun load-face-record (words record-index domain)
  "Validate and return FACE, SHAPE-WORD, STOCK."
  (check-type words (array (unsigned-byte 32) (*)))
  (check-type record-index (integer 0 *))
  (let ((base (* record-index +face-record-word-count+)))
    (when (> (+ base +face-record-word-count+) (length words))
      (error "Record ~D is outside a ~D-word array." record-index (length words)))
    (unless (zerop (aref words (+ base +face-record-reserved-word+)))
      (error "Record ~D has nonzero reserved word." record-index))
    (let* ((decorated
             (logior (aref words (+ base +face-record-site-low-word+))
                     (ash (aref words
                                (+ base +face-record-site-high-word+))
                          32)))
           (face (undecorated-site decorated))
           (shape (aref words (+ base +face-record-shape-word+)))
           (stock (decorated-site-stock decorated)))
      (%require-face domain face)
      (unless (shape-word-valid-p shape)
        (error "Record ~D has invalid shape word ~S." record-index shape))
      (values face shape stock))))

(defun make-face-record (domain face shape-word &optional (stock 0))
  (let ((record (make-face-record-array 1)))
    (store-face-record record 0 domain face shape-word stock)
    record))

;;; ---------------------------------------------------------------------------
;;; Canonical indexed raster topology

(defconstant +face-index-count+ 54)

(defun make-face-index-template (polarity &key (diagonal :c00-c11))
  "Generate 18 consistently wound triangles over the 4x4 local point grid."
  (check-type polarity (member 1 -1))
  (check-type diagonal (member :c00-c11 :c10-c01))
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
          (let ((c00 (local-point-index i j))
                (c10 (local-point-index (1+ i) j))
                (c11 (local-point-index (1+ i) (1+ j)))
                (c01 (local-point-index i (1+ j))))
            (ecase diagonal
              (:c00-c11
               (emit c00 c10 c11)
               (emit c00 c11 c01))
              (:c10-c01
               (emit c00 c10 c01)
               (emit c10 c11 c01)))))))
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
    (let* ((c0 (encode-corner-code 0 0 0 1/2))
           (c1 (encode-corner-code -1 1 0 2/3))
           (c2 (encode-corner-code 1 -1 1 1/2))
           (c3 (encode-corner-code 1 1 -1 2/3))
           (word (pack-shape-word 0 1 2 0 c0 c1 c2 c3)))
      (%check (shape-word-valid-p word))
      (%check (equal (multiple-value-list (unpack-shape-word word))
                     (list 0 1 2 0 c0 c1 c2 c3)))
      (%check (not (shape-word-valid-p (logior word #b11))))
      (%check (not (shape-word-valid-p
                    (dpb 27 (byte 5 (%corner-shift :low-low)) word)))))
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
                    (declare (ignore mx my mz reach))
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
                                       (code (ecase k
                                               (1 +edge-convex+)
                                               (2 +edge-balanced+)
                                               (3 +edge-concave+))))
                                  (%check local-edge)
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
                        (%check (= expected
                                   (shape-corner-code shape corner)))))))
                (%check (plusp incidences)))))))
      ;; Face records round-trip the exact four-u32 ABI and reject revision data.
      (let* ((face (make-site domain 3 4 5 +xy-face-extent+ -1))
             (corner (encode-corner-code 0 0 0 1/2))
             (shape (pack-shape-word 0 1 2 0
                                     corner corner corner corner))
             (record (make-face-record domain face shape 13)))
        (%check (= (length record) +face-record-word-count+))
        (%check (= (* (length record) 4) +face-record-byte-size+))
        (multiple-value-bind (loaded-face loaded-shape loaded-stock)
            (load-face-record record 0 domain)
          (%check (and (= face loaded-face)
                       (= shape loaded-shape)
                       (= 13 loaded-stock))))
        (setf (aref record +face-record-reserved-word+) 1)
        (%check (%signals-error-p
                 (lambda () (load-face-record record 0 domain))))))))

(defun %boundary-point-key (domain face i j)
  (let ((ib (or (= i 0) (= i 3)))
        (jb (or (= j 0) (= j 3))))
    (cond
      ((and ib jb)
       (list :vertex
             (site-geometry
              (face-corner-site domain face (%corner-point-data i j)))))
      ((or ib jb)
       (multiple-value-bind (edge parameter) (%edge-point-data i j)
         (list :edge
               (site-geometry (face-edge-site domain face edge))
               parameter)))
      (t nil))))

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
                  (dotimes (i 4)
                    (dotimes (j 4)
                      (let ((key (%boundary-point-key domain face i j)))
                        (when key
                          (let* ((base (* 3 (local-point-index i j)))
                                 (position
                                   (list (aref points base)
                                         (aref points (+ base 1))
                                         (aref points (+ base 2))))
                                 (old (assoc key seen :test #'equal)))
                            (if old
                                (progn
                                  (incf duplicate-count)
                                  (%check (every #'eql position (cdr old))))
                                (push (cons key position) seen)))))))))))))
      (%check (plusp duplicate-count)))
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
             (%check (= (shape-edge-code shape edge) +edge-convex+)))
           (dolist (corner '(:low-low :low-high :high-low :high-high))
             (multiple-value-bind (qx qy qz reach)
                 (decode-corner-code (shape-corner-code shape corner))
               (declare (ignore qx qy qz))
               (%check (= reach 2/3))))
           (dotimes (i 4)
             (dotimes (j 4)
               (let ((key (%boundary-point-key domain face i j)))
                 (when key
                   (let* ((base (* 3 (local-point-index i j)))
                          (position (list (aref points base)
                                          (aref points (+ base 1))
                                          (aref points (+ base 2))))
                          (old (assoc key seen :test #'equal)))
                     (if old
                         (progn
                           (incf duplicate-count)
                           (%check (every #'eql position (cdr old))))
                         (push (cons key position) seen)))))))))
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

(defun %index-u (index) (floor index 4))
(defun %index-v (index) (mod index 4))
(defun %triangle-area2 (a b c)
  (- (* (- (%index-u b) (%index-u a))
        (- (%index-v c) (%index-v a)))
     (* (- (%index-v b) (%index-v a))
        (- (%index-u c) (%index-u a)))))

(defun %test-index-templates ()
  (%with-test-section ("raster index templates")
    (let ((positive (positive-face-index-template))
          (negative (negative-face-index-template)))
      (%check (= (length positive) +face-index-count+))
      (%check (= (length negative) +face-index-count+))
      (%check (loop for index across positive always (<= 0 index 15)))
      (%check (loop for index across negative always (<= 0 index 15)))
      (dotimes (triangle 18)
        (let ((base (* triangle 3)))
          (%check (= (aref negative base) (aref positive base)))
          (%check (= (aref negative (+ base 1))
                     (aref positive (+ base 2))))
          (%check (= (aref negative (+ base 2))
                     (aref positive (+ base 1))))
          (%check (= (%triangle-area2
                      (aref positive base)
                      (aref positive (+ base 1))
                      (aref positive (+ base 2)))
                     1))
          (%check (= (%triangle-area2
                      (aref negative base)
                      (aref negative (+ base 1))
                      (aref negative (+ base 2)))
                     -1))))
      ;; Each consecutive pair shares a diagonal, not a boundary edge;
      ;; this excludes an equal-area overlap/gap pair as well as a bow tie.
      (dotimes (quad 9)
        (let* ((i (floor quad 3))
               (j (mod quad 3))
               (base (* quad 6))
               (c00 (local-point-index i j))
               (c10 (local-point-index (1+ i) j))
               (c11 (local-point-index (1+ i) (1+ j)))
               (c01 (local-point-index i (1+ j)))
               (allowed (list c00 c10 c11 c01))
               (first (loop for k from base below (+ base 3)
                            collect (aref positive k)))
               (second (loop for k from (+ base 3) below (+ base 6)
                             collect (aref positive k)))
               (used (append first second))
               (shared (intersection first second)))
          (%check (every (lambda (index) (member index allowed)) used))
          (%check (= (length (remove-duplicates used)) 4))
          (%check (= (length shared) 2))
          (%check (or (and (member c00 shared) (member c11 shared))
                      (and (member c10 shared) (member c01 shared))))
          (%check (= (+ (%triangle-area2
                         (aref positive base)
                         (aref positive (+ base 1))
                         (aref positive (+ base 2)))
                        (%triangle-area2
                         (aref positive (+ base 3))
                         (aref positive (+ base 4))
                         (aref positive (+ base 5))))
                     2)))))))

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
