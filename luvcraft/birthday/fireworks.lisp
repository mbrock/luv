;;; Fireworks: a great big show over the meadow at dusk.
;;;
;;; Contract:
;;;   (ADD-BIRTHDAY-FIREWORKS session &key origin) registers one scene
;;;   overlay running a continuous show: shells launch from near ORIGIN every
;;;   few seconds, ascend with a sparkling trail, and burst into sphere,
;;;   ring, and willow patterns of a few hundred sparks each.  The spark
;;;   population is simulated on the CPU in REFRESH-LUVCRAFT-OVERLAY
;;;   (gravity, drag, fade) and drawn as one instanced draw of small glowing
;;;   billboards whose radiance clears the bloom threshold, so the lens makes
;;;   them blaze.  Per-spark colour comes from the shell's palette.
;;;   (STOP-BIRTHDAY-FIREWORKS session) releases the overlay.
;;;
;;;   Shader sections live under (in-package #:luvcraft.shaders).
;;;
;;; The population lives in preallocated single-float columns inside a plain
;;; FIREWORK-SHOW structure, so the whole simulation steps -- and tests --
;;; without a GPU anywhere near it.  The per-frame work is one pop pass, one
;;; integrate-and-compact pass, and one instance-packing pass, all closed
;;; loops over specialized arrays that allocate nothing.  Chance never enters:
;;; every wobble, tilt, and palette is a hash of a counter, so a replayed
;;; show lands every shell in the same place.

(in-package #:luvcraft.birthday)

;;; ---------------------------------------------------------------------
;;; Deterministic dials
;;;
;;; The show wants variety, not surprise.  CL:RANDOM never appears here:
;;; every decision is a hash of the shell or spark counter and a small salt
;;; naming which decision is being made, folded down to a dial in [0,1).
;;; The arithmetic stays under a 32-bit mask, which SBCL compiles modularly,
;;; so no shell number ever conses a bignum.

(declaim (inline firework-mix))
(defun firework-mix (word)
  "One 32-bit avalanche round over WORD."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3)))
  (let* ((word (logand (* word 2654435761) #xffffffff))
         (word (logxor word (ash word -15)))
         (word (logand (* word 2246822519) #xffffffff))
         (word (logxor word (ash word -13))))
    word))

(declaim (ftype (function (fixnum (unsigned-byte 32)) single-float)
                firework-dial))
(defun firework-dial (index salt)
  "A deterministic dial in [0,1) for INDEX, distinct per SALT."
  (declare (type fixnum index) (type (unsigned-byte 32) salt)
           (optimize (speed 3)))
  (/ (float (firework-mix
             (logxor (logand (* (logand index #xffffffff) 2654435761)
                             #xffffffff)
                     (firework-mix (logand (+ salt 1013904223) #xffffffff))))
            1.0)
     4294967296.0))

;;; ---------------------------------------------------------------------
;;; The show's state: dense columns behind one semantic owner
;;;
;;; A spark has no identity worth an object: it is a row in fifteen parallel
;;; single-float lanes owned by the show.  Stable in-place compaction keeps
;;; the lanes in spawn order, so index zero is always the oldest survivor
;;; and "drop the oldest" is one leftward REPLACE per column.  Rockets are
;;; few enough to sit in eight fixed slots scanned whole.

(defconstant +firework-spark-capacity+ 4096
  "The most sparks alive at once; past it the oldest are dropped.")

(defconstant +firework-rocket-capacity+ 8
  "The most shells climbing at once, comfortably more than the schedule asks.")

(defparameter *firework-gravity* 5.0
  "Cells per second squared pulling sparks down.  Dreamier than the world's
own gravity on purpose: burst blooms should hang, and willows should droop
slowly enough to watch.")

(defparameter *firework-launch-interval* 2.5
  "Seconds between shells before the per-shell spread is added.")

(defparameter *firework-launch-spread* 1.5
  "How many extra seconds the launch dial can add to the interval.")

(defparameter *firework-trail-rate* 26.0
  "Trail sparks a climbing rocket sheds per second.")

(defstruct (firework-show (:constructor %make-firework-show) (:copier nil))
  "One continuous fireworks show: schedule, rockets, and spark columns.
Plain CPU state over preallocated arrays; it steps without a GPU."
  ;; Where shells rise from.
  (origin-x 0.0 :type single-float)
  (origin-y 0.0 :type single-float)
  (origin-z 0.0 :type single-float)
  ;; The schedule and the running counters every dial hangs off.
  (countdown 0.75 :type single-float)
  (shell-counter 0 :type fixnum)
  (sparkle-counter 0 :type fixnum)
  ;; What has happened so far, for tests and curiosity.
  (launch-count 0 :type fixnum)
  (burst-count 0 :type fixnum)
  (retired-count 0 :type fixnum)
  ;; The spark columns.
  (capacity +firework-spark-capacity+ :type fixnum)
  (spark-count 0 :type fixnum)
  (spark-x (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-y (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-z (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-vx (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-vy (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-vz (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-red (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-green (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-blue (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-size (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-glow (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-age (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-life (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (spark-drag (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  ;; Seconds until this spark pops into secondaries; zero means never.
  (spark-pop (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  ;; The rocket slots.
  (rocket-flag (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (rocket-x (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-y (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-z (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-vx (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-vy (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-vz (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-fuse (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-trail (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (rocket-pattern (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (rocket-palette (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (rocket-seed (make-array 0 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) (*))))

(defun make-firework-show (&key (origin '(0.0 0.0 0.0))
                                (capacity +firework-spark-capacity+))
  "Make a show ready to step, launching from near ORIGIN, a list (X Y Z)."
  (flet ((column () (make-array capacity :element-type 'single-float
                                         :initial-element 0.0))
         (slots () (make-array +firework-rocket-capacity+
                               :element-type 'single-float
                               :initial-element 0.0)))
    (%make-firework-show
     :origin-x (coerce (elt origin 0) 'single-float)
     :origin-y (coerce (elt origin 1) 'single-float)
     :origin-z (coerce (elt origin 2) 'single-float)
     :capacity capacity
     :spark-x (column) :spark-y (column) :spark-z (column)
     :spark-vx (column) :spark-vy (column) :spark-vz (column)
     :spark-red (column) :spark-green (column) :spark-blue (column)
     :spark-size (column) :spark-glow (column)
     :spark-age (column) :spark-life (column)
     :spark-drag (column) :spark-pop (column)
     :rocket-flag (make-array +firework-rocket-capacity+
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)
     :rocket-x (slots) :rocket-y (slots) :rocket-z (slots)
     :rocket-vx (slots) :rocket-vy (slots) :rocket-vz (slots)
     :rocket-fuse (slots) :rocket-trail (slots)
     :rocket-pattern (make-array +firework-rocket-capacity+
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)
     :rocket-palette (make-array +firework-rocket-capacity+
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)
     :rocket-seed (make-array +firework-rocket-capacity+
                              :element-type '(unsigned-byte 32)
                              :initial-element 0))))

;;; ---------------------------------------------------------------------
;;; Admitting sparks

(defun reserve-firework-room (show wanted)
  "Make room for WANTED sparks, dropping the oldest when the cap bites.
The columns are in spawn order, so dropping is one leftward shift each."
  (declare (type fixnum wanted) (optimize (speed 3)))
  (let* ((count (firework-show-spark-count show))
         (spill (- (+ count wanted) (firework-show-capacity show))))
    (declare (type fixnum count spill))
    (when (plusp spill)
      (macrolet ((shift-columns (&rest readers)
                   `(progn
                      ,@(loop for reader in readers
                              collect `(let ((column (,reader show)))
                                         (replace column column
                                                  :start2 spill
                                                  :end2 count))))))
        (shift-columns firework-show-spark-x firework-show-spark-y
                       firework-show-spark-z firework-show-spark-vx
                       firework-show-spark-vy firework-show-spark-vz
                       firework-show-spark-red firework-show-spark-green
                       firework-show-spark-blue firework-show-spark-size
                       firework-show-spark-glow firework-show-spark-age
                       firework-show-spark-life firework-show-spark-drag
                       firework-show-spark-pop))
      (incf (firework-show-retired-count show) spill)
      (setf (firework-show-spark-count show) (- count spill))))
  show)

(declaim (inline emit-firework-spark))
(defun emit-firework-spark (show x y z vx vy vz red green blue
                            size glow life drag pop)
  "Append one spark when a slot is free and say whether it was admitted.
Volleys call RESERVE-FIREWORK-ROOM first; strays simply yield at the cap."
  (declare (type single-float x y z vx vy vz red green blue
                 size glow life drag pop)
           (optimize (speed 3)))
  (let ((count (firework-show-spark-count show)))
    (declare (type fixnum count))
    (when (< count (firework-show-capacity show))
      (setf (aref (firework-show-spark-x show) count) x
            (aref (firework-show-spark-y show) count) y
            (aref (firework-show-spark-z show) count) z
            (aref (firework-show-spark-vx show) count) vx
            (aref (firework-show-spark-vy show) count) vy
            (aref (firework-show-spark-vz show) count) vz
            (aref (firework-show-spark-red show) count) red
            (aref (firework-show-spark-green show) count) green
            (aref (firework-show-spark-blue show) count) blue
            (aref (firework-show-spark-size show) count) size
            (aref (firework-show-spark-glow show) count) glow
            (aref (firework-show-spark-age show) count) 0.0
            (aref (firework-show-spark-life show) count) life
            (aref (firework-show-spark-drag show) count) drag
            (aref (firework-show-spark-pop show) count) pop
            (firework-show-spark-count show) (1+ count))
      t)))

;;; ---------------------------------------------------------------------
;;; Burst geometry and palettes

(declaim (inline firework-sphere-direction))
(defun firework-sphere-direction (k n)
  "The Kth of N points of a Fibonacci sphere: uniform without chance."
  (declare (type fixnum k n) (optimize (speed 3)))
  (let* ((y (- 1.0 (/ (* 2.0 (+ (float k 1.0) 0.5)) (float n 1.0))))
         (ring (sqrt (max 0.0 (- 1.0 (* y y)))))
         (theta (* 2.3999632 (float k 1.0))))
    (values (* ring (cos theta)) y (* ring (sin theta)))))

(defun firework-spark-color (palette k n seed)
  "The linear RGB the Kth of N sparks wears under PALETTE, dialed by SEED."
  (declare (type fixnum palette k n seed) (optimize (speed 3)))
  (let ((dial (firework-dial (+ seed k) 11)))
    (ecase palette
      (0 ;; Gold, from ember-orange up toward straw.
       (values 1.0 (+ 0.6 (* 0.24 dial)) (+ 0.2 (* 0.18 dial))))
      (1 ;; Silver-white, cool and slightly blued.
       (values (+ 0.84 (* 0.1 dial)) (+ 0.9 (* 0.06 dial)) 1.0))
      (2 ;; Pink and cyan, alternating around the shell.
       (if (evenp k)
           (values 1.0 0.38 0.62)
           (values 0.32 0.88 1.0)))
      (3 ;; Green and violet, likewise.
       (if (evenp k)
           (values 0.42 1.0 0.5)
           (values 0.72 0.42 1.0)))
      (4 ;; Rainbow: the whole wheel once around the shell.
       (let ((hue (/ (float k 1.0) (float (max n 1) 1.0))))
         (values (+ 0.5 (* 0.5 (cos (* 6.2831855 hue))))
                 (+ 0.5 (* 0.5 (cos (* 6.2831855 (- hue 0.33333334)))))
                 (+ 0.5 (* 0.5 (cos (* 6.2831855 (- hue 0.6666667)))))))))))

(defun burst-firework-peony (show x y z vx vy vz palette seed n speed pop-p)
  "A sphere of sparks: the classic bloom.  With POP-P each spark carries a
fuse of its own and pops into a handful of hot secondaries -- the double
burst."
  (declare (type single-float x y z vx vy vz speed)
           (type fixnum palette seed n)
           (optimize (speed 3)))
  (reserve-firework-room show n)
  (dotimes (k n)
    (multiple-value-bind (dx dy dz) (firework-sphere-direction k n)
      (let ((v (* speed (+ 0.85 (* 0.3 (firework-dial (+ seed k) 31))))))
        (multiple-value-bind (red green blue)
            (firework-spark-color palette k n seed)
          (emit-firework-spark
           show x y z
           (+ (* dx v) (* vx 0.2)) (+ (* dy v) (* vy 0.2))
           (+ (* dz v) (* vz 0.2))
           red green blue
           0.095 (+ 5.0 (* 2.0 (firework-dial (+ seed k) 32)))
           (+ 1.6 (* 0.8 (firework-dial (+ seed k) 33)))
           0.85
           (if pop-p
               (+ 0.5 (* 0.3 (firework-dial (+ seed k) 34)))
               0.0)))))))

(defun burst-firework-ring (show x y z vx vy vz palette seed)
  "A tilted circle of sparks, evenly spaced, all at one speed."
  (declare (type single-float x y z vx vy vz) (type fixnum palette seed)
           (optimize (speed 3)))
  (let* ((n 96)
         (tilt (+ 0.4 (* 0.5 (firework-dial seed 41))))
         (spin (* 6.2831855 (firework-dial seed 42)))
         ;; The ring's normal, then an orthonormal basis in its plane.
         ;; TILT stays well away from zero, so the cross with world up
         ;; never degenerates.
         (nx (* (sin tilt) (cos spin)))
         (ny (cos tilt))
         (nz (* (sin tilt) (sin spin)))
         (len (sqrt (+ (* nx nx) (* nz nz))))
         (ux (/ (- nz) len))
         (uz (/ nx len))
         (wx (* ny uz))
         (wy (- (* nz ux) (* nx uz)))
         (wz (- (* ny ux))))
    (reserve-firework-room show n)
    (dotimes (k n)
      (let* ((theta (/ (* 6.2831855 (float k 1.0)) (float n 1.0)))
             (ct (cos theta))
             (st (sin theta))
             (v (* 7.0 (+ 0.95 (* 0.1 (firework-dial (+ seed k) 43))))))
        (multiple-value-bind (red green blue)
            (firework-spark-color palette k n seed)
          (emit-firework-spark
           show x y z
           (+ (* (+ (* ux ct) (* wx st)) v) (* vx 0.2))
           (+ (* (* wy st) v) (* vy 0.2))
           (+ (* (+ (* uz ct) (* wz st)) v) (* vz 0.2))
           red green blue
           0.09 (+ 5.5 (* 1.5 (firework-dial (+ seed k) 44)))
           (+ 1.7 (* 0.5 (firework-dial (+ seed k) 45)))
           0.8 0.0))))))

(defun burst-firework-willow (show x y z vx vy vz seed)
  "Fewer, slower, heavier sparks that drag hard and droop long: always
gold, because a willow is."
  (declare (type single-float x y z vx vy vz) (type fixnum seed)
           (optimize (speed 3)))
  (let ((n 130))
    (reserve-firework-room show n)
    (dotimes (k n)
      (multiple-value-bind (dx dy dz) (firework-sphere-direction k n)
        ;; Lift the sphere toward the sky so the droop has somewhere to
        ;; fall from.
        (let* ((ly (+ (* dy 0.55) 0.4))
               (len (sqrt (+ (* dx dx) (* ly ly) (* dz dz))))
               (v (/ (* 4.6 (+ 0.8 (* 0.4 (firework-dial (+ seed k) 46))))
                     len)))
          (multiple-value-bind (red green blue)
              (firework-spark-color 0 k n seed)
            (emit-firework-spark
             show x y z
             (+ (* dx v) (* vx 0.2)) (+ (* ly v) (* vy 0.2))
             (+ (* dz v) (* vz 0.2))
             red green blue
             0.08 (+ 3.6 (* 1.2 (firework-dial (+ seed k) 47)))
             (+ 3.2 (* 1.0 (firework-dial (+ seed k) 48)))
             1.5 0.0)))))))

(defun pop-firework-spark (show i)
  "The delight of a double shell: spark I quietly pops into a handful of
tiny hot sparks of its own, whitened past its parent's colour.  At the
cap the pop simply fizzles; nothing shifts under the caller's sweep."
  (declare (type fixnum i) (optimize (speed 3)))
  (let* ((x (aref (firework-show-spark-x show) i))
         (y (aref (firework-show-spark-y show) i))
         (z (aref (firework-show-spark-z show) i))
         (vx (* (aref (firework-show-spark-vx show) i) 0.35))
         (vy (* (aref (firework-show-spark-vy show) i) 0.35))
         (vz (* (aref (firework-show-spark-vz show) i) 0.35))
         (red (min 1.0 (+ (aref (firework-show-spark-red show) i) 0.3)))
         (green (min 1.0 (+ (aref (firework-show-spark-green show) i) 0.3)))
         (blue (min 1.0 (+ (aref (firework-show-spark-blue show) i) 0.3)))
         (n (incf (firework-show-sparkle-counter show))))
    (dotimes (k 5)
      (multiple-value-bind (dx dy dz) (firework-sphere-direction k 5)
        (emit-firework-spark
         show x y z
         (+ vx (* dx 2.1)) (+ vy (* dy 2.1)) (+ vz (* dz 2.1))
         red green blue
         0.065 6.5
         (+ 0.4 (* 0.3 (firework-dial (+ n k) 51)))
         1.8 0.0)))))

;;; ---------------------------------------------------------------------
;;; Shells: launching, climbing, bursting

(defun launch-firework-shell (show shell)
  "Send shell number SHELL up from near the origin, its tilt, fuse,
pattern, and palette all dialed off its number."
  (declare (type fixnum shell) (optimize (speed 3)))
  (let ((slot (loop for s below +firework-rocket-capacity+
                    when (zerop (aref (firework-show-rocket-flag show) s))
                      return s)))
    (when slot
      (incf (firework-show-launch-count show))
      (setf (aref (firework-show-rocket-flag show) slot) 1
            (aref (firework-show-rocket-x show) slot)
            (+ (firework-show-origin-x show)
               (* 1.5 (- (firework-dial shell 2) 0.5)))
            (aref (firework-show-rocket-y show) slot)
            (firework-show-origin-y show)
            (aref (firework-show-rocket-z show) slot)
            (+ (firework-show-origin-z show)
               (* 1.5 (- (firework-dial shell 3) 0.5)))
            (aref (firework-show-rocket-vx show) slot)
            (* 2.2 (- (firework-dial shell 4) 0.5))
            (aref (firework-show-rocket-vy show) slot)
            (+ 9.5 (* 2.5 (firework-dial shell 5)))
            (aref (firework-show-rocket-vz show) slot)
            (* 2.2 (- (firework-dial shell 6) 0.5))
            (aref (firework-show-rocket-fuse show) slot)
            (+ 1.55 (* 0.5 (firework-dial shell 7)))
            (aref (firework-show-rocket-trail show) slot) 0.0
            ;; The first shell is a classic peony; after that the dial
            ;; chooses, leaning peony.
            (aref (firework-show-rocket-pattern show) slot)
            (if (zerop shell)
                0
                (let ((d (firework-dial shell 8)))
                  (cond ((< d 0.45) 0)      ; peony
                        ((< d 0.65) 1)      ; ring
                        ((< d 0.85) 2)      ; willow
                        (t 3))))            ; double peony
            ;; Every seventh shell wears the rainbow, finale-ish.
            (aref (firework-show-rocket-palette show) slot)
            (if (= 6 (mod shell 7))
                4
                (min 3 (floor (* 4.0 (firework-dial shell 9)))))
            (aref (firework-show-rocket-seed show) slot)
            (firework-mix (logand (* (+ shell 1) 2654435761) #xffffffff))))))

(defun emit-firework-trail (show slot)
  "Shed one gold ember behind the rocket in SLOT."
  (declare (type fixnum slot) (optimize (speed 3)))
  (let* ((n (incf (firework-show-sparkle-counter show)))
         (jx (- (firework-dial n 21) 0.5))
         (jy (- (firework-dial n 22) 0.5))
         (jz (- (firework-dial n 23) 0.5)))
    (emit-firework-spark
     show
     (aref (firework-show-rocket-x show) slot)
     (aref (firework-show-rocket-y show) slot)
     (aref (firework-show-rocket-z show) slot)
     (+ (* (aref (firework-show-rocket-vx show) slot) 0.3) (* jx 1.6))
     (+ (* (aref (firework-show-rocket-vy show) slot) 0.2) (* jy 1.6))
     (+ (* (aref (firework-show-rocket-vz show) slot) 0.3) (* jz 1.6))
     1.0 0.78 0.45
     0.05 2.8
     (+ 0.3 (* 0.35 (firework-dial n 24)))
     2.6 0.0)))

(defun burst-firework-shell (show slot)
  "The rocket in SLOT has reached its fuse: bloom, and free the slot."
  (declare (type fixnum slot) (optimize (speed 3)))
  (let ((x (aref (firework-show-rocket-x show) slot))
        (y (aref (firework-show-rocket-y show) slot))
        (z (aref (firework-show-rocket-z show) slot))
        (vx (aref (firework-show-rocket-vx show) slot))
        (vy (aref (firework-show-rocket-vy show) slot))
        (vz (aref (firework-show-rocket-vz show) slot))
        (palette (aref (firework-show-rocket-palette show) slot))
        (seed (aref (firework-show-rocket-seed show) slot)))
    (incf (firework-show-burst-count show))
    (setf (aref (firework-show-rocket-flag show) slot) 0)
    (ecase (aref (firework-show-rocket-pattern show) slot)
      (0 (burst-firework-peony show x y z vx vy vz palette seed 240 6.2 nil))
      (1 (burst-firework-ring show x y z vx vy vz palette seed))
      (2 (burst-firework-willow show x y z vx vy vz seed))
      (3 (burst-firework-peony show x y z vx vy vz palette seed 110 6.5 t)))))

;;; ---------------------------------------------------------------------
;;; The step: schedule, rockets, sparks

(defun step-firework-schedule (show dt)
  (declare (type single-float dt) (optimize (speed 3)))
  (let ((countdown (- (firework-show-countdown show) dt)))
    (if (plusp countdown)
        (setf (firework-show-countdown show) countdown)
        (let ((shell (firework-show-shell-counter show)))
          (launch-firework-shell show shell)
          (setf (firework-show-shell-counter show) (1+ shell)
                (firework-show-countdown show)
                (+ (coerce *firework-launch-interval* 'single-float)
                   (* (coerce *firework-launch-spread* 'single-float)
                      (firework-dial (1+ shell) 1))))))))

(defun step-firework-rockets (show dt)
  (declare (type single-float dt) (optimize (speed 3)))
  (let ((gravity (coerce *firework-gravity* 'single-float))
        (rate (coerce *firework-trail-rate* 'single-float)))
    (dotimes (slot +firework-rocket-capacity+)
      (when (plusp (aref (firework-show-rocket-flag show) slot))
        (let ((fuse (- (aref (firework-show-rocket-fuse show) slot) dt)))
          (cond
            ((plusp fuse)
             (let* ((decay (max 0.0 (- 1.0 (* 0.25 dt))))
                    (vx (* (aref (firework-show-rocket-vx show) slot) decay))
                    (vy (- (* (aref (firework-show-rocket-vy show) slot)
                              decay)
                          (* gravity dt)))
                    (vz (* (aref (firework-show-rocket-vz show) slot)
                           decay)))
               (setf (aref (firework-show-rocket-vx show) slot) vx
                     (aref (firework-show-rocket-vy show) slot) vy
                     (aref (firework-show-rocket-vz show) slot) vz
                     (aref (firework-show-rocket-fuse show) slot) fuse)
               (incf (aref (firework-show-rocket-x show) slot) (* vx dt))
               (incf (aref (firework-show-rocket-y show) slot) (* vy dt))
               (incf (aref (firework-show-rocket-z show) slot) (* vz dt))
               ;; The trail: a fractional emission accumulator, so the
               ;; sparkle density does not depend on the frame rate.
               (let ((carry (+ (aref (firework-show-rocket-trail show) slot)
                               (* rate dt))))
                 (loop while (>= carry 1.0)
                       do (emit-firework-trail show slot)
                          (decf carry 1.0))
                 (setf (aref (firework-show-rocket-trail show) slot)
                       carry))))
            (t
             (burst-firework-shell show slot))))))))

(defun step-firework-sparks (show dt)
  (declare (type single-float dt) (optimize (speed 3)))
  ;; Pops first, over the population that began the frame: a popping spark
  ;; appends its secondaries past COUNT, and the integrator below picks the
  ;; newborns up in the same sweep.  Emission during this pass only ever
  ;; appends -- POP-FIREWORK-SPARK never shifts -- so the indices hold.
  (let ((count (firework-show-spark-count show))
        (pops (firework-show-spark-pop show)))
    (declare (type fixnum count))
    (dotimes (i count)
      (let ((pop (aref pops i)))
        (when (plusp pop)
          (let ((left (- pop dt)))
            (cond ((plusp left) (setf (aref pops i) left))
                  (t (setf (aref pops i) 0.0)
                     (pop-firework-spark show i))))))))
  ;; Age, integrate, and compact in place.  Survivors slide left, so the
  ;; columns stay in spawn order and index zero stays the oldest spark.
  (let ((xs (firework-show-spark-x show))
        (ys (firework-show-spark-y show))
        (zs (firework-show-spark-z show))
        (vxs (firework-show-spark-vx show))
        (vys (firework-show-spark-vy show))
        (vzs (firework-show-spark-vz show))
        (reds (firework-show-spark-red show))
        (greens (firework-show-spark-green show))
        (blues (firework-show-spark-blue show))
        (sizes (firework-show-spark-size show))
        (glows (firework-show-spark-glow show))
        (ages (firework-show-spark-age show))
        (lives (firework-show-spark-life show))
        (drags (firework-show-spark-drag show))
        (pops (firework-show-spark-pop show))
        (gravity (coerce *firework-gravity* 'single-float))
        (count (firework-show-spark-count show))
        (write 0))
    (declare (type fixnum count write))
    (dotimes (read count)
      (let ((age (+ (aref ages read) dt)))
        (when (< age (aref lives read))
          (let* ((decay (max 0.0 (- 1.0 (* (aref drags read) dt))))
                 (vx (* (aref vxs read) decay))
                 (vy (- (* (aref vys read) decay) (* gravity dt)))
                 (vz (* (aref vzs read) decay)))
            (setf (aref xs write) (+ (aref xs read) (* vx dt))
                  (aref ys write) (+ (aref ys read) (* vy dt))
                  (aref zs write) (+ (aref zs read) (* vz dt))
                  (aref vxs write) vx
                  (aref vys write) vy
                  (aref vzs write) vz
                  (aref reds write) (aref reds read)
                  (aref greens write) (aref greens read)
                  (aref blues write) (aref blues read)
                  (aref sizes write) (aref sizes read)
                  (aref glows write) (aref glows read)
                  (aref ages write) age
                  (aref lives write) (aref lives read)
                  (aref drags write) (aref drags read)
                  (aref pops write) (aref pops read))
            (incf write)))))
    (incf (firework-show-retired-count show) (- count write))
    (setf (firework-show-spark-count show) write)))

(defun step-firework-show (show dt)
  "Advance SHOW by DT seconds: schedule, fly, burst, age, retire.
Plain arithmetic over the show's own columns; safe without a GPU."
  (let ((dt (coerce dt 'single-float)))
    (step-firework-schedule show dt)
    (step-firework-rockets show dt)
    (step-firework-sparks show dt))
  show)

;;; ---------------------------------------------------------------------
;;; Packing the instance lanes
;;;
;;; Each instance is two vec4s: (center.xyz, radius) and (colour.rgb,
;;; intensity).  A young spark's intensity sits well above the lens's
;;; bright-pass threshold of 1.5, so the bloom chain does the blazing; the
;;; last three tenths of a life fade it out.  The climbing rocket heads
;;; ride along at the end as bright white-gold comets.

(defun fill-firework-instances (show data)
  "Pack the live population into DATA, stride eight floats per instance,
and return how many instances are ready to draw."
  (declare (type (simple-array single-float (*)) data)
           (optimize (speed 3)))
  (let ((count (firework-show-spark-count show))
        (xs (firework-show-spark-x show))
        (ys (firework-show-spark-y show))
        (zs (firework-show-spark-z show))
        (reds (firework-show-spark-red show))
        (greens (firework-show-spark-green show))
        (blues (firework-show-spark-blue show))
        (sizes (firework-show-spark-size show))
        (glows (firework-show-spark-glow show))
        (ages (firework-show-spark-age show))
        (lives (firework-show-spark-life show))
        (filled 0))
    (declare (type fixnum count filled))
    (dotimes (i count)
      (let* ((base (* filled 8))
             (worn (/ (aref ages i) (aref lives i)))
             (fade (min 1.0 (* (- 1.0 worn) 3.3333333))))
        (setf (aref data base) (aref xs i)
              (aref data (+ base 1)) (aref ys i)
              (aref data (+ base 2)) (aref zs i)
              (aref data (+ base 3)) (aref sizes i)
              (aref data (+ base 4)) (aref reds i)
              (aref data (+ base 5)) (aref greens i)
              (aref data (+ base 6)) (aref blues i)
              (aref data (+ base 7)) (* (aref glows i) fade))
        (incf filled)))
    (dotimes (slot +firework-rocket-capacity+)
      (when (plusp (aref (firework-show-rocket-flag show) slot))
        (let ((base (* filled 8)))
          (setf (aref data base)
                (aref (firework-show-rocket-x show) slot)
                (aref data (+ base 1))
                (aref (firework-show-rocket-y show) slot)
                (aref data (+ base 2))
                (aref (firework-show-rocket-z show) slot)
                (aref data (+ base 3)) 0.12
                (aref data (+ base 4)) 1.0
                (aref data (+ base 5)) 0.92
                (aref data (+ base 6)) 0.72
                (aref data (+ base 7)) 7.0)
          (incf filled))))
    filled))

;;; ---------------------------------------------------------------------
;;; The overlay: the show's seat in the session
;;;
;;; The overlay owns the GPU resources and the frame clock; the show inside
;;; it owns nothing but arrays.  Refresh runs before vertex building, so
;;; what it packs is on screen the same frame.

(defclass fireworks-overlay ()
  ((show :initarg :show :reader fireworks-overlay-show)
   (pipeline :initarg :pipeline :accessor fireworks-overlay-pipeline)
   (vertex-buffer :initarg :vertex-buffer
                  :accessor fireworks-overlay-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :accessor fireworks-overlay-instance-buffer)
   (instance-data :initarg :instance-data
                  :reader fireworks-overlay-instance-data)
   (instance-count :initform 0 :accessor fireworks-overlay-instance-count)
   (last-refresh :initform nil :accessor fireworks-overlay-last-refresh)))

(defmethod luvcraft::luvcraft-overlay-live-shader-pipelines
    ((overlay fireworks-overlay))
  (list (fireworks-overlay-pipeline overlay)))

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay fireworks-overlay) session)
  (declare (ignore session))
  (let* ((now (get-internal-real-time))
         (last (shiftf (fireworks-overlay-last-refresh overlay) now))
         (seconds (if last
                      (min 0.1 (/ (float (- now last) 1.0)
                                  internal-time-units-per-second))
                      0.0))
         (show (fireworks-overlay-show overlay)))
    (step-firework-show show seconds)
    (let ((count (fill-firework-instances
                  show (fireworks-overlay-instance-data overlay))))
      (setf (fireworks-overlay-instance-count overlay) count)
      ;; One queue write of the whole preallocated lane; the draw below
      ;; only reads the live prefix.
      (when (plusp count)
        (luv:write-buffer (fireworks-overlay-instance-buffer overlay)
                          (fireworks-overlay-instance-data overlay)))))
  overlay)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay fireworks-overlay) session pass surface-texture)
  (let ((count (fireworks-overlay-instance-count overlay)))
    (when (plusp count)
      (let ((frame (luvcraft::luvcraft-frame-state session surface-texture)))
        (luv:set-pipeline
         pass
         (luvcraft::live-shader-pipeline-native-pipeline
          (fireworks-overlay-pipeline overlay)))
        (luv:set-vertex-buffer pass 0 (fireworks-overlay-vertex-buffer overlay))
        (luv:set-vertex-buffer pass 1 (fireworks-overlay-instance-buffer
                                       overlay))
        (luv:set-bind-group pass 0 (luvcraft::luvcraft-frame-scene-bind-group
                                    frame))
        (luv:draw pass 6 count))))
  overlay)

(defmethod luvcraft:release-luvcraft-overlay ((overlay fireworks-overlay))
  (when (fireworks-overlay-pipeline overlay)
    (luvcraft::release-live-shader-pipeline
     (fireworks-overlay-pipeline overlay))
    (setf (fireworks-overlay-pipeline overlay) nil))
  (dolist (resource (list (fireworks-overlay-instance-buffer overlay)
                          (fireworks-overlay-vertex-buffer overlay)))
    (when resource (luv:destroy resource)))
  (setf (fireworks-overlay-instance-buffer overlay) nil
        (fireworks-overlay-vertex-buffer overlay) nil)
  (values))

(defun find-fireworks-overlay (session)
  (find-if (lambda (overlay) (typep overlay 'fireworks-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun default-firework-origin (session)
  "Where the show stands when nobody says: two dozen cells ahead of the
camera along the ground, so the first shell rises into view."
  (let* ((camera (luvcraft:luvcraft-session-camera session))
         (yaw (luvcraft:camera-yaw camera)))
    (list (+ (luvcraft:camera-x camera) (* 24.0 (sin yaw)))
          (- (luvcraft:camera-y camera) 2.0)
          (+ (luvcraft:camera-z camera) (* 24.0 (cos yaw))))))

(defun add-birthday-fireworks (session &key origin)
  "Start the show: register one scene overlay whose shells rise from near
ORIGIN, a list (X Y Z), ahead of the camera by default.  Returns the
overlay; calling again while a show runs returns the running one."
  (or (find-fireworks-overlay session)
      (let* ((device (luvcraft:luvcraft-session-device session))
             (show (make-firework-show
                    :origin (or origin (default-firework-origin session))))
             (vertex-data (luvcraft::make-world-text-quad-vertices))
             (instance-data
               (make-array (* 8 (+ (firework-show-capacity show)
                                   +firework-rocket-capacity+))
                           :element-type 'single-float
                           :initial-element 0.0))
             (vertex-buffer nil)
             (instance-buffer nil)
             (pipeline nil)
             (completed-p nil))
        (unwind-protect
             (progn
               (setf vertex-buffer
                     (luv:create
                      device
                      (luv:make-buffer-descriptor
                       :label "birthday spark quad"
                       :size (* 4 (length vertex-data))
                       :usage '(:vertex :copy-dst)))
                     instance-buffer
                     (luv:create
                      device
                      (luv:make-buffer-descriptor
                       :label "birthday spark instances"
                       :size (* 4 (length instance-data))
                       :usage '(:vertex :copy-dst)))
                     pipeline
                     (luvcraft::make-live-shader-pipeline
                      :role :birthday-spark
                      :vertex-role :birthday-spark
                      :label "birthday spark pipeline"
                      :device device
                      :layout
                      (luvcraft::live-shader-pipeline-layout
                       (luvcraft:luvcraft-session-block-pipeline session))
                      :vertex-buffers
                      '((:array-stride 12
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)))
                        (:array-stride 32 :step-mode :instance
                         :attributes
                         ((:shader-location 1 :offset 0 :format :float32x4)
                          (:shader-location 2 :offset 16
                           :format :float32x4))))
                      :target-format luvcraft::+luvcraft-scene-color-format+
                      :target-blend :premultiplied-alpha
                      :primitive '(:topology :triangle-list)
                      :depth-stencil
                      '(:format :depth32-float
                        :depth-write-enabled nil
                        :depth-compare :less)))
               (luv:write-buffer vertex-buffer vertex-data)
               (let ((overlay (make-instance 'fireworks-overlay
                                             :show show :pipeline pipeline
                                             :vertex-buffer vertex-buffer
                                             :instance-buffer instance-buffer
                                             :instance-data instance-data)))
                 (setf completed-p t)
                 (luvcraft:add-luvcraft-overlay session overlay)))
          (unless completed-p
            (when pipeline
              (ignore-errors
                (luvcraft::release-live-shader-pipeline pipeline)))
            (when instance-buffer (ignore-errors (luv:destroy instance-buffer)))
            (when vertex-buffer (ignore-errors (luv:destroy vertex-buffer))))))))

(defun stop-birthday-fireworks (session)
  "End the show: release the fireworks overlay, returning it, or NIL when
none was running."
  (let ((overlay (find-fireworks-overlay session)))
    (when overlay
      (luvcraft:remove-luvcraft-overlay session overlay))
    overlay))

;;; ---------------------------------------------------------------------
;;; The spark's glow, on the GPU
;;;
;;; A spark is not sphere-traced: it is a small camera-facing square whose
;;; fragment measures its distance from the billboard's centre and falls
;;; off smoothly to nothing at the edge.  The colour lanes carry linear
;;; radiance already scaled by the spark's intensity, so a young spark's
;;; output clears the bloom threshold and the lens blazes it; the alpha
;;; stays low so overlapping glows add rather than occlude.

(in-package #:luvcraft.shaders)

(define-shader-method shader-specification-for
    birthday-spark-vertex-specification
    ((role (eql :birthday-spark)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (spark-center-size :vec4 :location 1)
              (spark-color-glow :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (glow-coordinate :vec2 :location 0)
               (glow-color :vec4 :location 1))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle spark-center-size :xyz))
         (radius (swizzle spark-center-size :w))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (world-position
           (+ center (+ (* right (* corner-x radius))
                        (* up (* corner-y radius)))))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (z-scale (representation (swizzle projection-vector :z)))
         (z-offset (representation (swizzle projection-vector :w))))
    (set-output clip-position
                (vec4 (* view-x x-scale)
                      (- (* view-y y-scale))
                      (+ (* view-z z-scale) z-offset)
                      view-z))
    (set-output glow-coordinate (vec2 corner-x corner-y))
    (set-output glow-color spark-color-glow)))

(define-shader-method shader-specification-for
    birthday-spark-fragment-specification
    ((role (eql :birthday-spark)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((glow-coordinate :vec2 :location 0)
              (glow-color :vec4 :location 1))
     :outputs ((color-output :vec4 :location 0)))
  ;; Squared distance from the billboard centre, smoothed to zero at the
  ;; edge and squared again for a hot little core.  Premultiplied output:
  ;; the colour lanes are the light, the low alpha keeps the glows nearly
  ;; additive over one another and over the dusk behind them.
  (let* ((separation (dot glow-coordinate glow-coordinate))
         (envelope (- 1.0 (smoothstep 0.0 1.0 separation)))
         (falloff (* envelope envelope))
         (blaze (* (swizzle glow-color :w) falloff)))
    (set-output color-output
                (vec4 (* (swizzle glow-color :xyz) blaze)
                      (* falloff 0.4)))))
