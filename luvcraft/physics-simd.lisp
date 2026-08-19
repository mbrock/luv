;;; The four-wide contact kernels: what the colouring bought (#G3W7KD).
;;;
;;; PHYSICS.LISP solves one contact at a time.  Within a colour no two
;;; contacts share an awake body, so four contacts of a colour can be
;;; gathered into f32.4 lanes, solved together with the same arithmetic in
;;; the same order, and scattered back without any lane's write landing on
;;; another lane's read.  That is exactly Box3D's arrangement (#T3C8FV):
;;; the constraint columns are already the struct-of-arrays the lanes want,
;;; and only the body state needs gathering.
;;;
;;; The kernels here are generated once per instruction family from one
;;; template, so the NEON family on arm64 and the SSE family on x86-64 are
;;; the same code lowered through sb-simd's differently named packages; a
;;; scalar family always exists as the reference (#E8M3JC).  A wide kernel
;;; runs the lane groups it can and hands its tail of fewer than four
;;; contacts back to the scalar kernel.  Branches become blends: where the
;;; scalar kernel skips work, the wide kernel does the work into a lane it
;;; then ignores, which is why the arithmetic never divides by a value it
;;; has not first pushed away from zero.
;;;
;;; The claim these kernels make is testable and tested: a world stepped
;;; with the wide family and the same world stepped with the scalar family
;;; reach bit-identical state (PHYSICS-TESTS.LISP).  Neither path uses a
;;; fused multiply-add or an approximate reciprocal, for that reason. #7PAQ3M

(in-package #:luvcraft)

(defmacro define-wide-physics-kernels (family package &key blend)
  "Define the PHYSICS-* kernel methods for FAMILY using PACKAGE's f32.4
operations.  BLEND is :bit-select where the package has F32.4-BIT-SELECT
and :and-or where SSE2's U32.4 comparison masks select the float lane bits."
  (flet ((sym (name) (intern name package)))
    (let ((f32.4 (sym "F32.4"))
          (make (sym "MAKE-F32.4"))
          (values4 (sym "F32.4-VALUES"))
          (aref4 (sym "F32.4-AREF"))
          (add (sym "F32.4+")) (sub (sym "F32.4-"))
          (mul (sym "F32.4*")) (div (sym "F32.4/"))
          (wmax (sym "F32.4-MAX"))
          (gt (sym "F32.4>")) (lt (sym "F32.4<"))
          (blend-form
            (ecase blend
              (:bit-select
               (let ((select (sym "F32.4-BIT-SELECT")))
                 (lambda (mask then else) `(,select ,mask ,then ,else))))
              (:and-or
               (let ((wand (sym "U32.4-AND")) (wandc1 (sym "U32.4-ANDC1"))
                     (wor (sym "U32.4-OR")) (cast (sym "U32.4!"))
                     (make (sym "MAKE-F32.4")))
                 (lambda (mask then else)
                   (let ((m (gensym "MASK")) (bits (gensym "BITS"))
                         (m0 (gensym "M0")) (m1 (gensym "M1"))
                         (m2 (gensym "M2")) (m3 (gensym "M3")))
                     ;; SSE2 comparisons produce U32.4 masks, while SBCL's
                     ;; SSE F32.4! cast cannot reinterpret a P128 value.  Do
                     ;; the bit selection in the integer view, then retag its
                     ;; four lanes as floats.  In a compiled kernel the casts
                     ;; into U32.4 are register moves; only this final retag is
                     ;; expanded lane by lane.
                     `(let* ((,m ,mask)
                             (,bits
                              (,wor
                               (,wand ,m (,cast ,then))
                               (,wandc1 ,m (,cast ,else)))))
                        (multiple-value-bind (,m0 ,m1 ,m2 ,m3)
                            (sb-ext:%simd-pack-singles ,bits)
                          (,make ,m0 ,m1 ,m2 ,m3))))))))))
      `(macrolet ((%wide-physics-warm-start (&rest args)
                    (list* ',(intern (format nil "%~A-PHYSICS-WARM-START" family)) args))
                  (%wide-physics-solve-contacts (&rest args)
                    (list* ',(intern (format nil "%~A-PHYSICS-SOLVE-CONTACTS" family)) args))
                  (%wide-physics-apply-restitution (&rest args)
                    (list* ',(intern (format nil "%~A-PHYSICS-APPLY-RESTITUTION" family)) args))
                  (w+ (a b) (list ',add a b))
                  (w- (a b) (list ',sub a b))
                  (w* (a b) (list ',mul a b))
                  (w/ (a b) (list ',div a b))
                  (wmax (a b) (list ',wmax a b))
                  ;; sb-simd 2.6.7's NEON F32.4-SQRT is mis-encoded and
                  ;; raises SIGILL, so the two square roots a lane group
                  ;; needs go through the scalar unit lane by lane.  The
                  ;; result is what the vector instruction would give.
                  (wsqrt (a)
                    (let ((v (gensym)) (l0 (gensym)) (l1 (gensym))
                          (l2 (gensym)) (l3 (gensym)))
                      `(let ((,v ,a))
                         (multiple-value-bind (,l0 ,l1 ,l2 ,l3) (,',values4 ,v)
                           (,',make (sqrt (the (single-float 0f0) ,l0))
                                    (sqrt (the (single-float 0f0) ,l1))
                                    (sqrt (the (single-float 0f0) ,l2))
                                    (sqrt (the (single-float 0f0) ,l3)))))))
                  (w> (a b) (list ',gt a b))
                  (w< (a b) (list ',lt a b))
                  (wblend (mask then else) (funcall ,blend-form mask then else))
                  (wsplat (x) (list ',f32.4 x))
                  (wload (array index) (list ',aref4 array index))
                  (wstore (array index value)
                    (list 'setf (list ',aref4 array index) value))
                  (wgather (array i0 i1 i2 i3)
                    (list ',make (list 'aref array i0) (list 'aref array i1)
                          (list 'aref array i2) (list 'aref array i3)))
                  (wscatter (array i0 i1 i2 i3 value)
                    (let ((a (gensym)) (b (gensym)) (c (gensym)) (d (gensym)))
                      `(multiple-value-bind (,a ,b ,c ,d) (,',values4 ,value)
                         (setf (aref ,array ,i0) ,a (aref ,array ,i1) ,b
                               (aref ,array ,i2) ,c (aref ,array ,i3) ,d))))
                  ;; The relative velocity at the contact point of each side.
                  (with-relative-velocities
                      ((vrax vray vraz vrbx vrby vrbz) &body body)
                    `(let* ((,vrax (w+ vax (w- (w* way raz) (w* waz ray))))
                            (,vray (w+ vay (w- (w* waz rax) (w* wax raz))))
                            (,vraz (w+ vaz (w- (w* wax ray) (w* way rax))))
                            (,vrbx (w+ (w+ vbx kvx) (w- (w* wby rbz) (w* wbz rby))))
                            (,vrby (w+ (w+ vby kvy) (w- (w* wbz rbx) (w* wbx rbz))))
                            (,vrbz (w+ (w+ vbz kvz) (w- (w* wbx rby) (w* wby rbx)))))
                       ,@body))
                  ;; Apply the impulse P to both sides' linear and angular state.
                  (apply-impulse (px py pz)
                    `(setf vax (w- vax (w* ima ,px)) vay (w- vay (w* ima ,py))
                           vaz (w- vaz (w* ima ,pz))
                           vbx (w+ vbx (w* imb ,px)) vby (w+ vby (w* imb ,py))
                           vbz (w+ vbz (w* imb ,pz))
                           wax (w- wax (w* iia (w- (w* ray ,pz) (w* raz ,py))))
                           way (w- way (w* iia (w- (w* raz ,px) (w* rax ,pz))))
                           waz (w- waz (w* iia (w- (w* rax ,py) (w* ray ,px))))
                           wbx (w+ wbx (w* iib (w- (w* rby ,pz) (w* rbz ,py))))
                           wby (w+ wby (w* iib (w- (w* rbz ,px) (w* rbx ,pz))))
                           wbz (w+ wbz (w* iib (w- (w* rbx ,py) (w* rby ,px)))))))

         (defmethod physics-integrate-velocities ((kernels (eql ,family)) awake h)
           (declare (single-float h))
           (records:with-columnar-buffer-storage
               ((count row (vxs vx) (vys vy) (vzs vz) (wxs wx) (wys wy) (wzs wz)
                 (dampings damping) (inverse-masses inverse-mass))
                awake physics-body-columns)
             (declare (ignore row) (optimize (speed 3) (safety 0)))
             (let* ((gravity (* h (coerce *physics-gravity* 'single-float)))
                    (wide-end (* 4 (floor count 4)))
                    (one (wsplat 1f0))
                    (hs (wsplat h))
                    (gs (wsplat gravity))
                    (zero (wsplat 0f0)))
               (declare (single-float gravity) (fixnum wide-end))
               (loop for i fixnum from 0 below wide-end by 4
                     do (let* ((scale (w/ one (w+ one (w* hs (wload dampings i)))))
                               (dynamic (w> (wload inverse-masses i) zero))
                               (fall (wblend dynamic gs zero)))
                          (wstore vxs i (w* scale (wload vxs i)))
                          (wstore vys i (w+ (w* scale (wload vys i)) fall))
                          (wstore vzs i (w* scale (wload vzs i)))
                          (wstore wxs i (w* scale (wload wxs i)))
                          (wstore wys i (w* scale (wload wys i)))
                          (wstore wzs i (w* scale (wload wzs i)))))
               (loop for i fixnum from wide-end below count
                     do (let ((scale (/ (+ 1f0 (* h (aref dampings i))))))
                          (declare (single-float scale))
                          (setf (aref vxs i) (* scale (aref vxs i))
                                (aref vys i) (+ (* scale (aref vys i))
                                                (if (plusp (aref inverse-masses i)) gravity 0f0))
                                (aref vzs i) (* scale (aref vzs i))
                                (aref wxs i) (* scale (aref wxs i))
                                (aref wys i) (* scale (aref wys i))
                                (aref wzs i) (* scale (aref wzs i))))))
             awake))

         (defmethod physics-integrate-positions ((kernels (eql ,family)) awake h)
           ;; The delta lanes go four at a time; the orientations, whose
           ;; normalization the eye alone consumes, stay scalar.
           (declare (single-float h))
           (records:with-columnar-buffer-storage
               ((count row (vxs vx) (vys vy) (vzs vz)
                 (dxs dx) (dys dy) (dzs dz))
                awake physics-body-columns)
             (declare (ignore row) (optimize (speed 3) (safety 0)))
             (let ((wide-end (* 4 (floor count 4)))
                   (hs (wsplat h)))
               (declare (fixnum wide-end))
               (loop for i fixnum from 0 below wide-end by 4
                     do (wstore dxs i (w+ (wload dxs i) (w* hs (wload vxs i))))
                        (wstore dys i (w+ (wload dys i) (w* hs (wload vys i))))
                        (wstore dzs i (w+ (wload dzs i) (w* hs (wload vzs i)))))
               (loop for i fixnum from wide-end below count
                     do (setf (aref dxs i) (+ (aref dxs i) (* h (aref vxs i)))
                              (aref dys i) (+ (aref dys i) (* h (aref vys i)))
                              (aref dzs i) (+ (aref dzs i) (* h (aref vzs i)))))))
           (%physics-integrate-orientations awake h)
           awake)

         (defmethod physics-warm-start ((kernels (eql ,family)) constraints awake starts)
           (do-physics-colors (start end starts)
             (%wide-physics-warm-start constraints awake start end))
           constraints)

         (defun ,(intern (format nil "%~A-PHYSICS-WARM-START" family)) (constraints awake start end)
           (declare (fixnum start end))
           (let ((wide-end (+ start (* 4 (floor (- end start) 4)))))
             (declare (fixnum wide-end))
             (with-physics-kernel-columns (constraints awake)
               (declare (optimize (speed 3) (safety 0)))
               (loop for c fixnum from start below wide-end by 4
                     do (let* ((c1 (+ c 1)) (c2 (+ c 2)) (c3 (+ c 3))
                               (ia0 (aref body-as c)) (ia1 (aref body-as c1))
                               (ia2 (aref body-as c2)) (ia3 (aref body-as c3))
                               (ib0 (aref body-bs c)) (ib1 (aref body-bs c1))
                               (ib2 (aref body-bs c2)) (ib3 (aref body-bs c3))
                               (nx (wload nxs c)) (ny (wload nys c)) (nz (wload nzs c))
                               (lambda-n (wload normal-impulses c))
                               (f1 (wload tangent-impulses-1 c))
                               (f2 (wload tangent-impulses-2 c))
                               (px (w+ (w+ (w* lambda-n nx) (w* f1 (wload t1xs c))) (w* f2 (wload t2xs c))))
                               (py (w+ (w+ (w* lambda-n ny) (w* f1 (wload t1ys c))) (w* f2 (wload t2ys c))))
                               (pz (w+ (w+ (w* lambda-n nz) (w* f1 (wload t1zs c))) (w* f2 (wload t2zs c))))
                               (rax (wload raxs c)) (ray (wload rays c)) (raz (wload razs c))
                               (rbx (wload rbxs c)) (rby (wload rbys c)) (rbz (wload rbzs c))
                               (ima (wgather inverse-masses ia0 ia1 ia2 ia3))
                               (imb (wgather inverse-masses ib0 ib1 ib2 ib3))
                               (iia (wgather inverse-inertias ia0 ia1 ia2 ia3))
                               (iib (wgather inverse-inertias ib0 ib1 ib2 ib3))
                               (rx (wload rolling-impulses-x c))
                               (ry (wload rolling-impulses-y c))
                               (rz (wload rolling-impulses-z c)))
                          (declare (fixnum c1 c2 c3 ia0 ia1 ia2 ia3 ib0 ib1 ib2 ib3))
                          (wscatter vxs ia0 ia1 ia2 ia3
                                    (w- (wgather vxs ia0 ia1 ia2 ia3) (w* ima px)))
                          (wscatter vys ia0 ia1 ia2 ia3
                                    (w- (wgather vys ia0 ia1 ia2 ia3) (w* ima py)))
                          (wscatter vzs ia0 ia1 ia2 ia3
                                    (w- (wgather vzs ia0 ia1 ia2 ia3) (w* ima pz)))
                          (wscatter vxs ib0 ib1 ib2 ib3
                                    (w+ (wgather vxs ib0 ib1 ib2 ib3) (w* imb px)))
                          (wscatter vys ib0 ib1 ib2 ib3
                                    (w+ (wgather vys ib0 ib1 ib2 ib3) (w* imb py)))
                          (wscatter vzs ib0 ib1 ib2 ib3
                                    (w+ (wgather vzs ib0 ib1 ib2 ib3) (w* imb pz)))
                          (wscatter wxs ia0 ia1 ia2 ia3
                                    (w- (wgather wxs ia0 ia1 ia2 ia3)
                                        (w* iia (w+ (w- (w* ray pz) (w* raz py)) rx))))
                          (wscatter wys ia0 ia1 ia2 ia3
                                    (w- (wgather wys ia0 ia1 ia2 ia3)
                                        (w* iia (w+ (w- (w* raz px) (w* rax pz)) ry))))
                          (wscatter wzs ia0 ia1 ia2 ia3
                                    (w- (wgather wzs ia0 ia1 ia2 ia3)
                                        (w* iia (w+ (w- (w* rax py) (w* ray px)) rz))))
                          (wscatter wxs ib0 ib1 ib2 ib3
                                    (w+ (wgather wxs ib0 ib1 ib2 ib3)
                                        (w* iib (w+ (w- (w* rby pz) (w* rbz py)) rx))))
                          (wscatter wys ib0 ib1 ib2 ib3
                                    (w+ (wgather wys ib0 ib1 ib2 ib3)
                                        (w* iib (w+ (w- (w* rbz px) (w* rbx pz)) ry))))
                          (wscatter wzs ib0 ib1 ib2 ib3
                                    (w+ (wgather wzs ib0 ib1 ib2 ib3)
                                        (w* iib (w+ (w- (w* rbx py) (w* rby px)) rz)))))))
             (when (< wide-end end)
               (%physics-warm-start-scalar constraints awake wide-end end))
             constraints))

         (defmethod physics-solve-contacts
             ((kernels (eql ,family)) constraints awake starts inv-h use-bias-p elapsed push-max)
           (declare (single-float inv-h elapsed push-max))
           (do-physics-colors (start end starts)
             (%wide-physics-solve-contacts constraints awake start end
                                           inv-h use-bias-p elapsed push-max))
           constraints)

         (defun ,(intern (format nil "%~A-PHYSICS-SOLVE-CONTACTS" family))
             (constraints awake start end inv-h use-bias-p elapsed push-max)
           (declare (fixnum start end) (single-float inv-h elapsed push-max))
           (let ((wide-end (+ start (* 4 (floor (- end start) 4)))))
             (declare (fixnum wide-end))
             (with-physics-kernel-columns (constraints awake)
               (declare (optimize (speed 3) (safety 0)))
               (let ((zero (wsplat 0f0)) (one (wsplat 1f0))
                     (inv-hs (wsplat inv-h)) (elapseds (wsplat elapsed))
                     (push-maxs (wsplat (- push-max)))
                     (epsilon (wsplat 1e-12)))
                 (macrolet
                     ((solve-lane-groups (biasing-p)
                        `(loop for c fixnum from start below wide-end by 4
                               do (let* ((c1 (+ c 1)) (c2 (+ c 2)) (c3 (+ c 3))
                                         (ia0 (aref body-as c)) (ia1 (aref body-as c1))
                                         (ia2 (aref body-as c2)) (ia3 (aref body-as c3))
                                         (ib0 (aref body-bs c)) (ib1 (aref body-bs c1))
                                         (ib2 (aref body-bs c2)) (ib3 (aref body-bs c3))
                                         (nx (wload nxs c)) (ny (wload nys c)) (nz (wload nzs c))
                                         (rax (wload raxs c)) (ray (wload rays c)) (raz (wload razs c))
                                         (rbx (wload rbxs c)) (rby (wload rbys c)) (rbz (wload rbzs c))
                                         (kvx (wload kvxs c)) (kvy (wload kvys c)) (kvz (wload kvzs c))
                                         (ima (wgather inverse-masses ia0 ia1 ia2 ia3))
                                         (imb (wgather inverse-masses ib0 ib1 ib2 ib3))
                                         (iia (wgather inverse-inertias ia0 ia1 ia2 ia3))
                                         (iib (wgather inverse-inertias ib0 ib1 ib2 ib3))
                                         (vax (wgather vxs ia0 ia1 ia2 ia3))
                                         (vay (wgather vys ia0 ia1 ia2 ia3))
                                         (vaz (wgather vzs ia0 ia1 ia2 ia3))
                                         (wax (wgather wxs ia0 ia1 ia2 ia3))
                                         (way (wgather wys ia0 ia1 ia2 ia3))
                                         (waz (wgather wzs ia0 ia1 ia2 ia3))
                                         (vbx (wgather vxs ib0 ib1 ib2 ib3))
                                         (vby (wgather vys ib0 ib1 ib2 ib3))
                                         (vbz (wgather vzs ib0 ib1 ib2 ib3))
                                         (wbx (wgather wxs ib0 ib1 ib2 ib3))
                                         (wby (wgather wys ib0 ib1 ib2 ib3))
                                         (wbz (wgather wzs ib0 ib1 ib2 ib3))
                                         (dpx (w+ (w- (wgather dxs ib0 ib1 ib2 ib3)
                                                      (wgather dxs ia0 ia1 ia2 ia3))
                                                  (w* kvx elapseds)))
                                         (dpy (w+ (w- (wgather dys ib0 ib1 ib2 ib3)
                                                      (wgather dys ia0 ia1 ia2 ia3))
                                                  (w* kvy elapseds)))
                                         (dpz (w+ (w- (wgather dzs ib0 ib1 ib2 ib3)
                                                      (wgather dzs ia0 ia1 ia2 ia3))
                                                  (w* kvz elapseds)))
                                         (s (w+ (w+ (w+ (wload separations c) (w* dpx nx))
                                                    (w* dpy ny))
                                                (w* dpz nz)))
                                         (speculative (w> s zero))
                                         (bias (wblend speculative (w* s inv-hs)
                                                       ,(if biasing-p
                                                            '(wmax (w* (wload bias-rates c) s) push-maxs)
                                                            'zero)))
                                         (mass-scale (wblend speculative one
                                                             ,(if biasing-p '(wload mass-scales c) 'one)))
                                         (impulse-scale (wblend speculative zero
                                                                ,(if biasing-p '(wload impulse-scales c) 'zero))))
                                    (declare (fixnum c1 c2 c3 ia0 ia1 ia2 ia3 ib0 ib1 ib2 ib3))
                                    (with-relative-velocities (vrax vray vraz vrbx vrby vrbz)
                                      (let* ((vn (w+ (w+ (w* (w- vrbx vrax) nx) (w* (w- vrby vray) ny))
                                                     (w* (w- vrbz vraz) nz)))
                                             (old (wload normal-impulses c))
                                             (delta (w- (w- zero (w* (wload normal-masses c)
                                                                     (w+ (w* mass-scale vn) bias)))
                                                        (w* impulse-scale old)))
                                             (new (wmax (w+ old delta) zero))
                                             (applied (w- new old))
                                             (px (w* applied nx)) (py (w* applied ny)) (pz (w* applied nz)))
                                        (wstore normal-impulses c new)
                                        (wstore total-normal-impulses c
                                                (w+ (wload total-normal-impulses c) new))
                                        (apply-impulse px py pz)
                                        ,@(unless biasing-p
                                            '(;; Rolling resistance.
                                              (let* ((resistance (wload rolling-resistances c))
                                                     (rolling-mass (wload rolling-masses c))
                                                     (ox (wload rolling-impulses-x c))
                                                     (oy (wload rolling-impulses-y c))
                                                     (oz (wload rolling-impulses-z c))
                                                     (nx2 (w- ox (w* rolling-mass (w- wbx wax))))
                                                     (ny2 (w- oy (w* rolling-mass (w- wby way))))
                                                     (nz2 (w- oz (w* rolling-mass (w- wbz waz))))
                                                     (limit (w* resistance new))
                                                     (magnitude-squared
                                                       (w+ (w+ (w* nx2 nx2) (w* ny2 ny2)) (w* nz2 nz2)))
                                                     (over (w> magnitude-squared
                                                               (w+ (w* limit limit) epsilon)))
                                                     ;; Where nothing is over, the divisor is
                                                     ;; pushed to one and the quotient discarded.
                                                     (scale (wblend over
                                                                    (w/ limit (wsqrt (wblend over magnitude-squared one)))
                                                                    one))
                                                     ;; A resistance of zero keeps its impulse at zero.
                                                     (active (w> resistance zero))
                                                     (nx3 (wblend active (w* nx2 scale) ox))
                                                     (ny3 (wblend active (w* ny2 scale) oy))
                                                     (nz3 (wblend active (w* nz2 scale) oz)))
                                                (wstore rolling-impulses-x c nx3)
                                                (wstore rolling-impulses-y c ny3)
                                                (wstore rolling-impulses-z c nz3)
                                                (let ((ax (w- nx3 ox)) (ay (w- ny3 oy)) (az (w- nz3 oz)))
                                                  (setf wax (w- wax (w* iia ax)) way (w- way (w* iia ay))
                                                        waz (w- waz (w* iia az))
                                                        wbx (w+ wbx (w* iib ax)) wby (w+ wby (w* iib ay))
                                                        wbz (w+ wbz (w* iib az)))))
                                              ;; Friction.
                                              (let* ((t1x (wload t1xs c)) (t1y (wload t1ys c)) (t1z (wload t1zs c))
                                                     (t2x (wload t2xs c)) (t2y (wload t2ys c)) (t2z (wload t2zs c)))
                                                (with-relative-velocities (vrax vray vraz vrbx vrby vrbz)
                                                  (let* ((vrx (w- vrbx vrax)) (vry (w- vrby vray)) (vrz (w- vrbz vraz))
                                                         (vt1 (w+ (w+ (w* vrx t1x) (w* vry t1y)) (w* vrz t1z)))
                                                         (vt2 (w+ (w+ (w* vrx t2x) (w* vry t2y)) (w* vrz t2z)))
                                                         (tangent-mass (wload tangent-masses c))
                                                         (o1 (wload tangent-impulses-1 c))
                                                         (o2 (wload tangent-impulses-2 c))
                                                         (n1 (w- o1 (w* tangent-mass vt1)))
                                                         (n2 (w- o2 (w* tangent-mass vt2)))
                                                         (limit (w* (wload frictions c) new))
                                                         (length-squared (w+ (w* n1 n1) (w* n2 n2)))
                                                         (over (w> length-squared (w* limit limit)))
                                                         (scale (wblend over
                                                                        (w/ limit (wsqrt (wblend over length-squared one)))
                                                                        one))
                                                         (n1 (w* n1 scale)) (n2 (w* n2 scale)))
                                                    (wstore tangent-impulses-1 c n1)
                                                    (wstore tangent-impulses-2 c n2)
                                                    (let* ((d1 (w- n1 o1)) (d2 (w- n2 o2))
                                                           (px (w+ (w* d1 t1x) (w* d2 t2x)))
                                                           (py (w+ (w* d1 t1y) (w* d2 t2y)))
                                                           (pz (w+ (w* d1 t1z) (w* d2 t2z))))
                                                      (apply-impulse px py pz)))))))
                                        (wscatter vxs ia0 ia1 ia2 ia3 vax)
                                        (wscatter vys ia0 ia1 ia2 ia3 vay)
                                        (wscatter vzs ia0 ia1 ia2 ia3 vaz)
                                        (wscatter wxs ia0 ia1 ia2 ia3 wax)
                                        (wscatter wys ia0 ia1 ia2 ia3 way)
                                        (wscatter wzs ia0 ia1 ia2 ia3 waz)
                                        (wscatter vxs ib0 ib1 ib2 ib3 vbx)
                                        (wscatter vys ib0 ib1 ib2 ib3 vby)
                                        (wscatter vzs ib0 ib1 ib2 ib3 vbz)
                                        (wscatter wxs ib0 ib1 ib2 ib3 wbx)
                                        (wscatter wys ib0 ib1 ib2 ib3 wby)
                                        (wscatter wzs ib0 ib1 ib2 ib3 wbz)))))))
                   (if use-bias-p
                       (solve-lane-groups t)
                       (solve-lane-groups nil)))))
             (when (< wide-end end)
               (%physics-solve-contacts-scalar constraints awake wide-end end
                                               inv-h use-bias-p elapsed push-max))
             constraints))

         (defmethod physics-apply-restitution
             ((kernels (eql ,family)) constraints awake starts threshold)
           (declare (single-float threshold))
           (do-physics-colors (start end starts)
             (%wide-physics-apply-restitution constraints awake start end threshold))
           constraints)

         (defun ,(intern (format nil "%~A-PHYSICS-APPLY-RESTITUTION" family))
             (constraints awake start end threshold)
           (declare (fixnum start end) (single-float threshold))
           (let ((wide-end (+ start (* 4 (floor (- end start) 4)))))
             (declare (fixnum wide-end))
             (with-physics-kernel-columns (constraints awake)
               (declare (optimize (speed 3) (safety 0)))
               (let ((zero (wsplat 0f0))
                     (thresholds (wsplat (- threshold))))
                 (loop for c fixnum from start below wide-end by 4
                       do (let* ((c1 (+ c 1)) (c2 (+ c 2)) (c3 (+ c 3))
                                 (ia0 (aref body-as c)) (ia1 (aref body-as c1))
                                 (ia2 (aref body-as c2)) (ia3 (aref body-as c3))
                                 (ib0 (aref body-bs c)) (ib1 (aref body-bs c1))
                                 (ib2 (aref body-bs c2)) (ib3 (aref body-bs c3))
                                 (relative (wload relative-velocities c))
                                 (restitution (wload restitutions c))
                                 (total (wload total-normal-impulses c))
                                 (nx (wload nxs c)) (ny (wload nys c)) (nz (wload nzs c))
                                 (rax (wload raxs c)) (ray (wload rays c)) (raz (wload razs c))
                                 (rbx (wload rbxs c)) (rby (wload rbys c)) (rbz (wload rbzs c))
                                 (kvx (wload kvxs c)) (kvy (wload kvys c)) (kvz (wload kvzs c))
                                 (ima (wgather inverse-masses ia0 ia1 ia2 ia3))
                                 (imb (wgather inverse-masses ib0 ib1 ib2 ib3))
                                 (iia (wgather inverse-inertias ia0 ia1 ia2 ia3))
                                 (iib (wgather inverse-inertias ib0 ib1 ib2 ib3))
                                 (vax (wgather vxs ia0 ia1 ia2 ia3))
                                 (vay (wgather vys ia0 ia1 ia2 ia3))
                                 (vaz (wgather vzs ia0 ia1 ia2 ia3))
                                 (wax (wgather wxs ia0 ia1 ia2 ia3))
                                 (way (wgather wys ia0 ia1 ia2 ia3))
                                 (waz (wgather wzs ia0 ia1 ia2 ia3))
                                 (vbx (wgather vxs ib0 ib1 ib2 ib3))
                                 (vby (wgather vys ib0 ib1 ib2 ib3))
                                 (vbz (wgather vzs ib0 ib1 ib2 ib3))
                                 (wbx (wgather wxs ib0 ib1 ib2 ib3))
                                 (wby (wgather wys ib0 ib1 ib2 ib3))
                                 (wbz (wgather wzs ib0 ib1 ib2 ib3)))
                            (declare (fixnum c1 c2 c3 ia0 ia1 ia2 ia3 ib0 ib1 ib2 ib3))
                            (with-relative-velocities (vrax vray vraz vrbx vrby vrbz)
                              (let* ((vn (w+ (w+ (w* (w- vrbx vrax) nx) (w* (w- vrby vray) ny))
                                             (w* (w- vrbz vraz) nz)))
                                     (old (wload normal-impulses c))
                                     (delta (w- zero (w* (wload normal-masses c)
                                                         (w+ vn (w* restitution relative)))))
                                     ;; Only a real hit bounces: it pushed, and it
                                     ;; arrived fast enough, and it has bounce in it.
                                     (bouncing (,(sym "U32.4-AND")
                                                (,(sym "U32.4-AND") (w> restitution zero) (w< relative thresholds))
                                                (w> total zero)))
                                     (delta (wblend bouncing delta zero))
                                     (new (wmax (w+ old delta) zero))
                                     (applied (w- new old))
                                     (px (w* applied nx)) (py (w* applied ny)) (pz (w* applied nz)))
                                (wstore normal-impulses c new)
                                (wstore total-normal-impulses c (wblend bouncing (w+ total new) total))
                                (apply-impulse px py pz)
                                (wscatter vxs ia0 ia1 ia2 ia3 vax)
                                (wscatter vys ia0 ia1 ia2 ia3 vay)
                                (wscatter vzs ia0 ia1 ia2 ia3 vaz)
                                (wscatter wxs ia0 ia1 ia2 ia3 wax)
                                (wscatter wys ia0 ia1 ia2 ia3 way)
                                (wscatter wzs ia0 ia1 ia2 ia3 waz)
                                (wscatter vxs ib0 ib1 ib2 ib3 vbx)
                                (wscatter vys ib0 ib1 ib2 ib3 vby)
                                (wscatter vzs ib0 ib1 ib2 ib3 vbz)
                                (wscatter wxs ib0 ib1 ib2 ib3 wbx)
                                (wscatter wys ib0 ib1 ib2 ib3 wby)
                                (wscatter wzs ib0 ib1 ib2 ib3 wbz)))))))
             (when (< wide-end end)
               (%physics-apply-restitution-scalar constraints awake wide-end end threshold))
             constraints))))))

;;; The families this image can run.  NEON is the whole 128-bit menu on
;;; arm64; SSE is the same width on x86-64.  Availability is a runtime
;;; property (#C3F7XM): the methods are defined whenever the package
;;; exists, and the default family is chosen at load time.

#+arm64
(define-wide-physics-kernels :neon #:sb-simd-neon :blend :bit-select)

#+x86-64
(define-wide-physics-kernels :sse2 #:sb-simd-sse2 :blend :and-or)

(defun physics-instruction-set-available-p (name)
  (sb-simd-internals:instruction-set-available-p
   (sb-simd-internals:find-instruction-set name)))

(defun available-physics-kernel-families ()
  "The kernel families this image can run, fastest first."
  (append
   #+arm64 (and (physics-instruction-set-available-p :neon) '(:neon))
   #+x86-64 (and (physics-instruction-set-available-p :sse2) '(:sse2))
   '(:scalar)))

(setf *fastest-physics-kernels* (first (available-physics-kernel-families)))
