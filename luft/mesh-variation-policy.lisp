(in-package #:luft)

(defmacro %do-surface-mesh-triangle-scalars
    ((mesh kind stock ambient mask
      ax ay az bx by bz cx cy cz)
     &body body)
  "Iterate MESH's packed triangles with scalar coordinates and no callback.

This is the dense-loop counterpart to %MAP-SURFACE-MESH-TRIANGLE-RECORDS.
The latter deliberately materializes convenient point and normal lists for
cold transformations and inspection; performance-sensitive compilers should
keep the packed instance/template representation through their inner loop."
  (let ((mesh-value (gensym "MESH"))
        (templates (gensym "TEMPLATES"))
        (ranges (gensym "RANGES"))
        (visit (gensym "VISIT"))
        (words (gensym "WORDS"))
        (kind-value (gensym "KIND"))
        (offset (gensym "OFFSET"))
        (base-x (gensym "BASE-X"))
        (base-y (gensym "BASE-Y"))
        (base-z (gensym "BASE-Z"))
        (meta (gensym "META"))
        (stock-value (gensym "STOCK"))
        (ambient-value (gensym "AMBIENT"))
        (template-id (gensym "TEMPLATE-ID"))
        (start (gensym "START"))
        (count (gensym "COUNT"))
        (vertex (gensym "VERTEX"))
        (attributes (gensym "ATTRIBUTES")))
    (labels ((coordinate (base vertex-offset axis)
               `(the mesh-global-tick
                  (+ (ash (the fixnum ,base) 3)
                     (- (aref ,templates
                              (+ (* (+ ,vertex ,vertex-offset)
                                    +mesh-template-vertex-word-count+)
                                 ,axis))
                        +mesh-template-coordinate-bias+)))))
      `(let* ((,mesh-value ,mesh)
              (,templates (surface-mesh-template-vertex-words ,mesh-value))
              (,ranges (surface-mesh-template-ranges ,mesh-value)))
         (flet ((,visit (,words ,kind-value)
                  (declare (type (simple-array (unsigned-byte 32) (*)) ,words))
                  (loop for ,offset fixnum from 0 below (length ,words)
                          by +mesh-instance-word-count+
                        for ,base-x = (aref ,words ,offset)
                        for ,base-y = (aref ,words (+ ,offset 1))
                        for ,base-z = (aref ,words (+ ,offset 2))
                        for ,meta = (aref ,words (+ ,offset 3))
                        for ,template-id = (ldb (byte 16 0) ,meta)
                        for ,stock-value =
                          (ldb (byte +mesh-instance-stock-bit-count+
                                     +mesh-instance-stock-shift+)
                               ,meta)
                        for ,ambient-value =
                          (ldb (byte 2
                                     +mesh-instance-ambient-occlusion-shift+)
                               ,meta)
                        for ,start = (aref ,ranges (* 2 ,template-id))
                        for ,count = (aref ,ranges (1+ (* 2 ,template-id)))
                        do (loop for ,vertex fixnum from ,start
                                   below (+ ,start ,count) by 3
                                 for ,attributes =
                                   (aref ,templates
                                         (+ (* ,vertex
                                               +mesh-template-vertex-word-count+)
                                            3))
                                 do (let ((,kind ,kind-value)
                                          (,stock ,stock-value)
                                          (,ambient ,ambient-value)
                                          (,mask (ldb (byte 3 10) ,attributes))
                                          (,ax ,(coordinate base-x 0 0))
                                          (,ay ,(coordinate base-y 0 1))
                                          (,az ,(coordinate base-z 0 2))
                                          (,bx ,(coordinate base-x 1 0))
                                          (,by ,(coordinate base-y 1 1))
                                          (,bz ,(coordinate base-z 1 2))
                                          (,cx ,(coordinate base-x 2 0))
                                          (,cy ,(coordinate base-y 2 1))
                                          (,cz ,(coordinate base-z 2 2)))
                                      (declare
                                       (ignorable ,kind ,stock ,ambient ,mask
                                                  ,ax ,ay ,az ,bx ,by ,bz
                                                  ,cx ,cy ,cz)
                                       (type fixnum ,stock ,ambient ,mask)
                                       (type mesh-global-tick
                                             ,ax ,ay ,az ,bx ,by ,bz
                                             ,cx ,cy ,cz))
                                      ,@body)))))
           (,visit (surface-mesh-face-instance-words ,mesh-value) :face)
           (,visit (surface-mesh-band-instance-words ,mesh-value) :band)
           (,visit (surface-mesh-fan-instance-words ,mesh-value) :junction))))))

(declaim (inline %unit-bevel-coordinate-site-and-direction)
         (ftype (function (mesh-global-tick)
                  (values (integer 0 #.(ash 1 17))
                          (integer -1 1) &optional))
                %unit-bevel-coordinate-site-and-direction))
(defun %unit-bevel-coordinate-site-and-direction (coordinate)
  "Decode one nonnegative width-one tick coordinate without materialization."
  (declare (optimize (speed 3) (safety 1))
           (type mesh-global-tick coordinate))
  (let ((cell (ash coordinate -3)))
    (case (logand coordinate 7)
      (0 (values cell 0))
      (1 (values cell 1))
      (7 (values (1+ cell) -1))
      (t (error "Width-one point coordinate ~D has no canonical lattice-site owner."
                coordinate)))))

(declaim (inline %unit-bevel-point-owner)
         (ftype (function
                  (mesh-global-tick mesh-global-tick mesh-global-tick)
                  (values (integer 0 #.(ash 1 17))
                          (integer 0 #.(ash 1 17))
                          (integer 0 #.(ash 1 17))
                          (integer -1 1) (integer -1 1) (integer -1 1)
                          &optional))
                %unit-bevel-point-owner))
(defun %unit-bevel-point-owner (x y z)
  "Return the scalar owner site and local direction for a witness point."
  (multiple-value-bind (site-x direction-x)
      (%unit-bevel-coordinate-site-and-direction x)
    (multiple-value-bind (site-y direction-y)
        (%unit-bevel-coordinate-site-and-direction y)
      (multiple-value-bind (site-z direction-z)
          (%unit-bevel-coordinate-site-and-direction z)
        (values site-x site-y site-z
                direction-x direction-y direction-z)))))

(defconstant +global-mesh-point-z-bit-count+ 12)
(defconstant +global-mesh-point-axis-bit-count+ 21)
(defconstant +global-mesh-point-y-shift+ +global-mesh-point-z-bit-count+)
(defconstant +global-mesh-point-x-shift+
  (+ +global-mesh-point-z-bit-count+ +global-mesh-point-axis-bit-count+))

(declaim (inline %pack-global-mesh-point
                 %global-mesh-point-x %global-mesh-point-y
                 %global-mesh-point-z %global-mesh-point-distance-squared))
(defun %pack-global-mesh-point (x y z)
  "Pack a world-domain tick point into one lexicographically ordered fixnum."
  (declare (optimize (speed 3) (safety 1))
           (type mesh-global-tick x y z))
  (unless (and (typep x '(unsigned-byte #.+global-mesh-point-axis-bit-count+))
               (typep y '(unsigned-byte #.+global-mesh-point-axis-bit-count+))
               (typep z '(unsigned-byte #.+global-mesh-point-z-bit-count+)))
    (error "Global mesh point ~S exceeds the LUFT world-domain tick range."
           (list x y z)))
  (logior (ash x +global-mesh-point-x-shift+)
          (ash y +global-mesh-point-y-shift+)
          z))

(defun %global-mesh-point-x (point)
  (ldb (byte +global-mesh-point-axis-bit-count+
             +global-mesh-point-x-shift+)
       point))
(defun %global-mesh-point-y (point)
  (ldb (byte +global-mesh-point-axis-bit-count+
             +global-mesh-point-y-shift+)
       point))
(defun %global-mesh-point-z (point)
  (ldb (byte +global-mesh-point-z-bit-count+ 0) point))

(defun %global-mesh-point-distance-squared (left right)
  (let ((dx (- (%global-mesh-point-x right) (%global-mesh-point-x left)))
        (dy (- (%global-mesh-point-y right) (%global-mesh-point-y left)))
        (dz (- (%global-mesh-point-z right) (%global-mesh-point-z left))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defun %global-mesh-point-list (point)
  (list (%global-mesh-point-x point)
        (%global-mesh-point-y point)
        (%global-mesh-point-z point)))

(declaim (inline %triangle-cross-scalars)
         (ftype (function
                  (mesh-global-tick mesh-global-tick mesh-global-tick
                   mesh-global-tick mesh-global-tick mesh-global-tick
                   mesh-global-tick mesh-global-tick mesh-global-tick)
                  (values (signed-byte 29) (signed-byte 29)
                          (signed-byte 29) &optional))
                %triangle-cross-scalars))
(defun %triangle-cross-scalars (ax ay az bx by bz cx cy cz)
  (declare (optimize (speed 3) (safety 1))
           (type mesh-global-tick ax ay az bx by bz cx cy cz))
  (let ((ux (the (signed-byte 14) (- bx ax)))
        (uy (the (signed-byte 14) (- by ay)))
        (uz (the (signed-byte 14) (- bz az)))
        (vx (the (signed-byte 14) (- cx ax)))
        (vy (the (signed-byte 14) (- cy ay)))
        (vz (the (signed-byte 14) (- cz az))))
    (values (the (signed-byte 29)
              (- (the fixnum (* uy vz)) (the fixnum (* uz vy))))
            (the (signed-byte 29)
              (- (the fixnum (* uz vx)) (the fixnum (* ux vz))))
            (the (signed-byte 29)
              (- (the fixnum (* ux vy)) (the fixnum (* uy vx)))))))

(defun %emit-global-triangle-scalars
    (builder kind stock ambient mask nx ny nz
     ax ay az bx by bz cx cy cz)
  "Emit one global-tick triangle without point, base, origin, or normal lists."
  (declare (optimize (speed 3) (safety 1))
           (type surface-mesh-builder builder)
           (type fixnum stock ambient mask)
           (type (signed-byte 29) nx ny nz)
           (type mesh-global-tick ax ay az bx by bz cx cy cz))
  (let* ((base-x (ash (min ax bx cx) -3))
         (base-y (ash (min ay by cy) -3))
         (base-z (ash (min az bz cz) -3))
         (origin-x (ash base-x 3))
         (origin-y (ash base-y 3))
         (origin-z (ash base-z 3))
         (scratch (surface-mesh-builder-vertex-scratch builder)))
    (%scratch-triangle
     scratch 0 (ecase kind (:face 0) (:band 1) (:junction 2)) mask
     (- ax origin-x) (- ay origin-y) (- az origin-z)
     (- bx origin-x) (- by origin-y) (- bz origin-z)
     (- cx origin-x) (- cy origin-y) (- cz origin-z)
     nx ny nz)
    (%emit-instance builder kind base-x base-y base-z stock ambient 3)))

(defun %unit-bevel-point-site (point)
  "Return the canonical lattice site and local direction owning POINT.

POINT must come from a width-one LUFT surface, so every coordinate is exactly
on, one tick above, or one tick below its owning lattice plane."
  (let ((site nil)
        (direction nil))
    (dolist (coordinate point)
      (multiple-value-bind (cell remainder)
          (floor coordinate +mesh-cell-size+)
        (case remainder
          (0 (push cell site) (push 0 direction))
          (1 (push cell site) (push 1 direction))
          (7 (push (1+ cell) site) (push -1 direction))
          (t (error "Width-one point coordinate ~D has no canonical lattice-site owner."
                    coordinate)))))
    (values (nreverse site) (nreverse direction))))

(declaim (ftype function %triangulate-coplanar-loop))

(defconstant +dense-bevel-site-field-byte-limit+ (* 16 1024 1024))
(defconstant +dense-bevel-site-field-sparsity-limit+ 16)
(defconstant +bevel-site-page-edge+ 8)
(defconstant +bevel-site-page-volume+
  (* +bevel-site-page-edge+ +bevel-site-page-edge+ +bevel-site-page-edge+))
;; Budget every possible page plus a conservative two-word directory entry.
;; The actual directory is one pointer per page, so this keeps the fast path
;; bounded without depending on implementation-specific object sizes.
(defconstant +bevel-site-page-directory-limit+
  (floor +dense-bevel-site-field-byte-limit+
         (+ +bevel-site-page-volume+ 16)))

(declaim (inline %dense-bevel-site-index))
(defun %dense-bevel-site-index
    (x y z x0 y0 z0 y-span z-span)
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x y z x0 y0 z0 y-span z-span))
  (the fixnum
    (+ (the fixnum (- z z0))
       (the fixnum
         (* z-span
            (the fixnum
              (+ (the fixnum (- y y0))
                 (the fixnum (* y-span (the fixnum (- x x0)))))))))))

(defun %paged-byte-stock-mask-policy-p (domain stock-masks site-widths)
  "Whether STOCK-MASKS can use the bounded direct page directory for DOMAIN."
  (and (typep stock-masks '(simple-array (unsigned-byte 8) (*)))
       (typep site-widths '(simple-array (unsigned-byte 8) (*)))
       ;; Zero is the unobserved-site sentinel inside a page.  Wider or zero
       ;; masks retain the fully general EQL hash compiler below.
       (loop for stock-mask across stock-masks always (plusp stock-mask))
       (let* ((x-pages (ceiling (1+ (world-domain-x-limit domain))
                               +bevel-site-page-edge+))
              (y-pages (ceiling (1+ (world-domain-y-limit domain))
                               +bevel-site-page-edge+))
              (z-pages (ceiling (1+ +top-z+) +bevel-site-page-edge+)))
         (<= (* x-pages y-pages z-pages)
             +bevel-site-page-directory-limit+))))

(defun %compile-paged-byte-stock-mask-bevel-sites
    (owner-witnesses stock-masks site-widths width-census)
  "Fold a positive byte stock lane through sparse 8-cubed pages.

Return the sparse or dense realized width field, its exact site count and
maximum width, and its inclusive coordinate bounds.  Pages are only the
one-pass accumulation language; the returned field has the same tight layout
used by the generic compiler and all realization passes."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 8) (*)) stock-masks)
           (type (simple-array (unsigned-byte 8) (*)) site-widths)
           (type (simple-array (unsigned-byte 32) (5)) width-census))
  (let* ((domain (surface-mesh-domain (cdar owner-witnesses)))
         (x-pages (ceiling (1+ (world-domain-x-limit domain))
                           +bevel-site-page-edge+))
         (y-pages (ceiling (1+ (world-domain-y-limit domain))
                           +bevel-site-page-edge+))
         (z-pages (ceiling (1+ +top-z+) +bevel-site-page-edge+))
         (directory-count (* x-pages y-pages z-pages))
         (pages (make-array directory-count :initial-element nil))
         (touched
           (make-array (min 1024 directory-count)
                       :element-type '(unsigned-byte 32)
                       :adjustable t :fill-pointer 0))
         (site-count 0)
         (minimum-site-x most-positive-fixnum)
         (maximum-site-x most-negative-fixnum)
         (minimum-site-y most-positive-fixnum)
         (maximum-site-y most-negative-fixnum)
         (minimum-site-z most-positive-fixnum)
         (maximum-site-z most-negative-fixnum))
    (declare (type fixnum x-pages y-pages z-pages directory-count site-count
                          minimum-site-x maximum-site-x
                          minimum-site-y maximum-site-y
                          minimum-site-z maximum-site-z))
    (labels ((directory-index (x y z)
               (the fixnum
                 (+ (ash z -3)
                    (the fixnum
                      (* z-pages
                         (the fixnum
                           (+ (ash y -3)
                              (the fixnum (* y-pages (ash x -3))))))))))
             (local-index (x y z)
               (the (unsigned-byte 9)
                 (logior (logand z 7)
                         (ash (logand y 7) 3)
                         (ash (logand x 7) 6))))
             (observe (x y z stock-mask)
               (declare (type (integer 0 #.(ash 1 17)) x y)
                        (type (integer 0 255) z)
                        (type (unsigned-byte 8) stock-mask))
               (let* ((page-index (directory-index x y z))
                      (page (aref pages page-index)))
                 (unless page
                   (setf page
                         (make-array +bevel-site-page-volume+
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                         (aref pages page-index) page)
                   (vector-push-extend page-index touched))
                 (let* ((page
                          (the (simple-array (unsigned-byte 8)
                                             (#.+bevel-site-page-volume+))
                            page))
                        (index (local-index x y z))
                        (old (aref page index)))
                   (when (zerop old)
                     (incf site-count)
                     (setf minimum-site-x (min minimum-site-x x)
                           maximum-site-x (max maximum-site-x x)
                           minimum-site-y (min minimum-site-y y)
                           maximum-site-y (max maximum-site-y y)
                           minimum-site-z (min minimum-site-z z)
                           maximum-site-z (max maximum-site-z z)))
                   (setf (aref page index) (logior old stock-mask))))))
      (declare
       (inline directory-index local-index observe)
       (ftype (function (fixnum fixnum fixnum) fixnum)
              directory-index local-index)
       (ftype (function (fixnum fixnum fixnum (unsigned-byte 8)) *) observe))
      (dolist (owner-witness owner-witnesses)
        (let ((witness (cdr owner-witness)))
          (%do-surface-mesh-triangle-scalars
              (witness kind stock ambient mask
                       ax ay az bx by bz cx cy cz)
            (declare (ignore kind ambient mask))
            (unless (< stock (length stock-masks))
              (error "Mesh stock ~D is outside the compiled bevel policy of ~D entries."
                     stock (length stock-masks)))
            (let ((stock-mask (aref stock-masks stock)))
              (multiple-value-bind (asx asy asz)
                  (%unit-bevel-point-owner ax ay az)
                (multiple-value-bind (bsx bsy bsz)
                    (%unit-bevel-point-owner bx by bz)
                  (multiple-value-bind (csx csy csz)
                      (%unit-bevel-point-owner cx cy cz)
                    (observe asx asy asz stock-mask)
                    (unless (and (= asx bsx) (= asy bsy) (= asz bsz))
                      (observe bsx bsy bsz stock-mask))
                    (unless (or (and (= asx csx) (= asy csy) (= asz csz))
                                (and (= bsx csx) (= bsy csy) (= bsz csz)))
                      (observe csx csy csz stock-mask))))))))))
    (when (zerop site-count)
      (setf minimum-site-x 0 maximum-site-x 0
            minimum-site-y 0 maximum-site-y 0
            minimum-site-z 0 maximum-site-z 0))
    (let* ((site-x-span (1+ (- maximum-site-x minimum-site-x)))
           (site-y-span (1+ (- maximum-site-y minimum-site-y)))
           (site-z-span (1+ (- maximum-site-z minimum-site-z)))
           (site-volume (* site-x-span site-y-span site-z-span))
           (dense-widths
             (when (and (plusp site-count)
                        (<= site-volume +dense-bevel-site-field-byte-limit+)
                        (<= site-volume
                            (* +dense-bevel-site-field-sparsity-limit+
                               site-count)))
               (make-array site-volume :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
           (width-by-site
             (unless dense-widths
               (make-hash-table :test #'eql :size (max 16 site-count))))
           (maximum-width 1))
      (declare (type fixnum site-x-span site-y-span site-z-span site-volume
                            maximum-width))
      (loop for page-index across touched do
        (multiple-value-bind (page-x remainder)
            (truncate page-index (* y-pages z-pages))
          (multiple-value-bind (page-y page-z)
              (truncate remainder z-pages)
            (let ((page
                    (the (simple-array (unsigned-byte 8)
                                       (#.+bevel-site-page-volume+))
                      (aref pages page-index))))
              (dotimes (index +bevel-site-page-volume+)
                (let ((site-mask (aref page index)))
                  (unless (zerop site-mask)
                    (let ((x (+ (ash page-x 3) (ash index -6)))
                          (y (+ (ash page-y 3) (ldb (byte 3 3) index)))
                          (z (+ (ash page-z 3) (ldb (byte 3 0) index))))
                      (declare (type fixnum x y z))
                      (unless (< site-mask (length site-widths))
                        (error "Incident mesh stocks compiled to invalid bevel mask ~D at ~S."
                               site-mask (list x y z)))
                      (let ((width (aref site-widths site-mask)))
                        (unless (and (integerp width) (<= 1 width 4))
                          (error "Site-local bevel policy assigned invalid width ~S at ~S."
                                 width (list x y z)))
                        (setf maximum-width (max maximum-width width))
                        (incf (aref width-census width))
                        (if dense-widths
                            (setf (aref dense-widths
                                        (%dense-bevel-site-index
                                         x y z
                                         minimum-site-x minimum-site-y
                                         minimum-site-z
                                         site-y-span site-z-span))
                                  width)
                            (setf (gethash (%lattice-key x y z) width-by-site)
                                  width)))))))))))
      (values width-by-site dense-widths site-count maximum-width
              minimum-site-x maximum-site-x
              minimum-site-y maximum-site-y
              minimum-site-z maximum-site-z))))

;;; The policy compiler above chooses one of two deliberately equivalent
;;; representations.  Downstream transition planning and emission share this
;;; small immutable view instead of growing another all-in-one compiler
;;; function merely to keep the field's representation details lexical.
(defstruct (%bevel-site-width-field
             (:constructor %make-bevel-site-width-field
                 (width-by-site dense-widths
                  minimum-site-x minimum-site-y minimum-site-z
                  site-y-span site-z-span)))
  (width-by-site nil :type (or null hash-table) :read-only t)
  (dense-widths nil
                :type (or null
                          (simple-array (unsigned-byte 8) (*)))
                :read-only t)
  (minimum-site-x 0 :type fixnum :read-only t)
  (minimum-site-y 0 :type fixnum :read-only t)
  (minimum-site-z 0 :type fixnum :read-only t)
  (site-y-span 1 :type fixnum :read-only t)
  (site-z-span 1 :type fixnum :read-only t))

(defmacro %with-bevel-site-width-field ((site-width) field &body body)
  "Execute BODY with an inline SITE-WIDTH lookup over FIELD.

The representation is unpacked once around an entire planning or emission
stage, keeping the scalar triangle loop independent of the policy compiler
without paying structure-access or generic-call costs per vertex."
  (let ((field-var (gensym "FIELD"))
        (width-by-site (gensym "WIDTH-BY-SITE"))
        (dense-widths (gensym "DENSE-WIDTHS"))
        (minimum-site-x (gensym "MINIMUM-SITE-X"))
        (minimum-site-y (gensym "MINIMUM-SITE-Y"))
        (minimum-site-z (gensym "MINIMUM-SITE-Z"))
        (site-y-span (gensym "SITE-Y-SPAN"))
        (site-z-span (gensym "SITE-Z-SPAN")))
    `(let* ((,field-var ,field)
            (,width-by-site
              (%bevel-site-width-field-width-by-site ,field-var))
            (,dense-widths
              (%bevel-site-width-field-dense-widths ,field-var))
            (,minimum-site-x
              (%bevel-site-width-field-minimum-site-x ,field-var))
            (,minimum-site-y
              (%bevel-site-width-field-minimum-site-y ,field-var))
            (,minimum-site-z
              (%bevel-site-width-field-minimum-site-z ,field-var))
            (,site-y-span
              (%bevel-site-width-field-site-y-span ,field-var))
            (,site-z-span
              (%bevel-site-width-field-site-z-span ,field-var)))
       (declare (type (or null hash-table) ,width-by-site)
                (type (or null (simple-array (unsigned-byte 8) (*)))
                      ,dense-widths)
                (type fixnum ,minimum-site-x ,minimum-site-y ,minimum-site-z
                      ,site-y-span ,site-z-span))
       (labels ((,site-width (site-x site-y site-z)
                  (let ((width
                          (if ,dense-widths
                              (aref ,dense-widths
                                    (%dense-bevel-site-index
                                     site-x site-y site-z
                                     ,minimum-site-x ,minimum-site-y
                                     ,minimum-site-z
                                     ,site-y-span ,site-z-span))
                              (gethash (%lattice-key site-x site-y site-z)
                                       ,width-by-site))))
                    (unless (and (integerp width) (<= 1 width 4))
                      (error "No site-local bevel width was compiled for ~S."
                             (list site-x site-y site-z)))
                    (the (integer 1 4) width))))
         (declare (inline ,site-width)
                  (ftype (function (fixnum fixnum fixnum) (integer 1 4))
                         ,site-width))
         ,@body))))

(defmacro %with-transformed-bevel-triangle
    ((tax tay taz tbx tby tbz tcx tcy tcz)
     site-width ax ay az bx by bz cx cy cz
     &body body)
  "Bind the exact site-width transform of one width-one witness triangle."
  `(multiple-value-bind (asx asy asz adx ady adz)
       (%unit-bevel-point-owner ,ax ,ay ,az)
     (multiple-value-bind (bsx bsy bsz bdx bdy bdz)
         (%unit-bevel-point-owner ,bx ,by ,bz)
       (multiple-value-bind (csx csy csz cdx cdy cdz)
           (%unit-bevel-point-owner ,cx ,cy ,cz)
         (let* ((aw (,site-width asx asy asz))
                (bw (if (and (= asx bsx) (= asy bsy) (= asz bsz))
                        aw
                        (,site-width bsx bsy bsz)))
                (cw (cond
                      ((and (= asx csx) (= asy csy) (= asz csz)) aw)
                      ((and (= bsx csx) (= bsy csy) (= bsz csz)) bw)
                      (t (,site-width csx csy csz))))
                (ad (1- aw))
                (bd (1- bw))
                (cd (1- cw))
                (,tax (+ ,ax (* ad adx)))
                (,tay (+ ,ay (* ad ady)))
                (,taz (+ ,az (* ad adz)))
                (,tbx (+ ,bx (* bd bdx)))
                (,tby (+ ,by (* bd bdy)))
                (,tbz (+ ,bz (* bd bdz)))
                (,tcx (+ ,cx (* cd cdx)))
                (,tcy (+ ,cy (* cd cdy)))
                (,tcz (+ ,cz (* cd cdz))))
           (declare (type (integer 1 4) aw bw cw)
                    (type (integer 0 3) ad bd cd)
                    (type mesh-global-tick
                          ,tax ,tay ,taz ,tbx ,tby ,tbz ,tcx ,tcy ,tcz))
           ,@body)))))
