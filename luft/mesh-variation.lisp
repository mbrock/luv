(in-package #:luft)

(defun %vary-surface-mesh-bevel-widths
    (owner-witnesses output-owners realize-context-owners
     width-function stock-masks site-widths contract-t-junctions-p)
  "Compile one site policy and realize an owner-preserving mesh cohort."
  (check-type owner-witnesses list)
  (check-type output-owners list)
  (check-type realize-context-owners list)
  (unless owner-witnesses
    (error "A site-local bevel cohort must contain at least one witness."))
  (unless output-owners
    (error "A site-local bevel cohort must select at least one output owner."))
  (when width-function
    (check-type width-function function))
  (when stock-masks
    (check-type stock-masks vector)
    (check-type site-widths vector))
  (unless (if width-function
              (and (null stock-masks) (null site-widths))
              (and stock-masks site-widths))
    (error "Specify exactly one site-local bevel policy representation."))
  ;; Site policy remains semantic.  The renderer's positive byte-mask lane
  ;; folds through bounded sparse pages; arbitrary masks and the generic
  ;; callback retain the EQL table oracle.  Triangle realization stays in the
  ;; witness's packed scalar language, and both compilers produce the same
  ;; tight dense-or-sparse width field below.
  (let* ((domain (surface-mesh-domain (cdar owner-witnesses)))
         (owner-set (make-hash-table :test #'equal))
         (output-set (make-hash-table :test #'equal))
         (realize-set (make-hash-table :test #'equal))
         (output-witnesses nil)
         (realized-witnesses nil)
         (total-triangle-count 0)
         (width-by-site nil)
         (dense-widths nil)
         (site-count 0)
         (width-census (make-array 5 :element-type '(unsigned-byte 32)
                                    :initial-element 0))
         (maximum-width 1)
         (minimum-site-x most-positive-fixnum)
         (maximum-site-x most-negative-fixnum)
         (minimum-site-y most-positive-fixnum)
         (maximum-site-y most-negative-fixnum)
         (minimum-site-z most-positive-fixnum)
         (maximum-site-z most-negative-fixnum))
    (dolist (owner-witness owner-witnesses)
      (unless (consp owner-witness)
        (error "A bevel cohort entry must be (OWNER . SURFACE-MESH), not ~S."
               owner-witness))
      (let ((owner (car owner-witness))
            (witness (cdr owner-witness)))
        (check-type witness surface-mesh)
        (when (nth-value 1 (gethash owner owner-set))
          (error "Duplicate bevel cohort owner ~S." owner))
        (setf (gethash owner owner-set) t)
        (unless (world-domain= domain (surface-mesh-domain witness))
          (error "Bevel cohort owner ~S has a different world domain." owner))
        (unless (= 1 (surface-mesh-bevel-width witness))
          (error "A site-local bevel witness must have width one, not ~D."
                 (surface-mesh-bevel-width witness)))
        (incf total-triangle-count (surface-mesh-triangle-count witness))))
    (dolist (owner output-owners)
      (when (nth-value 1 (gethash owner output-set))
        (error "Duplicate bevel cohort output owner ~S." owner))
      (unless (nth-value 1 (gethash owner owner-set))
        (error "Bevel cohort output owner ~S has no witness." owner))
      (setf (gethash owner output-set) t))
    (dolist (owner (append output-owners realize-context-owners))
      (unless (nth-value 1 (gethash owner owner-set))
        (error "Bevel cohort realization owner ~S has no witness." owner))
      (setf (gethash owner realize-set) t))
    (setf output-witnesses
          (remove-if-not
           (lambda (owner-witness)
             (nth-value 1 (gethash (car owner-witness) output-set)))
           owner-witnesses)
          realized-witnesses
          (remove-if-not
           (lambda (owner-witness)
             (nth-value 1 (gethash (car owner-witness) realize-set)))
           owner-witnesses))
    (if (and stock-masks
             (%paged-byte-stock-mask-policy-p
              domain stock-masks site-widths))
        (multiple-value-setq
            (width-by-site dense-widths site-count maximum-width
             minimum-site-x maximum-site-x
             minimum-site-y maximum-site-y
             minimum-site-z maximum-site-z)
          (%compile-paged-byte-stock-mask-bevel-sites
           owner-witnesses stock-masks site-widths width-census))
        (progn
          (setf width-by-site
                (make-hash-table
                 :test #'eql
                 :size
                 (max 16 (truncate total-triangle-count 8))))
          (labels ((owner-key (x y z)
                     (multiple-value-bind
                           (site-x site-y site-z
                            direction-x direction-y direction-z)
                         (%unit-bevel-point-owner x y z)
                       (declare (ignore direction-x direction-y direction-z))
                       (setf minimum-site-x (min minimum-site-x site-x)
                             maximum-site-x (max maximum-site-x site-x)
                             minimum-site-y (min minimum-site-y site-y)
                             maximum-site-y (max maximum-site-y site-y)
                             minimum-site-z (min minimum-site-z site-z)
                             maximum-site-z (max maximum-site-z site-z))
                       (%lattice-key site-x site-y site-z)))
                   (observe-stock (key stock)
                     (pushnew stock (gethash key width-by-site) :test #'=))
                   (observe-stock-mask (key stock-mask)
                     (setf (gethash key width-by-site)
                           (the fixnum
                             (logior stock-mask
                                     (the fixnum
                                       (gethash key width-by-site 0)))))))
            (declare
             (inline owner-key observe-stock observe-stock-mask)
             (ftype (function
                      (mesh-global-tick mesh-global-tick mesh-global-tick)
                      fixnum)
                    owner-key)
             (ftype (function (fixnum fixnum) *)
                    observe-stock observe-stock-mask))
            (if stock-masks
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
                        (unless (typep stock-mask '(unsigned-byte 61))
                          (error "Mesh stock ~D has invalid compiled bevel mask ~S."
                                 stock stock-mask))
                        (let* ((stock-mask (the fixnum stock-mask))
                               (a (owner-key ax ay az))
                               (b (owner-key bx by bz))
                               (c (owner-key cx cy cz)))
                          (observe-stock-mask a stock-mask)
                          (unless (= b a)
                            (observe-stock-mask b stock-mask))
                          (unless (or (= c a) (= c b))
                            (observe-stock-mask c stock-mask)))))))
                (dolist (owner-witness owner-witnesses)
                  (let ((witness (cdr owner-witness)))
                    (%do-surface-mesh-triangle-scalars
                        (witness kind stock ambient mask
                                 ax ay az bx by bz cx cy cz)
                      (declare (ignore kind ambient mask))
                      (let ((a (owner-key ax ay az))
                            (b (owner-key bx by bz))
                            (c (owner-key cx cy cz)))
                        (observe-stock a stock)
                        (unless (= b a)
                          (observe-stock b stock))
                        (unless (or (= c a) (= c b))
                          (observe-stock c stock))))))))
          (labels ((record-width (key width)
                     (unless (and (integerp width)
                                  (<= 1 width (/ +mesh-cell-size+ 2)))
                       (error "Site-local bevel policy assigned invalid width ~S at ~S."
                              width (list (%lattice-key-x key)
                                          (%lattice-key-y key)
                                          (%lattice-key-z key))))
                     (setf (gethash key width-by-site) width
                           maximum-width (max maximum-width width))
                     (incf (aref width-census width))))
            (if stock-masks
                (maphash
                 (lambda (key site-mask)
                   (unless (and (plusp site-mask)
                                (< site-mask (length site-widths)))
                     (error "Incident mesh stocks compiled to invalid bevel mask ~D at ~S."
                            site-mask (list (%lattice-key-x key)
                                            (%lattice-key-y key)
                                            (%lattice-key-z key))))
                   (record-width key (aref site-widths site-mask)))
                 width-by-site)
                (maphash
                 (lambda (key stocks)
                   (record-width
                    key
                    (funcall width-function
                             (%lattice-key-x key)
                             (%lattice-key-y key)
                             (%lattice-key-z key)
                             (sort stocks #'<))))
                 width-by-site)))
          (setf site-count (hash-table-count width-by-site))
          (when (zerop site-count)
            (setf minimum-site-x 0 maximum-site-x 0
                  minimum-site-y 0 maximum-site-y 0
                  minimum-site-z 0 maximum-site-z 0))))
    (let* ((site-x-span (1+ (- maximum-site-x minimum-site-x)))
           (site-y-span (1+ (- maximum-site-y minimum-site-y)))
           (site-z-span (1+ (- maximum-site-z minimum-site-z)))
           (site-volume (* site-x-span site-y-span site-z-span))
           (dense-widths
             (or dense-widths
                 (when (and (plusp site-count)
                            (<= site-volume +dense-bevel-site-field-byte-limit+)
                            (<= site-volume
                                (* +dense-bevel-site-field-sparsity-limit+
                                   site-count)))
                   (make-array site-volume :element-type '(unsigned-byte 8)
                                            :initial-element 0))))
           (packing
             (%make-spatial-edge-packing-for-box
              minimum-site-x (1+ maximum-site-x)
              minimum-site-y (1+ maximum-site-y)))
           (builder-by-owner (make-hash-table :test #'equal))
           (live-triangle-counts-by-owner
             (make-hash-table :test #'equal)))
      (declare (type fixnum site-x-span site-y-span site-z-span
                            site-volume site-count))
      (when (and dense-widths width-by-site
                 (plusp (hash-table-count width-by-site)))
        (maphash
         (lambda (key width)
           (setf (aref dense-widths
                       (%dense-bevel-site-index
                        (%lattice-key-x key)
                        (%lattice-key-y key)
                        (%lattice-key-z key)
                        minimum-site-x minimum-site-y minimum-site-z
                        site-y-span site-z-span))
                 width))
         width-by-site)
        (clrhash width-by-site))
      (dolist (owner-witness realized-witnesses)
        (let* ((owner (car owner-witness))
               (witness (cdr owner-witness))
               (builder (%make-surface-mesh-builder domain maximum-width)))
          (setf (surface-mesh-builder-singular-star-count builder)
                (surface-mesh-singular-star-count witness)
                (gethash owner builder-by-owner) builder
                (gethash owner live-triangle-counts-by-owner)
                (make-array 3 :element-type '(unsigned-byte 32)
                              :initial-element 0))))
      (let* ((field
               (%make-bevel-site-width-field
                width-by-site dense-widths
                minimum-site-x minimum-site-y minimum-site-z
                site-y-span site-z-span))
             (plan
               (%plan-variable-bevel-transitions
                field owner-witnesses output-witnesses
                output-set realize-set live-triangle-counts-by-owner
                packing contract-t-junctions-p)))
        (multiple-value-bind (owner-meshes context-owner-meshes)
            (%emit-variable-bevel-transition-owners
             field realized-witnesses output-set builder-by-owner
             live-triangle-counts-by-owner packing plan)
          (let ((candidate-splits
                  (%bevel-transition-plan-candidate-splits plan)))
            (values
             owner-meshes
             width-census
             (list
              :collapsed-triangle-count
              (%bevel-transition-plan-collapsed-triangle-count plan)
              :context-collapsed-triangle-count
              (%bevel-transition-plan-context-collapsed-triangle-count plan)
              :unmatched-edge-count
              (%bevel-transition-plan-unmatched-edge-count plan)
              :repaired-edge-count
              (hash-table-count
               (%bevel-transition-plan-repair-splits plan))
              :residual-edge-count
              (%bevel-transition-plan-residual-edge-count plan)
              :candidate-splits
              (loop for edge being the hash-keys of candidate-splits
                      using (hash-value points)
                    append
                    (multiple-value-bind (left right)
                        (%spatial-edge-points packing edge)
                      (loop for point in points
                            collect
                            (list left (%global-mesh-point-list point)
                                  right)))))
             context-owner-meshes)))))))

(defun %vary-one-surface-mesh-bevel-widths
    (witness width-function contract-t-junctions-p)
  (check-type width-function function)
  (multiple-value-bind (owner-meshes width-census diagnostics)
      (%vary-surface-mesh-bevel-widths
       (list (cons nil witness)) (list nil)
       nil width-function nil nil contract-t-junctions-p)
    (values (cdar owner-meshes) width-census diagnostics)))

(defun vary-surface-mesh-bevel-widths (witness width-function)
  "Evaluate one closed width-one WITNESS at a local width per vertex site.

WIDTH-FUNCTION is called once for each canonical lattice vertex as
  (WIDTH-FUNCTION X Y Z INCIDENT-STOCKS)
where INCIDENT-STOCKS is a sorted, duplicate-free list of the packed stocks on
witness triangles using that site.  It must return an integer width from one
through four.

Every witness vertex has the exact affine form 8*S + Q with Q in {-1,0,1}^3.
The result replaces it by 8*S + WIDTH(S)*Q.  Since every incident primitive
uses the same canonical S, shared vertices remain equal without stitching.
At the medial limit a witness triangle can collapse to three collinear points.
The result contracts that triangle by splitting its surviving neighbour's long
edge at the middle point, eliminating the otherwise visible T-junction without
inventing a surface.  WITNESS remains the rebuild oracle for topology and
uniform-width geometry.  Transition triangles may leave the uniform mesher's
26 exact normal directions.  The packed trit normal remains an orientation
witness; fragment shading derives the actual primitive normal from world-space
position derivatives, so the new directions are not lighting-quantized.

The second value is a five-entry site census indexed by width.  The third is a
diagnostic plist containing the collapsed-triangle, locally unmatched-edge,
repaired-edge, and residual-edge counts.  For the required closed width-one
witness, only a collapse can change edge parity, so the queried local counts
are the complete transition defect.  This production entry point always
contracts medial T-junctions; the deliberately open study surface has a
separate diagnostic name."
  (%vary-one-surface-mesh-bevel-widths witness width-function t))

(defun vary-uncontracted-surface-mesh-bevel-widths-diagnostic
    (witness width-function)
  "Return the deliberately open pre-repair variable-width study surface."
  (%vary-one-surface-mesh-bevel-widths witness width-function nil))

(defun %vary-one-surface-mesh-bevel-widths-from-stock-masks
    (witness stock-masks site-widths contract-t-junctions-p)
  (check-type stock-masks vector)
  (check-type site-widths vector)
  (multiple-value-bind (owner-meshes width-census diagnostics)
      (%vary-surface-mesh-bevel-widths
       (list (cons nil witness)) (list nil)
       nil nil stock-masks site-widths contract-t-junctions-p)
    (values (cdar owner-meshes) width-census diagnostics)))

(defun vary-surface-mesh-bevel-widths-from-stock-masks
    (witness stock-masks site-widths)
  "Evaluate a closed width-one WITNESS from an incident-stock mask policy.

STOCK-MASKS is indexed by packed triangle stock.  The masks of every stock
incident on a canonical vertex site are combined with LOGIOR, then that mask
indexes SITE-WIDTHS.  This is the dense production form of the generic callback
contract: it preserves the same shared site field and the same realization and
repair algorithm without constructing stock lists in the triangle loop.

Every referenced stock mask must be a positive fixnum bit mask, and each
combined mask must be a valid SITE-WIDTHS index.  Index zero is unused; every
selected entry must be an integer width from one through four.  This canonical
entry point always repairs medial T-junctions."
  (%vary-one-surface-mesh-bevel-widths-from-stock-masks
   witness stock-masks site-widths t))

(defun vary-uncontracted-surface-mesh-bevel-widths-from-stock-masks-diagnostic
    (witness stock-masks site-widths)
  "Return the dense-policy variable-width study surface before repair."
  (%vary-one-surface-mesh-bevel-widths-from-stock-masks
   witness stock-masks site-widths nil))

(defun vary-surface-mesh-cohort-bevel-widths-from-stock-masks
    (owner-witnesses stock-masks site-widths
     &key (output-owners nil output-owners-p)
          realize-context-owners)
  "Evaluate an owner-keyed width-one cohort from one shared stock-mask policy.

OWNER-WITNESSES is a nonempty alist of stable owner keys to width-one surface
meshes in one world domain.  The incident stock masks of every witness are
compiled into one canonical site field; collapse discovery and T-junction
repair likewise see the entire cohort.  Each surviving source triangle and
every repair child is emitted to the source triangle's original owner.

When OUTPUT-OWNERS is omitted, every witness owner is returned.  When supplied,
it is a nonempty subset of the witness keys.  Every witness contributes to the
shared site policy and repair plan.  REALIZE-CONTEXT-OWNERS may additionally
select nonpublished owners whose transformed surface is needed by a cold query,
such as an attachment frame whose canonical primitive belongs to a neighboring
chunk.  Context realization never expands the output publication set.

The first value is a fresh alist, in OWNER-WITNESSES order, containing only the
selected owners.  The second value is the shared five-entry site-width census.
The third is the transition diagnostic plist; its residual count covers only
collapse/repair neighborhoods relevant to the selected output interior.  The
fourth value is a fresh alist of realized context owners, also in witness order
and excluding every selected output owner."
  (check-type owner-witnesses list)
  (check-type stock-masks vector)
  (check-type site-widths vector)
  (%vary-surface-mesh-bevel-widths
   owner-witnesses
   (if output-owners-p
       output-owners
       (mapcar #'car owner-witnesses))
   realize-context-owners
   nil stock-masks site-widths t))
