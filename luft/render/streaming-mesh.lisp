(in-package #:luft.render)

;;; Compile a dependency-closed region: choose canonical geometry owners,
;;; freeze occupancy/material/light inputs, reuse unchanged star products,
;;; and return all prepared meshes with their exact light generation.
;;; None of these operations installs a renderer publication. Scheduling and
;;; acceptance live in streaming-publication.lisp; residency in streaming.lisp.

(defun streaming-scene-canonical-owner-closure (scene source-keys)
  "Return the exact half-open geometry-owner closure of SOURCE-KEYS.

A source column can emit primitives to its own owner and to the +X, +Y, or
+X+Y owner at high seam anchors.  Low anchors remain source-owned.  The
directional closure publishes every real primitive without inventing five
unneeded low-side empty slots around a sparse source."
  (let* ((domain (scene-domain scene))
         (maximum-x
           (1- (ceiling (luft:world-domain-x-limit domain)
                        luft:+chunk-size+)))
         (maximum-y
           (1- (ceiling (luft:world-domain-y-limit domain)
                        luft:+chunk-size+)))
         (keys (make-hash-table :test #'eql)))
    (dolist (source source-keys)
      (let ((source-x (luft:chunk-key-x source))
            (source-y (luft:chunk-key-y source)))
        (loop for x from source-x to (min maximum-x (1+ source-x)) do
          (loop for y from source-y to (min maximum-y (1+ source-y)) do
            (setf (gethash
                   (luft:chunk-key-at (* x luft:+chunk-size+)
                                      (* y luft:+chunk-size+))
                   keys)
                  t)))))
    (sort (loop for key being the hash-keys of keys collect key) #'<)))

(defun streaming-scene-dependency-guard-keys
    (scene seeds &optional (radius +streaming-owner-dependency-radius+))
  "Return every in-domain owner key within RADIUS of SEEDS.

The result deliberately includes virtual empty owners.  Canonical face, band,
and fan ownership can cross a chunk seam even when the neighboring occupancy
chunk is empty; excluding that owner drops real boundary triangles."
  (check-type radius (integer 0 *))
  (let* ((domain (scene-domain scene))
         (maximum-x
           (1- (ceiling (luft:world-domain-x-limit domain)
                        luft:+chunk-size+)))
         (maximum-y
           (1- (ceiling (luft:world-domain-y-limit domain)
                        luft:+chunk-size+)))
         (keys (make-hash-table :test #'eql)))
    (dolist (seed seeds)
      (let ((seed-x (luft:chunk-key-x seed))
            (seed-y (luft:chunk-key-y seed)))
        (loop for x from (max 0 (- seed-x radius))
                to (min maximum-x (+ seed-x radius)) do
          (loop for y from (max 0 (- seed-y radius))
                  to (min maximum-y (+ seed-y radius)) do
            (setf (gethash
                   (luft:chunk-key-at (* x luft:+chunk-size+)
                                      (* y luft:+chunk-size+))
                   keys)
                  t)))))
    (sort (loop for key being the hash-keys of keys collect key) #'<)))

(defun scene-torch-semantics-signature (scene resident-source-keys)
  "Return exact sorted resident attachment semantics and authored emission."
  (list
   (scene-torch-light-emission scene)
   (loop for attachment across (scene-torches scene)
         for support = (torch-attachment-support-cell attachment)
         when (member (luft:site-chunk-key support) resident-source-keys
                      :test #'eql)
           collect
           (list support
                 (torch-attachment-face attachment)
                 (torch-attachment-clearance-cell attachment)
                 (torch-attachment-chart-u attachment)
                 (torch-attachment-chart-v attachment)))))

(defun streaming-scene-mesh-stamp (scene output-keys bevel-width)
  "Name exact content, owner, geometry, light, and torch request inputs."
  (let ((resident-source-keys
          (sort
           (loop for key being the hash-keys of (streaming-scene-loaded scene)
                 collect key)
           #'<)))
    (list :scene-mesh-request-v2
          (scene-content-revision scene)
          (copy-list output-keys)
          bevel-width
          (scene-authored-light-revision scene)
          (scene-authored-light-provenance scene)
          (scene-voxel-light-propagation-p scene)
          (scene-torch-semantics-signature scene resident-source-keys)
          (loop for key in resident-source-keys
                collect (list key
                              (gethash key (streaming-scene-loaded scene))
                              (streaming-store-incarnation scene key))))))

(zdefun (make-streaming-region-snapshot :zone :luft/capture-mesh-region
                                        :value (length output-keys))
    (scene output-keys bevel-width &key (realize-torch-light-p t))
  "Capture one immutable union window and its width/repair guard ring."
  (check-type scene streaming-scene)
  (check-type output-keys list)
  (unless output-keys
    (error "A streaming region snapshot needs at least one output owner."))
  (let* ((output-keys
           (sort
            (remove-duplicates (copy-list output-keys) :test #'eql)
            #'<))
         (witness-keys
           (streaming-scene-dependency-guard-keys scene output-keys))
         (captured-keys
           (streaming-scene-dependency-guard-keys scene witness-keys))
         (union-neighborhood (make-hash-table :test #'eql))
         (loaded (streaming-scene-loaded scene))
         (resident-source-keys
           (sort (loop for key being the hash-keys of loaded collect key) #'<))
         (domain (scene-domain scene)))
    (unless (every (lambda (key) (member key witness-keys :test #'eql))
                   output-keys)
      (error "Streaming outputs ~S are not all resident in witness set ~S."
             output-keys witness-keys))
    (dolist (key captured-keys)
      ;; Presence records residency.  Empty fibers are still a captured answer
      ;; and must not be confused with an unknown/out-of-window chunk.
      (multiple-value-bind (fibers present-p)
          (streaming-store-fibers scene key)
        (cond
          (present-p
           (setf (gethash key union-neighborhood) fibers))
          ((streaming-scene-source scene)
           (error "Demand snapshot crossed unknown nonresident chunk ~D." key))
          (t
           (setf (gethash key union-neighborhood)
                 (luft:make-chunk-fibers domain key))))))
    (%make-streaming-mesh-snapshot
     scene (snapshot-streaming-scene-input scene)
     output-keys witness-keys resident-source-keys
     bevel-width union-neighborhood
     (streaming-scene-mesh-stamp
      scene output-keys bevel-width)
     realize-torch-light-p
     (streaming-scene-light-generation scene)
     (streaming-scene-mesh-cache scene))))

(defun streaming-owner-input-signature (snapshot key)
  "Identity of an owner's immutable geometry/material inputs and compiler.

The conservative 3x3 guard includes both sides of every chunk seam. Mutable
legacy material tables and callable fields deliberately bypass reuse."
  (let* ((scene (streaming-mesh-snapshot-input-scene snapshot))
         (materials (scene-material-cells scene)))
    (when (typep materials 'material-store)
      (list (scene-domain scene)
            (streaming-mesh-snapshot-bevel-width snapshot)
            (symbol-function 'luft:mesh-star-chunk)
            (symbol-function 'compile-surface-mesh-appearance)
            (symbol-function 'make-render-population)
            (symbol-function 'pack-terrain-appearance-codes)
            (loop for neighbor in
                  (streaming-scene-dependency-guard-keys scene (list key))
                  collect
                  (list neighbor
                        (gethash neighbor
                                 (streaming-mesh-snapshot-union-neighborhood
                                  snapshot))
                        (gethash neighbor (material-store-chunks materials))))))))

(defun streaming-products-outside-cell (scene cell)
  "Retain valid products whose sampled cell box excludes this exact edit.

Chunk identity changes on every edit, but only stars touching CELL can change
geometry or appearance. Validate the old inputs before rebasing unaffected
products; arbitrary source or compiler changes must still miss the cache."
  (let* ((products (streaming-scene-mesh-cache scene))
         (snapshot
           (and products
                (make-streaming-region-snapshot
                 scene (mapcar #'streaming-star-product-key products)
                 luft:+mesh-bevel-width+))))
    (remove-if-not
     (lambda (product)
       (let* ((key (streaming-star-product-key product))
              (x (* luft:+chunk-size+ (luft:chunk-key-x key)))
              (y (* luft:+chunk-size+ (luft:chunk-key-y key))))
         (and (not (and (<= (1- x) (luft:site-x cell) (+ x luft:+chunk-size+))
                        (<= (1- y) (luft:site-y cell) (+ y luft:+chunk-size+))))
              (equal (streaming-star-product-signature product)
                     (streaming-owner-input-signature snapshot key)))))
     products)))

(defun rebase-streaming-star-products (products changed-key fibers materials)
  "Advance only proven-unaffected products to the successor chunk identities."
  (mapcar
   (lambda (product)
     (let ((signature (streaming-star-product-signature product)))
       (make-streaming-star-product
        (streaming-star-product-key product)
        (append
         (butlast signature)
         (list (loop for input in (car (last signature))
                     collect (if (eql changed-key (first input))
                                 (list changed-key fibers materials)
                                 input))))
        (streaming-star-product-descriptors product)
        (streaming-star-product-prepared product))))
   products))

(defun make-streaming-mesh-snapshot
    (scene key bevel-width &key (realize-torch-light-p t))
  "Capture the one-owner form of MAKE-STREAMING-REGION-SNAPSHOT."
  (make-streaming-region-snapshot
   scene (list key) bevel-width
   :realize-torch-light-p realize-torch-light-p))

(zdefun (mesh-streaming-snapshot :zone :luft/mesh-region
                                 :value
                                 (length
                                  (streaming-mesh-snapshot-output-keys snapshot)))
    (snapshot &key prepare-products-p)
  "Mesh one worker-owned regional snapshot without reading owner state.

The first value is an alist of output owner to final mesh. Every output uses
the same width-one star selector against the captured guard fibers. Star
appearance reads authored cells directly and needs no meshed context owners."
  (let* ((owner-scene (streaming-mesh-snapshot-scene snapshot))
         (scene (streaming-mesh-snapshot-input-scene snapshot))
         (neighborhood (streaming-mesh-snapshot-union-neighborhood snapshot))
         (descriptors
           (compile-terrain-material-descriptors (scene-material-vocabulary scene)))
         (products nil))
    (labels ((mesh-owner (key)
               (let* ((fibers (gethash key neighborhood))
                      (signature (streaming-owner-input-signature snapshot key))
                      (cached
                        (find key (streaming-mesh-snapshot-mesh-cache snapshot)
                              :key #'streaming-star-product-key :test #'eql))
                      (reusable
                        (and signature cached
                             (equal signature (streaming-star-product-signature cached))
                             (equalp descriptors (streaming-star-product-descriptors cached))))
                      (prepared (and reusable (streaming-star-product-prepared cached)))
                      (old-mesh (and prepared (prepared-render-mesh-mesh prepared))))
                 (unless fibers
                   (error "Chunk ~D was not captured by this regional snapshot."
                          key))
                 (let ((mesh
                         (if old-mesh
                             (luft:make-surface-mesh
                              (scene-domain scene)
                              (luft:surface-mesh-star-site-words old-mesh))
                             (zone :luft/mesh-owner
                               (luft:mesh-star-chunk
                                fibers key :outside-domain-policy :air)))))
                   ;; New metadata shell: never change an installed mesh's light
                   ;; or attachment witnesses when borrowing immutable arrays.
                   (if old-mesh
                       (setf (luft:surface-mesh-appearance-codes mesh)
                             (luft:surface-mesh-appearance-codes old-mesh)
                             (luft:surface-mesh-appearance-descriptor-words mesh)
                             descriptors)
                       (zone :luft/compile-appearance
                         (compile-surface-mesh-appearance
                          mesh (scene-material-cells scene) descriptors)))
                   (push (list key signature prepared) products)
                   mesh)))
             (decorate-owners (owners)
               (decorate-scene-meshes
                owners scene :appearance-prepared-p t
                :generation-scene owner-scene
                :request-stamp (streaming-mesh-snapshot-stamp snapshot)
                :reusable-light-generation
                (streaming-mesh-snapshot-reusable-light-generation snapshot)
                :realize-torch-light-p
                (streaming-mesh-snapshot-realize-torch-light-p snapshot))))
      (handler-bind
          ((luft:missing-chunk
             (lambda (condition)
               (multiple-value-bind (fibers present-p)
                   (gethash (luft:missing-chunk-key condition) neighborhood)
                 (if present-p
                     (invoke-restart 'luft:use-chunk fibers)
                     (invoke-restart 'luft:treat-as-air)))))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let* ((output-keys
                 (streaming-mesh-snapshot-output-keys snapshot))
               (owners
                 (mapcar (lambda (key) (cons key (mesh-owner key)))
                         output-keys)))
          (multiple-value-bind (decorated generation)
              (decorate-owners owners)
            (values
             decorated nil nil generation
             (when prepare-products-p
               (loop for (key signature previous) in (nreverse products)
                     for mesh = (cdr (assoc key decorated :test #'eql))
                     collect
                     (make-streaming-star-product
                      key signature descriptors
                      (if previous
                          (%make-prepared-render-mesh
                           mesh (prepared-render-mesh-population previous))
                          (prepare-render-mesh mesh))))))))))))

(defun make-scene-regional-meshes
    (scene bevel-width &key reusable-light-generation)
  "Compile all of SCENE through the same owner/halo path used by streaming.

This is the fully resident static form of the regional compiler.  The returned
alist retains canonical chunk ownership, while callers that require the legacy
single-mesh surface can borrow it as a companion tree."
  (check-type scene scene)
  (let* ((streaming (make-streaming-scene scene))
         (source-keys
           (sort (loop for key being the hash-keys of
                       (streaming-scene-store streaming)
                       collect key)
                 #'<))
         (owner-keys
           (streaming-scene-canonical-owner-closure streaming source-keys)))
    (when reusable-light-generation
      (when (and (typep reusable-light-generation 'scene-mesh-generation)
                 (not
                  (eq (scene-authored-light-provenance scene)
                      (scene-authored-light-provenance
                       (scene-mesh-generation-scene
                        reusable-light-generation)))))
        (error "A reusable generation belongs to a different authored scene input."))
      (setf (streaming-scene-light-generation streaming)
            (etypecase reusable-light-generation
              (realized-light-generation reusable-light-generation)
              (scene-mesh-generation
               (scene-mesh-generation-light-generation
                reusable-light-generation)))))
    (dolist (key source-keys)
      (setf (gethash key (streaming-scene-loaded streaming)) bevel-width))
    (if owner-keys
        (multiple-value-bind (owners census diagnostics generation)
            (mesh-streaming-snapshot
             (make-streaming-region-snapshot
              streaming owner-keys bevel-width))
          (values
           owners census diagnostics
           (make-scene-mesh-generation-value
            scene (scene-mesh-generation-request-stamp generation)
            (scene-mesh-generation-light-generation generation)
            :mesh-entries owners)))
        (let* ((request-stamp
                 (streaming-scene-mesh-stamp
                  streaming nil bevel-width))
               (light-generation
                 (or reusable-light-generation
                     (scene-authored-light-generation scene)))
               (generation
                 (make-scene-mesh-generation-value
                  scene request-stamp
                  (etypecase light-generation
                    (realized-light-generation light-generation)
                    (scene-mesh-generation
                     (scene-mesh-generation-light-generation
                      light-generation))))))
          (values nil nil nil generation)))))

(defun mesh-streaming-chunk (scene key bevel-width)
  "Synchronously mesh KEY's canonical publication closure.

The returned root is KEY's owner mesh; the +X/+Y/+X+Y owners required by the
half-open primitive convention are companions.  The second value is the exact
immutable SCENE-MESH-GENERATION that a publication boundary must retain.  This
synchronous compiler does not mutate the streaming scene's installed reusable
generation before publication succeeds."
  (let* ((resident-source-keys
           (sort (loop for resident being the hash-keys of
                       (streaming-scene-loaded scene)
                       collect resident)
                 #'<))
         (resident-torch-p
           (plusp
            (length
             (streaming-scene-resident-torch-support-keys
              scene resident-source-keys))))
         (output-keys
           (streaming-scene-canonical-owner-closure
            scene (if resident-torch-p resident-source-keys (list key))))
         (snapshot
           (make-streaming-region-snapshot
            scene output-keys bevel-width)))
    (multiple-value-bind (owners census diagnostics generation)
        (mesh-streaming-snapshot snapshot)
      (declare (ignore census diagnostics))
      (let ((root-entry (assoc key owners :test #'eql)))
        (unless root-entry
          (error "Synchronous streaming result omitted requested chunk ~D." key))
        (let ((root (cdr root-entry)))
          (setf (luft:surface-mesh-companions root)
                (append (luft:surface-mesh-companions root)
                        (mapcar #'cdr (remove root-entry owners :test #'eq))))
          ;; As with the static single-root API, owner companions were just
          ;; aggregated.  Return a generation for the exact resulting tree,
          ;; explicitly unbound from any eventual renderer slot key.
          (values
           root
           (make-scene-mesh-generation-value
            scene (scene-mesh-generation-request-stamp generation)
            (scene-mesh-generation-light-generation generation)
            :mesh-entries (list (cons +unkeyed-scene-mesh-output+ root))
            :unkeyed-mesh-p t)))))))

(zdefmethod (production:perform-production-request
             :zone :luft/produce-mesh-region)
    ((request streaming-mesh-request))
  (multiple-value-bind (meshes census diagnostics generation products)
      (mesh-streaming-snapshot (streaming-mesh-request-snapshot request)
                               :prepare-products-p t)
    (declare (ignore meshes census diagnostics))
    (%make-streaming-mesh-result
     (mapcar (lambda (product)
               (cons (streaming-star-product-key product)
                     (streaming-star-product-prepared product)))
             products)
     generation products)))
