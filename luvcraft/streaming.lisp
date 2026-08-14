;;; Asynchronous chunk residency: generation and meshing off the frame thread.
;;;
;;; The world/canvas thread is the only writer of residency and the only owner
;;; of GPU objects.  It ships immutable work descriptions (chunk load requests,
;;; mesh snapshots) to the production worker, then validates each returned
;;; product against the current desired set and dependency stamps before
;;; publishing a bounded number of results per frame.  Stale products fail
;;; validation harmlessly instead of requiring cancellation of active work.

(in-package #:luv)

(defclass block-mesh-production-request (production-request)
  ((absent-neighbor-policy
    :initarg :absent-neighbor-policy
    :reader block-mesh-production-request-absent-neighbor-policy)
   (snapshot :initarg :snapshot :reader block-mesh-production-request-snapshot)))

(defmethod perform-production-request ((request block-mesh-production-request))
  (mesh-block-snapshot
   (make-instance
    'exposed-face-mesher
    :absent-neighbor-policy
    (block-mesh-production-request-absent-neighbor-policy request))
   (block-mesh-production-request-snapshot request)))

(defclass block-chunk-load-payload ()
  ((key :initarg :key :reader block-chunk-load-payload-key)
   (palette :initarg :palette :reader block-chunk-load-payload-palette)
   (indices :initarg :indices :reader block-chunk-load-payload-indices)))

(defclass block-chunk-load-request (production-request)
  ((demand-token :initarg :demand-token
                 :reader block-chunk-load-request-demand-token))
  (:documentation
   "The source-neutral half of asynchronous chunk loading.

A concrete world source subclasses this with whatever it needs to rebuild
one chunk in isolation on the worker, and answers
PERFORM-PRODUCTION-REQUEST with a BLOCK-CHUNK-LOAD-PAYLOAD.  Publication
and demand-token validation live here, on the base class."))

(defgeneric make-block-chunk-load-request (source world key demand-token priority)
  (:documentation
   "Make the off-thread load request which produces resident chunk KEY.

Return NIL when SOURCE cannot generate chunks away from the owning thread;
the session then keeps KEY desired without scheduling work for it.  A
returned request must carry the production key (:LOAD KEY) so it matches
the session's outstanding-work and cancellation bookkeeping."))

(defmethod make-block-chunk-load-request
    ((source t) world key demand-token priority)
  (declare (ignore world key demand-token priority))
  nil)

;;; The little world's realization of the load protocol.  The request captures
;;; everything the worker needs as plain values: the seed, the chunk shape,
;;; deterministic landmarks owned by the chunk, and the sparse edits inside it.

(defclass little-world-load-request (block-chunk-load-request)
  ((seed :initarg :seed :reader little-world-load-request-seed)
   (width :initarg :width :reader little-world-load-request-width)
   (height :initarg :height :reader little-world-load-request-height)
   (depth :initarg :depth :reader little-world-load-request-depth)
   (landmarks :initarg :landmarks :initform nil
              :reader little-world-load-request-landmarks)
   (edits :initarg :edits :initform nil
          :reader little-world-load-request-edits)))

(defmethod make-block-chunk-load-request
    ((source little-world-source) world key demand-token priority)
  (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (captured-edits nil))
    (maphash
     (lambda (coordinate block)
       (destructuring-bind (x y z) coordinate
         (when (and (= (floor x width) (first key))
                    (= (floor y height) (second key))
                    (= (floor z depth) (third key)))
           (push (list block x y z) captured-edits))))
     (block-edit-overlay-entries (little-world-source-edits source)))
    (make-instance
     'little-world-load-request
     :key (list :load key)
     :priority priority
     :seed (little-world-source-seed source)
     :demand-token demand-token
     :width width :height height :depth depth
     :landmarks (little-world-landmarks-for-chunk source world key)
     :edits captured-edits)))

(defmethod perform-production-request ((request little-world-load-request))
  "Generate one isolated chunk and transfer only its dense content columns."
  (destructuring-bind (chunk-x chunk-y chunk-z)
      (second (production-request-key request))
    (let* ((source (make-instance 'little-world-source
                                  :seed (little-world-load-request-seed request)))
           (world (make-block-world
                   :chunk-width (little-world-load-request-width request)
                   :chunk-height (little-world-load-request-height request)
                   :chunk-depth (little-world-load-request-depth request)
                   :source source)))
      (materialize-block-world-chunk source world chunk-x chunk-y chunk-z)
      (dolist (landmark (little-world-load-request-landmarks request))
        (destructuring-bind (block x y z) landmark
          (setf (world-block-at world x y z) block)))
      (dolist (edit (little-world-load-request-edits request))
        (destructuring-bind (block x y z) edit
          (setf (world-block-at world x y z) block)))
      (let ((chunk (world-chunk-at world chunk-x chunk-y chunk-z)))
        (with-block-content-storage (domain palette indices) chunk
          (declare (ignore domain))
          (make-instance 'block-chunk-load-payload
                         :key (list chunk-x chunk-y chunk-z)
                         :palette palette :indices indices))))))

(defclass luvcraft-chunk-product ()
  ((coordinate :initarg :coordinate
               :reader luvcraft-chunk-product-coordinate)
   (dependency-stamp :initarg :dependency-stamp
                     :reader luvcraft-chunk-product-dependency-stamp)
   (mesh :initarg :mesh :reader luvcraft-chunk-product-mesh)
   (vertex-buffer :initarg :vertex-buffer
                  :reader luvcraft-chunk-product-vertex-buffer)))

(defun cancel-luvcraft-chunk-production (session key)
  (dolist (kind '(:load :mesh))
    (let ((production-key (list kind key)))
      ;; Active work cannot be canceled, so retain its ticket until its result
      ;; returns and fails desired-set/incarnation validation.
      (when (cancel-production-request
             (luvcraft-session-production-system session) production-key)
        (remhash production-key
                 (luvcraft-session-outstanding-production session))))))

(defun luvcraft-player-chunk-center (world player)
  (if player
      (let ((shape (voxel-space-chunk-shape (block-world-space world))))
        (list (floor (player-x player) (chunk-shape-width shape))
              (floor (player-z player) (chunk-shape-depth shape))))
      '(0 0)))

(defun maintain-generated-luvcraft-residency
    (session world player radius)
  (let ((center (luvcraft-player-chunk-center world player)))
    (unless (equal center (luvcraft-session-residency-center session))
      (let ((desired (luvcraft-session-desired-chunks session))
            (next-desired (make-hash-table :test #'equal)))
        (loop for chunk-x from (- (first center) radius)
                to (+ (first center) radius) do
          (loop for chunk-z from (- (second center) radius)
                  to (+ (second center) radius)
                for key = (chunk-key chunk-x 0 chunk-z)
                do (setf (gethash key next-desired)
                         (or (gethash key desired)
                             (incf (luvcraft-session-next-residency-demand
                                    session))))))
        (maphash
         (lambda (old-key token)
           (declare (ignore token))
           (unless (gethash old-key next-desired)
             (cancel-luvcraft-chunk-production session old-key)))
         desired)
        (clrhash desired)
        (maphash (lambda (key token)
                   (setf (gethash key desired) token))
                 next-desired)
        (setf (luvcraft-session-residency-center session) center)
        ;; Eviction is an owner-side publication.  Pending work is either
        ;; canceled before it starts or allowed to finish and fail its
        ;; desired-set/incarnation validation harmlessly.
        (dolist (chunk (resident-world-chunks world))
          (let ((key (block-chunk-key chunk)))
            (unless (gethash key desired)
              (destructuring-bind (x y z) key
                (remove-world-chunk world x y z))
              (cancel-luvcraft-chunk-production session key))))))))

(defun maintain-static-luvcraft-residency (session world player)
  "Treat a caller-owned resident set as desired without loading or eviction."
  (let ((desired (luvcraft-session-desired-chunks session))
        (resident (make-hash-table :test #'equal)))
    (dolist (chunk (resident-world-chunks world))
      (let ((key (block-chunk-key chunk)))
        (setf (gethash key resident) t)
        (unless (gethash key desired)
          (setf (gethash key desired)
                (incf (luvcraft-session-next-residency-demand session))))))
    (let ((departed nil))
      (maphash (lambda (key token)
                 (declare (ignore token))
                 (unless (gethash key resident)
                   (push key departed)))
               desired)
      (dolist (key departed)
        (remhash key desired)
        (cancel-luvcraft-chunk-production session key)))
    (setf (luvcraft-session-residency-center session)
          (luvcraft-player-chunk-center world player))))

(defgeneric maintain-block-world-residency (source session world)
  (:documentation
   "Reconcile SESSION's desired chunk set as WORLD's SOURCE prescribes.

A source which can rematerialize its terrain keeps a sliding window around
the player, letting the session load and evict chunks as the player moves.
The default method treats a caller-owned resident set as desired without
loading or eviction: those chunks still receive immutable mesh production,
but nothing could regenerate them once evicted."))

(defmethod maintain-block-world-residency ((source t) session world)
  (maintain-static-luvcraft-residency
   session world (luvcraft-session-player session)))

(defmethod maintain-block-world-residency
    ((source little-world-source) session world)
  (let ((player (luvcraft-session-player session))
        (radius (luvcraft-session-residency-radius session)))
    ;; Without a player or radius there is no window to slide, so even a
    ;; generative world falls back to caller-owned residency.
    (if (and player radius)
        (maintain-generated-luvcraft-residency session world player radius)
        (call-next-method))))

(defun maintain-luvcraft-residency (session)
  "Reconcile desired residency without generating chunks on the frame thread."
  (let ((world (luvcraft-session-world session)))
    (maintain-block-world-residency
     (block-world-source world) session world)))

(defun chunk-light-stamp-revision (chunk)
  "The chunk's light revision, or -1 while it has no published field."
  (let ((field (block-chunk-light-field chunk)))
    (if field (chunk-light-field-revision field) -1)))

(defun chunk-light-stamp-boundary-revision (chunk dx dy dz)
  (let ((field (block-chunk-light-field chunk)))
    (if field (chunk-light-field-boundary-revision field dx dy dz) -1)))

(defun chunk-mesh-dependency-stamp (world chunk)
  "Describe exactly which resident data CHUNK's exposed mesh observes.

Content and light are named as separate revision domains: relighting must
invalidate a mesh without impersonating an authored edit, and the stamp
preserves which domain made a product stale."
  (let* ((coordinate
           (chunk-domain-coordinate (block-chunk-domain chunk)))
         (x (chunk-coordinate-x coordinate))
         (y (chunk-coordinate-y coordinate))
         (z (chunk-coordinate-z coordinate)))
    (cons
     (list (block-chunk-key chunk)
           (block-chunk-incarnation chunk)
           (block-chunk-revision chunk)
           (chunk-light-stamp-revision chunk))
     (loop for (dx dy dz) in *chunk-neighbor-directions*
           collect
           (multiple-value-bind (neighbor present-p)
               (world-chunk-at world (+ x dx) (+ y dy) (+ z dz))
             (if present-p
                 ;; Only the neighbor boundary facing this chunk contributes.
                 (list (block-chunk-key neighbor)
                       (block-chunk-incarnation neighbor)
                       (block-chunk-boundary-revision
                        neighbor (- dx) (- dy) (- dz))
                       (chunk-light-stamp-boundary-revision
                        neighbor (- dx) (- dy) (- dz)))
                 '(nil)))))))

(defun luvcraft-session-products-in-order (session)
  (let ((products (luvcraft-session-chunk-products session)))
    (loop for product being the hash-values of products
          collect product into result
          finally
             (return
               (sort result
                     (lambda (left right)
                       (let ((a (luvcraft-chunk-product-coordinate left))
                             (b (luvcraft-chunk-product-coordinate right)))
                         (or (< (chunk-coordinate-x a) (chunk-coordinate-x b))
                             (and (= (chunk-coordinate-x a)
                                     (chunk-coordinate-x b))
                                  (or (< (chunk-coordinate-y a)
                                         (chunk-coordinate-y b))
                                      (and (= (chunk-coordinate-y a)
                                              (chunk-coordinate-y b))
                                           (< (chunk-coordinate-z a)
                                              (chunk-coordinate-z b)))))))))))))

(defun luvcraft-session-mesh (session)
  "Return a combined, inspectable snapshot of SESSION's chunk meshes."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (vertex-count 0)
        (face-count 0))
    (dolist (product (luvcraft-session-products-in-order session))
      (let ((mesh (luvcraft-chunk-product-mesh product)))
        (loop for component across (block-mesh-vertices mesh)
              do (vector-push-extend component vertices))
        (incf vertex-count (block-mesh-vertex-count mesh))
        (incf face-count (block-mesh-face-count mesh))))
    (make-instance 'block-mesh :vertices vertices
                               :vertex-count vertex-count
                               :face-count face-count)))

(defun destroy-luvcraft-chunk-products (session)
  (dolist (products (list (luvcraft-session-chunk-products session)
                          (luvcraft-session-staged-chunk-products session)))
    (maphash
     (lambda (key product)
       (declare (ignore key))
       (let ((buffer (luvcraft-chunk-product-vertex-buffer product)))
         (when buffer (destroy buffer))))
     products)
    (clrhash products))
  (values))

(defun current-luvcraft-chunk-product-p (session key product)
  (let* ((world (luvcraft-session-world session))
         (chunk (apply #'world-chunk-at world key)))
    (and chunk
         (gethash key (luvcraft-session-desired-chunks session))
         (equal (luvcraft-chunk-product-dependency-stamp product)
                (chunk-mesh-dependency-stamp world chunk)))))

(defun stale-luvcraft-visible-product-keys (session)
  "Return the keys of visible products which no longer describe the world."
  (let ((keys nil))
    (maphash
     (lambda (key product)
       (when (and (gethash key (luvcraft-session-desired-chunks session))
                  (not (current-luvcraft-chunk-product-p session key product)))
         (push key keys)))
     (luvcraft-session-chunk-products session))
    keys))

(defun chunk-neighbor-key (key direction)
  (destructuring-bind (x y z) key
    (destructuring-bind (dx dy dz) direction
      (chunk-key (+ x dx) (+ y dy) (+ z dz)))))

(defun luvcraft-stale-product-components (session)
  "Group stale visible chunk products connected across block faces."
  (let ((remaining (make-hash-table :test #'equal))
        (components nil))
    (dolist (key (stale-luvcraft-visible-product-keys session))
      (setf (gethash key remaining) t))
    (loop while (plusp (hash-table-count remaining))
          for seed = (loop for key being the hash-keys of remaining
                           do (return key))
          do (let ((frontier (list seed))
                   (component nil))
               (remhash seed remaining)
               (loop while frontier
                     for key = (pop frontier)
                     do (push key component)
                        (dolist (direction *chunk-neighbor-directions*)
                          (let ((neighbor
                                  (chunk-neighbor-key key direction)))
                            (when (gethash neighbor remaining)
                              (remhash neighbor remaining)
                              (push neighbor frontier)))))
               (push component components)))
    components))

(defun ready-luvcraft-mesh-publication-groups (session)
  "Return staged mesh groups which may replace visible products together.

A block edit can invalidate its own chunk and each face-neighbor whose mesh
observed the edited boundary.  Connected stale visible products therefore
form one publication cohort: the old cohort remains drawable until every
current replacement is staged.  A chunk with no visible predecessor may be
published independently."
  (let* ((staged (luvcraft-session-staged-chunk-products session))
         (stale-components (luvcraft-stale-product-components session))
         (stale-keys (make-hash-table :test #'equal))
         (groups nil))
    (dolist (component stale-components)
      (dolist (key component)
        (setf (gethash key stale-keys) t))
      (when (every (lambda (key)
                     (let ((product (gethash key staged)))
                       (and product
                            (current-luvcraft-chunk-product-p
                             session key product))))
                   component)
        (push component groups)))
    (maphash
     (lambda (key product)
       (when (and (not (gethash key stale-keys))
                  (current-luvcraft-chunk-product-p session key product))
         (push (list key) groups)))
     staged)
    groups))

(defun discard-stale-luvcraft-staged-products (session)
  (let ((discarded nil)
        (staged (luvcraft-session-staged-chunk-products session)))
    (maphash
     (lambda (key product)
       (unless (current-luvcraft-chunk-product-p session key product)
         (let ((buffer (luvcraft-chunk-product-vertex-buffer product)))
           (when buffer (destroy buffer)))
         (push key discarded)))
     staged)
    (dolist (key discarded) (remhash key staged))
    (length discarded)))

(defun publish-ready-luvcraft-meshes (session)
  "Atomically replace every complete stale mesh cohort at a frame boundary."
  (let ((published 0)
        (retired nil)
        (products (luvcraft-session-chunk-products session))
        (staged (luvcraft-session-staged-chunk-products session)))
    (dolist (group (ready-luvcraft-mesh-publication-groups session))
      ;; No rendering can interleave with these owner-thread hash updates.
      ;; Install the complete cohort before retiring any of its predecessors.
      (dolist (key group)
        (let ((candidate (gethash key staged))
              (old (gethash key products)))
          (setf (gethash key products) candidate)
          (remhash key staged)
          (when old (push old retired))
          (incf published))))
    (dolist (product retired)
      (let ((buffer (luvcraft-chunk-product-vertex-buffer product)))
        (when buffer (destroy buffer))))
    published))

(defun chunk-key-distance-squared (key center)
  (+ (expt (- (first key) (first center)) 2)
     (expt (- (third key) (second center)) 2)))

(defun chunk-key-nearer-p (left right center)
  (let ((left-distance (chunk-key-distance-squared left center))
        (right-distance (chunk-key-distance-squared right center)))
    (or (< left-distance right-distance)
        (and (= left-distance right-distance)
             (loop for a in left
                   for b in right
                   when (/= a b) return (< a b)
                   finally (return nil))))))

(defun schedule-luvcraft-chunk-loads (session)
  (let* ((world (luvcraft-session-world session))
         (source (block-world-source world))
         (center (luvcraft-session-residency-center session))
         (outstanding (luvcraft-session-outstanding-production session))
         (candidates nil))
    (maphash
     (lambda (key demand-token)
       (let ((production-key (list :load key)))
         (unless (or (nth-value 1 (apply #'world-chunk-at world key))
                     (gethash production-key outstanding))
           (push (list key demand-token production-key) candidates))))
     (luvcraft-session-desired-chunks session))
    (setf candidates
          (sort candidates
                (lambda (left right)
                  (chunk-key-nearer-p (first left) (first right) center))))
    (loop repeat (luvcraft-session-load-schedule-limit session)
          for candidate in candidates
          do (destructuring-bind (key demand-token production-key) candidate
               (let ((request
                       (make-block-chunk-load-request
                        source world key demand-token
                        (chunk-key-distance-squared key center))))
                 (when request
                   (setf (gethash production-key outstanding)
                         (schedule-production-request
                          (luvcraft-session-production-system session)
                          request))))))))

(defun schedule-luvcraft-meshes (session)
  "Capture at most the configured number of immutable mesh inputs this frame."
  (let* ((world (luvcraft-session-world session))
         (products (luvcraft-session-chunk-products session))
         (staged (luvcraft-session-staged-chunk-products session))
         (outstanding (luvcraft-session-outstanding-production session))
         (center (luvcraft-session-residency-center session))
         (candidates nil))
    (dolist (chunk (resident-world-chunks world))
      (let* ((key (block-chunk-key chunk))
             (production-key (list :mesh key))
             (stamp (chunk-mesh-dependency-stamp world chunk))
             (old (gethash key products))
             (candidate (gethash key staged)))
        (when (and (gethash key (luvcraft-session-desired-chunks session))
                   (not (gethash production-key outstanding))
                   (not (and (or candidate old)
                             (equal stamp
                                    (luvcraft-chunk-product-dependency-stamp
                                     (or candidate old))))))
          (push (list (chunk-key-distance-squared key center)
                      chunk key production-key stamp)
                candidates))))
    (setf candidates (sort candidates #'< :key #'first))
    (loop repeat (luvcraft-session-mesh-capture-limit session)
          for candidate in candidates
          do (destructuring-bind (priority chunk key production-key stamp)
                 candidate
               (declare (ignore key))
               (let* ((snapshot
                        (make-block-mesh-snapshot world chunk stamp))
                      (request
                        (make-instance
                         'block-mesh-production-request
                         :key production-key :priority priority
                         :absent-neighbor-policy
                         (exposed-face-mesher-absent-neighbor-policy
                          (luvcraft-session-mesher session))
                         :snapshot snapshot)))
                 (setf (gethash production-key outstanding)
                       (schedule-production-request
                        (luvcraft-session-production-system session) request)))))))

(defgeneric publish-production-result (session request value)
  (:documentation
   "Validate and accept one worker product on the render/GPU owning thread.

This is the owner-side mirror of PERFORM-PRODUCTION-REQUEST: each request
class carries its own publication rule, so a new kind of asynchronous product
plugs in with one method on each generic rather than an edit to the drain
loop.  A stale product simply fails its own validation here.  Product kinds
whose visible dependencies span several chunks may stage a complete candidate
here and install its publication cohort later at the frame boundary."))

(defmethod publish-production-result
    ((session luvcraft-session) (request block-mesh-production-request) mesh)
  (let* ((snapshot (block-mesh-production-request-snapshot request))
         (key (block-mesh-snapshot-key snapshot))
         (world (luvcraft-session-world session))
         (chunk (apply #'world-chunk-at world key)))
    (when (and chunk
               (gethash key (luvcraft-session-desired-chunks session))
               (equal (block-mesh-snapshot-dependency-stamp snapshot)
                      (chunk-mesh-dependency-stamp world chunk)))
      (let ((buffer nil) (completed-p nil)
            (old (gethash key
                          (luvcraft-session-staged-chunk-products session))))
        (unwind-protect
             (progn
               (setf buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label (format nil "block chunk ~{~D~^,~} async mesh" key)
                       :size (max 4 (* 4 (length (block-mesh-vertices mesh))))
                       :usage '(:vertex))))
               (write-buffer buffer (block-mesh-vertices mesh))
               (setf (gethash key
                              (luvcraft-session-staged-chunk-products session))
                     (make-instance
                      'luvcraft-chunk-product
                      :coordinate
                      (chunk-domain-coordinate (block-chunk-domain chunk))
                      :dependency-stamp
                      (block-mesh-snapshot-dependency-stamp snapshot)
                      :mesh mesh :vertex-buffer buffer)
                     completed-p t)
               (when old
                 (let ((old-buffer
                         (luvcraft-chunk-product-vertex-buffer old)))
                   (when old-buffer (destroy old-buffer)))))
          (unless completed-p
            (when buffer (destroy buffer))))))))

(defmethod publish-production-result
    ((session luvcraft-session) (request block-chunk-load-request) payload)
  (let* ((key (block-chunk-load-payload-key payload))
         (world (luvcraft-session-world session)))
    (when (and (eql (gethash key (luvcraft-session-desired-chunks session))
                    (block-chunk-load-request-demand-token request))
               (not (nth-value 1 (apply #'world-chunk-at world key))))
      (destructuring-bind (x y z) key
        (install-world-chunk-storage
         world x y z
         (block-chunk-load-payload-palette payload)
         (block-chunk-load-payload-indices payload))))))

(defun drain-luvcraft-production (session)
  "Publish a bounded number of completed CPU products this frame."
  (loop repeat (luvcraft-session-publication-limit session)
        do (multiple-value-bind (result present-p)
               (receive-production-result-no-hang
                (luvcraft-session-production-system session))
             (unless present-p (return))
             (let* ((request (production-result-request result))
                    (key (production-request-key request))
                    (ticket (gethash key
                                     (luvcraft-session-outstanding-production
                                      session))))
               (when (eql ticket (production-request-ticket request))
                 (remhash key (luvcraft-session-outstanding-production session))
                 (if (production-result-condition result)
                     (push result (luvcraft-session-production-errors session))
                     (publish-production-result
                      session request (production-result-value result))))))))

(defun evict-luvcraft-products (session)
  (let ((evicted nil)
        (desired (luvcraft-session-desired-chunks session))
        (product-tables
          (list (luvcraft-session-chunk-products session)
                (luvcraft-session-staged-chunk-products session))))
    (dolist (products product-tables)
      (setf evicted nil)
      (maphash (lambda (key product)
                 (unless (gethash key desired)
                   (let ((buffer (luvcraft-chunk-product-vertex-buffer product)))
                     (when buffer (destroy buffer)))
                   (push key evicted)))
               products)
      (dolist (key evicted) (remhash key products)))))

(defun refresh-luvcraft-mesh (session)
  "Advance asynchronous loading/meshing without doing either computation here."
  (drain-luvcraft-production session)
  (schedule-luvcraft-chunk-loads session)
  ;; Lighting reconciles before mesh snapshots are captured, so every
  ;; snapshot and dependency stamp observes stable published light.
  (let ((lighting (luvcraft-session-lighting-state session)))
    (when lighting
      (reconcile-lighting lighting)))
  ;; A result captured before this frame's lighting publication may already
  ;; be stale.  Reject it before deciding whether a complete mesh cohort can
  ;; cross the frame boundary.
  (discard-stale-luvcraft-staged-products session)
  (publish-ready-luvcraft-meshes session)
  (schedule-luvcraft-meshes session)
  (setf (luvcraft-session-meshed-world-revision session)
        (block-world-revision (luvcraft-session-world session)))
  (luvcraft-session-products-in-order session))
