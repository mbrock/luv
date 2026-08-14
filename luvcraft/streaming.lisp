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
  ((seed :initarg :seed :reader block-chunk-load-request-seed)
   (demand-token :initarg :demand-token
                 :reader block-chunk-load-request-demand-token)
   (width :initarg :width :reader block-chunk-load-request-width)
   (height :initarg :height :reader block-chunk-load-request-height)
   (depth :initarg :depth :reader block-chunk-load-request-depth)
   (landmarks :initarg :landmarks :initform nil
              :reader block-chunk-load-request-landmarks)
   (edits :initarg :edits :initform nil
          :reader block-chunk-load-request-edits)))

(defmethod perform-production-request ((request block-chunk-load-request))
  "Generate one isolated chunk and transfer only its dense content columns."
  (destructuring-bind (chunk-x chunk-y chunk-z)
      (second (production-request-key request))
    (let* ((source (make-instance 'little-world-source
                                  :seed (block-chunk-load-request-seed request)))
           (world (make-block-world
                   :chunk-width (block-chunk-load-request-width request)
                   :chunk-height (block-chunk-load-request-height request)
                   :chunk-depth (block-chunk-load-request-depth request)
                   :source source)))
      (materialize-block-world-chunk source world chunk-x chunk-y chunk-z)
      (dolist (landmark (block-chunk-load-request-landmarks request))
        (destructuring-bind (block x y z) landmark
          (setf (describe-block-allocatingly world x y z) block)))
      (dolist (edit (block-chunk-load-request-edits request))
        (destructuring-bind (block x y z) edit
          (setf (describe-block-allocatingly world x y z) block)))
      (let ((chunk (world-chunk-at world chunk-x chunk-y chunk-z)))
        (with-block-content-storage (domain palette indices) chunk
          (declare (ignore domain))
          (make-instance 'block-chunk-load-payload
                         :key (list chunk-x chunk-y chunk-z)
                         :palette palette :indices indices))))))

(defparameter *chunk-neighbor-directions*
  '((-1 0 0) (1 0 0) (0 -1 0) (0 1 0) (0 0 -1) (0 0 1)))

(defclass cube-world-chunk-product ()
  ((coordinate :initarg :coordinate
               :reader cube-world-chunk-product-coordinate)
   (dependency-stamp :initarg :dependency-stamp
                     :reader cube-world-chunk-product-dependency-stamp)
   (mesh :initarg :mesh :reader cube-world-chunk-product-mesh)
   (vertex-buffer :initarg :vertex-buffer
                  :reader cube-world-chunk-product-vertex-buffer)))

(defun cancel-cube-world-chunk-production (demo key)
  (dolist (kind '(:load :mesh))
    (let ((production-key (list kind key)))
      ;; Active work cannot be canceled, so retain its ticket until its result
      ;; returns and fails desired-set/incarnation validation.
      (when (cancel-production-request
             (cube-world-demo-production-system demo) production-key)
        (remhash production-key
                 (cube-world-demo-outstanding-production demo))))))

(defun cube-world-player-chunk-center (world player)
  (if player
      (let ((shape (voxel-space-chunk-shape (block-world-space world))))
        (list (floor (player-x player) (chunk-shape-width shape))
              (floor (player-z player) (chunk-shape-depth shape))))
      '(0 0)))

(defun maintain-generated-cube-world-residency
    (demo world player radius)
  (let ((center (cube-world-player-chunk-center world player)))
    (unless (equal center (cube-world-demo-residency-center demo))
      (let ((desired (cube-world-demo-desired-chunks demo))
            (next-desired (make-hash-table :test #'equal)))
        (loop for chunk-x from (- (first center) radius)
                to (+ (first center) radius) do
          (loop for chunk-z from (- (second center) radius)
                  to (+ (second center) radius)
                for key = (chunk-key chunk-x 0 chunk-z)
                do (setf (gethash key next-desired)
                         (or (gethash key desired)
                             (incf (cube-world-demo-next-residency-demand
                                    demo))))))
        (maphash
         (lambda (old-key token)
           (declare (ignore token))
           (unless (gethash old-key next-desired)
             (cancel-cube-world-chunk-production demo old-key)))
         desired)
        (clrhash desired)
        (maphash (lambda (key token)
                   (setf (gethash key desired) token))
                 next-desired)
        (setf (cube-world-demo-residency-center demo) center)
        ;; Eviction is an owner-side publication.  Pending work is either
        ;; canceled before it starts or allowed to finish and fail its
        ;; desired-set/incarnation validation harmlessly.
        (dolist (chunk (resident-world-chunks world))
          (let ((key (block-chunk-key chunk)))
            (unless (gethash key desired)
              (destructuring-bind (x y z) key
                (remove-world-chunk world x y z))
              (cancel-cube-world-chunk-production demo key))))))))

(defun maintain-static-cube-world-residency (demo world player)
  "Treat a caller-owned resident set as desired without loading or eviction."
  (let ((desired (cube-world-demo-desired-chunks demo))
        (resident (make-hash-table :test #'equal)))
    (dolist (chunk (resident-world-chunks world))
      (let ((key (block-chunk-key chunk)))
        (setf (gethash key resident) t)
        (unless (gethash key desired)
          (setf (gethash key desired)
                (incf (cube-world-demo-next-residency-demand demo))))))
    (let ((departed nil))
      (maphash (lambda (key token)
                 (declare (ignore token))
                 (unless (gethash key resident)
                   (push key departed)))
               desired)
      (dolist (key departed)
        (remhash key desired)
        (cancel-cube-world-chunk-production demo key)))
    (setf (cube-world-demo-residency-center demo)
          (cube-world-player-chunk-center world player))))

(defun maintain-cube-world-residency (demo)
  "Reconcile desired residency without generating chunks on the frame thread."
  (let* ((world (cube-world-demo-world demo))
         (source (block-world-source world))
         (player (cube-world-demo-player demo))
         (radius (cube-world-demo-residency-radius demo)))
    (if (and player radius (typep source 'little-world-source))
        (maintain-generated-cube-world-residency
         demo world player radius)
        ;; A caller may supply an already resident world whose source has no
        ;; asynchronous generator.  Those chunks still need immutable mesh
        ;; production; they simply are not loaded or evicted by this demo.
        (maintain-static-cube-world-residency demo world player))))

(defun chunk-mesh-dependency-stamp (world chunk)
  "Describe exactly which resident block data CHUNK's exposed mesh observes."
  (let* ((coordinate
           (chunk-domain-coordinate (block-chunk-domain chunk)))
         (x (chunk-coordinate-x coordinate))
         (y (chunk-coordinate-y coordinate))
         (z (chunk-coordinate-z coordinate)))
    (cons
     (list (block-chunk-key chunk)
           (block-chunk-incarnation chunk)
           (block-chunk-revision chunk))
     (loop for (dx dy dz) in *chunk-neighbor-directions*
           collect
           (multiple-value-bind (neighbor present-p)
               (world-chunk-at world (+ x dx) (+ y dy) (+ z dz))
             (if present-p
                 ;; Only the neighbor boundary facing this chunk contributes.
                 (list (block-chunk-key neighbor)
                       (block-chunk-incarnation neighbor)
                       (block-chunk-boundary-revision
                        neighbor (- dx) (- dy) (- dz)))
                 '(nil)))))))

(defun cube-world-demo-products-in-order (demo)
  (let ((products (cube-world-demo-chunk-products demo)))
    (loop for product being the hash-values of products
          collect product into result
          finally
             (return
               (sort result
                     (lambda (left right)
                       (let ((a (cube-world-chunk-product-coordinate left))
                             (b (cube-world-chunk-product-coordinate right)))
                         (or (< (chunk-coordinate-x a) (chunk-coordinate-x b))
                             (and (= (chunk-coordinate-x a)
                                     (chunk-coordinate-x b))
                                  (or (< (chunk-coordinate-y a)
                                         (chunk-coordinate-y b))
                                      (and (= (chunk-coordinate-y a)
                                              (chunk-coordinate-y b))
                                           (< (chunk-coordinate-z a)
                                              (chunk-coordinate-z b)))))))))))))

(defun cube-world-demo-mesh (demo)
  "Return a combined, inspectable snapshot of DEMO's chunk meshes."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (vertex-count 0)
        (face-count 0))
    (dolist (product (cube-world-demo-products-in-order demo))
      (let ((mesh (cube-world-chunk-product-mesh product)))
        (loop for component across (block-mesh-vertices mesh)
              do (vector-push-extend component vertices))
        (incf vertex-count (block-mesh-vertex-count mesh))
        (incf face-count (block-mesh-face-count mesh))))
    (make-instance 'block-mesh :vertices vertices
                               :vertex-count vertex-count
                               :face-count face-count)))

(defun destroy-cube-world-chunk-products (demo)
  (maphash
   (lambda (key product)
     (declare (ignore key))
     (destroy (cube-world-chunk-product-vertex-buffer product)))
   (cube-world-demo-chunk-products demo))
  (clrhash (cube-world-demo-chunk-products demo))
  (values))

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

(defun schedule-cube-world-chunk-loads (demo)
  (let* ((world (cube-world-demo-world demo))
         (source (block-world-source world))
         (shape (voxel-space-chunk-shape (block-world-space world)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (center (cube-world-demo-residency-center demo))
         (outstanding (cube-world-demo-outstanding-production demo))
         (candidates nil))
    (maphash
     (lambda (key demand-token)
       (let ((production-key (list :load key)))
         (unless (or (nth-value 1 (apply #'world-chunk-at world key))
                     (gethash production-key outstanding))
           (push (list key demand-token production-key) candidates))))
     (cube-world-demo-desired-chunks demo))
    (setf candidates
          (sort candidates
                (lambda (left right)
                  (chunk-key-nearer-p (first left) (first right) center))))
    (loop repeat (cube-world-demo-load-schedule-limit demo)
          for candidate in candidates
          do (destructuring-bind (key demand-token production-key) candidate
           (let ((captured-edits nil))
             (when (typep source 'little-world-source)
               (maphash
                (lambda (coordinate block)
                  (destructuring-bind (x y z) coordinate
                    (when (and (= (floor x width) (first key))
                               (= (floor y height) (second key))
                               (= (floor z depth) (third key)))
                      (push (list block x y z) captured-edits))))
                (block-edit-overlay-entries (little-world-source-edits source))))
             (let ((request
                     (make-instance
                      'block-chunk-load-request
                      :key production-key
                      :priority (chunk-key-distance-squared key center)
                      :seed (little-world-source-seed source)
                      :demand-token demand-token
                      :width width :height height :depth depth
                      :landmarks
                      (little-world-landmarks-for-chunk source world key)
                      :edits captured-edits)))
               (setf (gethash production-key outstanding)
                     (schedule-production-request
                      (cube-world-demo-production-system demo) request))))))))

(defun schedule-cube-world-meshes (demo)
  "Capture at most the configured number of immutable mesh inputs this frame."
  (let* ((world (cube-world-demo-world demo))
         (products (cube-world-demo-chunk-products demo))
         (outstanding (cube-world-demo-outstanding-production demo))
         (center (cube-world-demo-residency-center demo))
         (candidates nil))
    (dolist (chunk (resident-world-chunks world))
      (let* ((key (block-chunk-key chunk))
             (production-key (list :mesh key))
             (stamp (chunk-mesh-dependency-stamp world chunk))
             (old (gethash key products)))
        (when (and (gethash key (cube-world-demo-desired-chunks demo))
                   (not (gethash production-key outstanding))
                   (not (and old
                             (equal stamp
                                    (cube-world-chunk-product-dependency-stamp
                                     old)))))
          (push (list (chunk-key-distance-squared key center)
                      chunk key production-key stamp)
                candidates))))
    (setf candidates (sort candidates #'< :key #'first))
    (loop repeat (cube-world-demo-mesh-capture-limit demo)
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
                          (cube-world-demo-mesher demo))
                         :snapshot snapshot)))
                 (setf (gethash production-key outstanding)
                       (schedule-production-request
                        (cube-world-demo-production-system demo) request)))))))

(defun publish-cube-world-mesh (demo request mesh)
  "Validate and install one worker result on the render/GPU owning thread."
  (let* ((snapshot (block-mesh-production-request-snapshot request))
         (key (block-mesh-snapshot-key snapshot))
         (world (cube-world-demo-world demo))
         (chunk (apply #'world-chunk-at world key)))
    (when (and chunk
               (gethash key (cube-world-demo-desired-chunks demo))
               (equal (block-mesh-snapshot-dependency-stamp snapshot)
                      (chunk-mesh-dependency-stamp world chunk)))
      (let ((buffer nil) (completed-p nil)
            (old (gethash key (cube-world-demo-chunk-products demo))))
        (unwind-protect
             (progn
               (setf buffer
                     (create
                      (cube-world-demo-device demo)
                      (make-buffer-descriptor
                       :label (format nil "block chunk ~{~D~^,~} async mesh" key)
                       :size (max 4 (* 4 (length (block-mesh-vertices mesh))))
                       :usage '(:vertex))))
               (write-buffer buffer (block-mesh-vertices mesh))
               (setf (gethash key (cube-world-demo-chunk-products demo))
                     (make-instance
                      'cube-world-chunk-product
                      :coordinate
                      (chunk-domain-coordinate (block-chunk-domain chunk))
                      :dependency-stamp
                      (block-mesh-snapshot-dependency-stamp snapshot)
                      :mesh mesh :vertex-buffer buffer)
                     completed-p t)
               (when old
                 (destroy (cube-world-chunk-product-vertex-buffer old))))
          (unless completed-p
            (when buffer (destroy buffer))))))))

(defun publish-cube-world-load (demo request payload)
  (let* ((key (block-chunk-load-payload-key payload))
         (world (cube-world-demo-world demo)))
    (when (and (eql (gethash key (cube-world-demo-desired-chunks demo))
                    (block-chunk-load-request-demand-token request))
               (not (nth-value 1 (apply #'world-chunk-at world key))))
      (destructuring-bind (x y z) key
        (install-world-chunk-storage
         world x y z
         (block-chunk-load-payload-palette payload)
         (block-chunk-load-payload-indices payload))))))

(defun drain-cube-world-production (demo)
  "Publish a bounded number of completed CPU products this frame."
  (loop repeat (cube-world-demo-publication-limit demo)
        do (multiple-value-bind (result present-p)
               (receive-production-result-no-hang
                (cube-world-demo-production-system demo))
             (unless present-p (return))
             (let* ((request (production-result-request result))
                    (key (production-request-key request))
                    (ticket (gethash key
                                     (cube-world-demo-outstanding-production
                                      demo))))
               (when (eql ticket (production-request-ticket request))
                 (remhash key (cube-world-demo-outstanding-production demo))
                 (cond
                   ((production-result-condition result)
                    (push result (cube-world-demo-production-errors demo)))
                   ((typep request 'block-chunk-load-request)
                    (publish-cube-world-load
                     demo request (production-result-value result)))
                   ((typep request 'block-mesh-production-request)
                    (publish-cube-world-mesh
                     demo request (production-result-value result)))))))))

(defun evict-cube-world-products (demo)
  (let ((evicted nil)
        (desired (cube-world-demo-desired-chunks demo))
        (products (cube-world-demo-chunk-products demo)))
    (maphash (lambda (key product)
               (unless (gethash key desired)
                 (destroy (cube-world-chunk-product-vertex-buffer product))
                 (push key evicted)))
             products)
    (dolist (key evicted) (remhash key products))))

(defun refresh-cube-world-mesh (demo)
  "Advance asynchronous loading/meshing without doing either computation here."
  (drain-cube-world-production demo)
  (schedule-cube-world-chunk-loads demo)
  (schedule-cube-world-meshes demo)
  (setf (cube-world-demo-meshed-world-revision demo)
        (block-world-revision (cube-world-demo-world demo)))
  (cube-world-demo-products-in-order demo))
