;;;; Disposable scene-only batching probe, adapted from the 1125588 probe
;;;; for main at 876e5d3 (accepts its shadow-rectangles binding keyword).
;;;; Load AFTER bench-gpu-fps.lisp (BENCH_LIBRARY=1), and AFTER any harness
;;;; wrapper of GPU-FPS::RUN-CASE. Use a fresh benchmark process, not Sly.
;;;; No ASDF loads, GPU allocation, shader compilation or benchmark at load.
;;;; The original shader's source AST is reparsed, not its generated MSL.
;;;; Shadow bindings/draws, fragment shader, and all existing resources stay put.

(defpackage #:terrain-batch-probe
  (:use #:cl)
  (:local-nicknames (#:r #:luft.render)
                    (#:s #:luft.render.shaders)))
(in-package #:terrain-batch-probe)

;; DEFVAR makes reloading this file avoid wrapping our own wrappers. Reloading
;; the benchmark/harness afterwards is not supported: it replaces RUN-CASE.
(defvar *original-vertex* (symbol-function 's:terrain-vertex-specification))
(defvar *original-frame-binding*
  (symbol-function 'r::renderer-frame-resident-bind-group))
(defvar *original-run-case* (symbol-function 'gpu-fps::run-case))
(defvar *active-case-p* nil)
(defvar *batches* (make-hash-table :test #'eq)) ; original site buffer -> batch
(defvar *patches* (make-hash-table :test #'eq)) ; terrain drawing -> CPU patches
(defvar *buffers* nil)                       ; strong ownership until teardown

(defstruct batch drawing references indices (index-count 0))
(defstruct patch vertices indices)

(defun batched-vertex-specification ()
  "Change only address selection in the current original terrain shader."
  (let* ((specification (funcall *original-vertex*))
         (source (luv.shader:shader-object-source-form specification))
         (options (copy-tree (third source)))
         (body (copy-tree (cdddr source)))
         (site-reads 0)
         (vertex-reads 0))
    (assert (eq (second source) 's:terrain-vertex-specification))
    (assert (and (= (length body) 1) (eq (caar body) 'let*)))
    (assert (not (find 9 (getf options :resources)
                       :key (lambda (resource) (getf (cddr resource) :binding)))))
    ;; Rewrite the BODY only, leaving built-in declarations and all contracts
    ;; intact. Three site reads include appearance and light, not just origin.
    (labels ((rewrite (tree)
               (cond ((eq tree 's::instance-index)
                      (incf site-reads)
                      '(luv.shader:swizzle s::terrain-batch-reference :x))
                     ((eq tree 's::vertex-index)
                      (incf vertex-reads)
                      '(luv.shader:swizzle s::terrain-batch-reference :y))
                     ((consp tree) (mapcar #'rewrite tree))
                     (t tree))))
      (setf body (rewrite body)))
    ;; Fail closed if upstream changes the shader's addressing assumptions.
    (assert (= site-reads 3))
    (assert (= vertex-reads 1))
    (push '(s::terrain-batch-reference
            (luv.shader:buffer-element s::terrain-references s::vertex-index))
          (second (first body)))
    (setf (getf options :resources)
          (append (getf options :resources)
                  '((s::terrain-references :storage-buffer
                     :binding 9 :element :uvec2))))
    (luv.shader:parse-shader-specification (second source) options body)))

(defun index-patch (atlas-indices first count)
  "Compact one patch by first index occurrence; retain global atlas numbers."
  (let ((lookup (make-hash-table :test #'eql))
        (vertices (make-array count :element-type '(unsigned-byte 32)
                                    :fill-pointer 0))
        (indices (make-array count :element-type '(unsigned-byte 32))))
    (dotimes (i count)
      (let* ((global (aref atlas-indices (+ first i)))
             (local (or (gethash global lookup)
                        (setf (gethash global lookup)
                              (vector-push global vertices)))))
        (setf (aref indices i) local)))
    (make-patch :vertices (coerce vertices '(simple-array (unsigned-byte 32) (*)))
                :indices indices)))

(defun drawing-patches (drawing)
  (or (gethash drawing *patches*)
      (multiple-value-bind (words normals indices shadow-words shadow-indices ranges)
          (r::star-patch-tables r::*terrain-bevels-p*)
        (declare (ignore words normals shadow-words shadow-indices))
        ;; Bevel mode must not change between drawing creation and warmup.
        (assert (equalp ranges (r::terrain-star-ranges drawing)))
        (setf (gethash drawing *patches*)
              (map 'vector
                   (lambda (range) (index-patch indices (aref range 0) (aref range 1)))
                   ranges)))))

(defun batch-arrays (patches runs)
  "Return interleaved (site, global-atlas-vertex) words and rebased indices.
Each site's patch keeps exactly the original sharing and triangle order."
  (let ((vertex-count 0) (index-count 0) (old-draws 0) (sites 0))
    (dolist (run runs)
      (destructuring-bind (star first count) run
        (assert (and (typep first '(unsigned-byte 32))
                     (typep count '(unsigned-byte 32))
                     (<= (+ first count) (expt 2 32))))
        (let ((patch (aref patches star)))
          (incf vertex-count (* count (length (patch-vertices patch))))
          (incf index-count (* count (length (patch-indices patch))))
          (incf sites count)
          (when (and (plusp count) (plusp (length (patch-indices patch))))
            (incf old-draws)))))
    (assert (<= vertex-count (expt 2 32)))
    (assert (< index-count (expt 2 32)))
    (let ((references (make-array (* 2 vertex-count) :element-type '(unsigned-byte 32)))
          (indices (make-array index-count :element-type '(unsigned-byte 32)))
          (base 0) (cursor 0))
      (dolist (run runs)
        (destructuring-bind (star first count) run
          (let* ((patch (aref patches star))
                 (vertices (patch-vertices patch))
                 (local-indices (patch-indices patch)))
            (dotimes (instance count)
              (dotimes (v (length vertices))
                (setf (aref references (* 2 (+ base v))) (+ first instance)
                      (aref references (1+ (* 2 (+ base v)))) (aref vertices v)))
              (dotimes (i (length local-indices))
                (setf (aref indices cursor) (+ base (aref local-indices i)))
                (incf cursor))
              (incf base (length vertices))))))
      (assert (= base vertex-count))
      (assert (= cursor index-count))
      (values references indices old-draws sites))))

(defun upload (device label words usage)
  (let ((buffer (luv:create device
                           (luv:make-buffer-descriptor
                            :label label :size (max 16 (* 4 (length words)))
                            :usage (list usage :copy-dst)))))
    ;; Record ownership BEFORE the fallible write, including partial setup.
    (push buffer *buffers*)
    (when (plusp (length words)) (luv:write-buffer buffer words))
    buffer))

(defun prepare-resident (renderer resident)
  (assert *active-case-p* () "Batch probe must run inside GPU-FPS::RUN-CASE.")
  (let* ((sites (r::resident-population-instance-buffer resident))
         (drawing (r::renderer-terrain renderer))
         (existing (gethash sites *batches*)))
    (when existing
      (assert (eq drawing (batch-drawing existing)))
      (return-from prepare-resident existing))
    (multiple-value-bind (references indices old-draws site-count)
        (batch-arrays (drawing-patches drawing) (r::resident-population-star-runs resident))
      (let* ((device (r::renderer-device renderer))
             (batch (make-batch
                     :drawing drawing
                     :references (upload device "probe terrain vertex references" references :storage)
                     :indices (upload device "probe terrain rebased indices" indices :index)
                     :index-count (length indices))))
        (setf (gethash sites *batches*) batch)
        (format t "~&BATCH resident=~D sites=~D scene-draws=~D->~D refs=~D indices=~D extra-bytes=~D (refs=~D index=~D); shadow unchanged~%"
                (hash-table-count *batches*) site-count old-draws
                (if (plusp (length indices)) 1 0) (/ (length references) 2)
                (length indices)
                (+ (max 16 (* 4 (length references))) (max 16 (* 4 (length indices))))
                (max 16 (* 4 (length references))) (max 16 (* 4 (length indices))))
        batch))))

(defmethod r:make-terrain-binding :around
    ((drawing r:terrain-drawing) device sites camera
     &key appearances lights descriptors shadow-map shadow-sampler shadow-p
     &allow-other-keys)
  (if shadow-p
      (call-next-method)
      (let ((batch (gethash sites *batches*)))
        (assert (and batch (eq drawing (batch-drawing batch))))
        (r:make-program-binding
         (r::terrain-scene-program drawing) device
         :sites sites :camera-state camera
         :terrain-vertices (r::terrain-scene-vertices drawing)
         :terrain-normals (r::terrain-normals drawing)
         :terrain-appearances appearances :terrain-lights lights
         :material-descriptors descriptors
         :shadow-map shadow-map :shadow-sampler shadow-sampler
         :terrain-references (batch-references batch)))))

(defmethod r:encode-terrain :around
    ((drawing r:terrain-drawing) pass binding resident &key shadow-p)
  (if shadow-p
      (call-next-method)
      (let ((batch (gethash (r::resident-population-instance-buffer resident) *batches*)))
        (assert (and batch (eq drawing (batch-drawing batch))))
        (when (plusp (batch-index-count batch))
          (luv:set-pipeline pass (r::program-pipeline (r::terrain-scene-program drawing)))
          (luv:set-bind-group pass 0 binding)
          (luv:draw-indexed pass (batch-indices batch) :uint32
                            (batch-index-count batch) 1 0 0 0)))))

(defun cleanup ()
  "Run only after the original case has captured AND torn down its host.
Metal 1125588 supports buffer release after device teardown. Failed destroys
remain strongly held for a manual CLEANUP retry; they are not silently lost."
  (let ((failed nil))
    (dolist (buffer *buffers*)
      (handler-case (luv:destroy buffer)
        (error (condition)
          (push buffer failed)
          (warn "Batch probe buffer cleanup failed: ~A" condition))))
    (setf *buffers* (nreverse failed)))
  (clrhash *batches*)
  (clrhash *patches*)
  (values))

(setf (symbol-function 's:terrain-vertex-specification) #'batched-vertex-specification)
(setf (symbol-function 'r::renderer-frame-resident-bind-group)
      (lambda (renderer frame resident shadow-p)
        ;; Before calling the original cache/binder, not after creating a
        ;; binding. Shadow never needs or allocates the reference buffer.
        (unless shadow-p (prepare-resident renderer resident))
        (funcall *original-frame-binding* renderer frame resident shadow-p)))
(setf (symbol-function 'gpu-fps::run-case)
      (lambda (&rest arguments)
        ;; Global rather than dynamic state: uploads run on the canvas thread.
        ;; Benchmark cases are serial; concurrent/nested cases are unsupported.
        (assert (not *active-case-p*))
        (assert (null *buffers*) () "Retry TERRAIN-BATCH-PROBE::CLEANUP first.")
        (setf *active-case-p* t)
        (unwind-protect (apply *original-run-case* arguments)
          (unwind-protect (cleanup)
            (setf *active-case-p* nil)))))

(in-package #:cl-user)
