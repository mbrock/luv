;;; Presentation-slot-local application resources.

(in-package #:luv)

(defclass canvas-frame-resource-cache ()
  ((entries
    :initarg :entries
    :initform (make-hash-table :test #'eql)
    :reader canvas-frame-resource-cache-entries)
   (closed-p :initform nil :accessor canvas-frame-resource-cache-closed-p))
  (:documentation
   "Application values keyed by the presentation slots of a canvas context.

Acquiring a surface texture proves that its presentation slot is no longer in
use by an earlier GPU submission.  A value obtained through CANVAS-FRAME-RESOURCE
may therefore contain mapped buffers which the application overwrites before
encoding the newly acquired frame.  The cache owns its values, but callers
supply their construction and release operations because a value need not be a
single GPU handle.  #5OROXO"))

(defun make-canvas-frame-resource-cache (&key entries)
  "Make an open presentation-slot resource cache.

ENTRIES may supply an existing EQL hash table when migrating an application
which already owns a per-drawable table."
  (when entries
    (check-type entries hash-table)
    (unless (member (hash-table-test entries) (list 'eql #'eql) :test #'eq)
      (error "A canvas frame resource cache requires an EQL hash table.")))
  (make-instance 'canvas-frame-resource-cache
                 :entries (or entries (make-hash-table :test #'eql))))

(defun canvas-frame-resource-count (cache)
  "Return the number of presentation slots currently retained by CACHE."
  (check-type cache canvas-frame-resource-cache)
  (hash-table-count (canvas-frame-resource-cache-entries cache)))

(defun map-canvas-frame-resources (function cache)
  "Call FUNCTION with each value and stable key currently retained by CACHE."
  (check-type function function)
  (check-type cache canvas-frame-resource-cache)
  (maphash (lambda (key value) (funcall function value key))
           (canvas-frame-resource-cache-entries cache))
  cache)

(defun canvas-frame-resource (cache context surface-texture constructor)
  "Return CACHE's value for SURFACE-TEXTURE, constructing it on first use.

CONSTRUCTOR receives the stable frame key and SURFACE-TEXTURE.  Its result is
published only after it returns normally, so a failed construction leaves the
cache unchanged.  The application must call this only after acquiring the
surface from CONTEXT; that acquisition is the safe-reuse proof for mutable
contents retained in the returned value."
  (check-type cache canvas-frame-resource-cache)
  (check-type context canvas-context)
  (check-type constructor function)
  (when (canvas-frame-resource-cache-closed-p cache)
    (error "Cannot acquire a resource from a closed canvas frame cache."))
  (let* ((key (canvas-frame-resource-key context surface-texture))
         (entries (canvas-frame-resource-cache-entries cache)))
    (multiple-value-bind (value present-p) (gethash key entries)
      (if present-p
          (values value nil key)
          (let ((candidate (funcall constructor key surface-texture)))
            (setf (gethash key entries) candidate)
            (values candidate t key))))))

(defun evict-canvas-frame-resource-key (cache key releaser)
  "Release and forget CACHE's value for KEY, retaining it if release fails."
  (check-type cache canvas-frame-resource-cache)
  (check-type releaser function)
  (let ((entries (canvas-frame-resource-cache-entries cache)))
    (multiple-value-bind (value present-p) (gethash key entries)
      (when present-p
        (funcall releaser value)
        (remhash key entries)
        (values value t)))))

(defun evict-canvas-frame-resource
    (cache context surface-texture releaser)
  "Release and forget CACHE's value for SURFACE-TEXTURE."
  (evict-canvas-frame-resource-key
   cache (canvas-frame-resource-key context surface-texture) releaser))

(defun clear-canvas-frame-resource-cache (cache releaser)
  "Release every CACHE value, attempting all entries and retaining failures."
  (check-type cache canvas-frame-resource-cache)
  (check-type releaser function)
  (let ((entries (canvas-frame-resource-cache-entries cache)))
    (with-release-report
      ;; Snapshot keys because successful release removes the table entry.
      (dolist (key (loop for key being the hash-keys of entries collect key))
        (releasing (list :canvas-frame-resource key)
          (evict-canvas-frame-resource-key cache key releaser)))))
  cache)

(defun destroy-canvas-frame-resource-cache (cache releaser)
  "Close CACHE to new acquisition and release all values.

Failed values remain owned by the closed cache, so calling this function again
retries them."
  (check-type cache canvas-frame-resource-cache)
  (setf (canvas-frame-resource-cache-closed-p cache) t)
  (clear-canvas-frame-resource-cache cache releaser))
