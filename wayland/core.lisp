;;;; Core protocol: compositor, surfaces, regions, subsurfaces, output, and
;;;; the clipboard device foot insists on.

(in-package #:luv.wayland)

;;; Destructor requests need no method: the dispatcher destroys the resource
;;; after HANDLE-REQUEST returns.  These keep them out of the unhandled log.

(define-request (resource resource :destroy) ())
(define-request (resource resource :release) ())

;;; Snapshots.  A committed shm buffer is copied into a Lisp array of 32-bit
;;; words, in the layout LUV:WRITE-TEXTURE takes, and the client's buffer is
;;; released at once.  The host reads snapshots on its own thread, so the
;;; arrays are recycled through a small handoff instead of being allocated
;;; per commit:
;;;
;;;   :FRESH --host claims--> :READING --done--> :SPENT --recycled--> :RECYCLED
;;;      `--server publishes a newer one first--> :SPENT
;;;
;;; Each step is a compare-and-swap.  Only the host's claim may read pixels,
;;; and exactly one thread wins :SPENT -> :RECYCLED and returns the array to
;;; its surface's pool, so an array is never refilled while it is read.

(defstruct (snapshot-pool (:constructor make-snapshot-pool ()))
  (free '())
  (allocated 0 :type fixnum))

(defstruct (snapshot (:constructor %make-snapshot (width height pixels pool)))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (pixels nil :type (simple-array (unsigned-byte 32) (* *)))
  (format :argb8888)
  (serial 0)
  (state :fresh)
  (pool nil))

(defmethod print-object ((snapshot snapshot) stream)
  (print-unreadable-object (snapshot stream :type t)
    (format stream "~Dx~D ~(~A~) #~D ~(~A~)" (snapshot-width snapshot)
            (snapshot-height snapshot) (snapshot-format snapshot)
            (snapshot-serial snapshot) (snapshot-state snapshot))))

(defun take-snapshot (pool width height)
  "A recycled WIDTH by HEIGHT snapshot from POOL, or a new one.  Arrays of
another size are dropped.  Runs on the server thread."
  (loop
    (let ((snapshot (sb-ext:atomic-pop (snapshot-pool-free pool))))
      (cond
        ((null snapshot)
         (incf (snapshot-pool-allocated pool))
         (return (%make-snapshot width height
                                 (make-array (list height width)
                                             :element-type '(unsigned-byte 32))
                                 pool)))
        ((and (= width (snapshot-width snapshot))
              (= height (snapshot-height snapshot)))
         (return snapshot))))))

(defun recycle-snapshot (snapshot)
  "Return a spent SNAPSHOT to its pool, unless the other thread already did."
  (when (eq :spent (sb-ext:compare-and-swap (snapshot-state snapshot) :spent :recycled))
    (sb-ext:atomic-push snapshot (snapshot-pool-free (snapshot-pool snapshot)))))

(defun retire-snapshot (snapshot)
  "SNAPSHOT is no longer current.  If the host never claimed it, it is spent
now; if the host is reading it, the host recycles it when done."
  (sb-ext:compare-and-swap (snapshot-state snapshot) :fresh :spent)
  (recycle-snapshot snapshot))

(defun publish-snapshot (surface snapshot)
  (let ((old (surface-snapshot surface)))
    (sb-thread:barrier (:write))
    (setf (surface-snapshot surface) snapshot)
    (when old (retire-snapshot old))))

(defun call-with-surface-snapshot (surface function)
  "Call FUNCTION with SURFACE's current snapshot if this thread can claim it,
and return its value; otherwise return NIL.  A claim fails when the snapshot
was already read or a newer one replaced it.  FUNCTION may read the pixels
only for its own extent.  Callable from any thread."
  (let ((snapshot (surface-snapshot surface)))
    (when (and snapshot
               (eq :fresh (sb-ext:compare-and-swap (snapshot-state snapshot)
                                                   :fresh :reading)))
      (unwind-protect (funcall function snapshot)
        (sb-ext:compare-and-swap (snapshot-state snapshot) :reading :spent)
        (unless (eq snapshot (surface-snapshot surface))
          (recycle-snapshot snapshot))))))

(defun shm-format-keyword (code)
  (case code (0 :argb8888) (1 :xrgb8888) (t nil)))

(defvar *snapshot-serial* 0)

(defun copy-shm-buffer (buffer-pointer pool)
  "A snapshot from POOL holding the shm buffer behind BUFFER-POINTER, or NIL
if it is not one we can read."
  (let ((shm (%shm-buffer-get buffer-pointer)))
    (unless (cffi:null-pointer-p shm)
      (let* ((width (%shm-buffer-get-width shm))
             (height (%shm-buffer-get-height shm))
             (stride (%shm-buffer-get-stride shm))
             (format (shm-format-keyword (%shm-buffer-get-format shm))))
        (unless format
          (server-log "unsupported shm format ~D" (%shm-buffer-get-format shm))
          (return-from copy-shm-buffer nil))
        (let* ((snapshot (take-snapshot pool width height))
               (pixels (snapshot-pixels snapshot)))
          (%shm-buffer-begin-access shm)
          (unwind-protect
               (let ((source (%shm-buffer-get-data shm))
                     (row-bytes (* 4 width)))
                 (sb-sys:with-pinned-objects (pixels)
                   (let ((destination (sb-sys:vector-sap
                                       (sb-ext:array-storage-vector pixels))))
                     (dotimes (row height)
                       (%memcpy (cffi:inc-pointer destination (* row row-bytes))
                                (cffi:inc-pointer source (* row stride))
                                row-bytes)))))
            (%shm-buffer-end-access shm))
          (setf (snapshot-format snapshot) format
                (snapshot-serial snapshot) (incf *snapshot-serial*)
                (snapshot-state snapshot) :fresh)
          snapshot)))))

(defun release-buffer (buffer)
  "wl_buffer.release, for a buffer implemented by libwayland or by Lisp."
  (%resource-post-event-array (resource-pointer buffer) 0 (cffi:null-pointer)))

;;; wl_compositor.

(defclass compositor (resource) ())

(define-globals compositor-global ()
  (add-global "wl_compositor" 6
              (lambda (client version id)
                (make-resource 'compositor client "wl_compositor" version id))))

(define-request (compositor compositor :create-surface) (id)
  (let ((surface (make-child-resource 'surface compositor "wl_surface" id)))
    (push surface (server-surfaces *server*))
    surface))

(define-request (compositor compositor :create-region) (id)
  (make-child-resource 'region compositor "wl_region" id))

;;; wl_region.  Opaque and input regions are accepted and, for now, ignored.

(defclass region (resource)
  ((rectangles :initform '() :accessor region-rectangles)))

(define-request (region region :add) (x y width height)
  (push (list :add x y width height) (region-rectangles region)))

(define-request (region region :subtract) (x y width height)
  (push (list :subtract x y width height) (region-rectangles region)))

;;; wl_callback.

(defclass frame-callback (resource) ())

(defun fire-callback (callback time-ms)
  (when (resource-live-p callback)
    (post-event callback :done (ldb (byte 32 0) time-ms))
    (destroy-resource callback)))

;;; wl_surface.  Requests change pending state; commit applies it.

(defclass surface (resource)
  ((pending-buffer :initform :unchanged :accessor surface-pending-buffer)
   (pending-buffer-listener :initform nil :accessor surface-pending-buffer-listener)
   (pending-callbacks :initform '() :accessor surface-pending-callbacks)
   (pending-scale :initform nil :accessor surface-pending-scale)
   (callbacks :initform '() :accessor surface-callbacks
              :documentation "Frame callbacks committed and waiting for a frame.")
   (scale :initform 1 :accessor surface-scale)
   (snapshot :initform nil :accessor surface-snapshot
             :documentation "The newest committed contents.  Read its pixels only
through CALL-WITH-SURFACE-SNAPSHOT.")
   (snapshot-pool :initform (make-snapshot-pool) :reader surface-snapshot-pool)
   (role :initform nil :accessor surface-role
         :documentation "The xdg_surface, subsurface, or other role object.")))

(defmethod resource-destroyed ((surface surface))
  (forget-pending-buffer surface)
  (setf (server-surfaces *server*) (remove surface (server-surfaces *server*))))

(defun forget-pending-buffer (surface)
  (let ((listener (surface-pending-buffer-listener surface)))
    (when listener
      ;; Unlink from the buffer's destroy signal before freeing.
      (let ((link (cffi:foreign-slot-pointer listener '(:struct wl-listener) 'link)))
        (cffi:foreign-funcall "wl_list_remove" :pointer link :void))
      (free-listener listener)
      (setf (surface-pending-buffer-listener surface) nil))))

(define-request (surface surface :attach) (buffer x y)
  (declare (ignore x y))
  (forget-pending-buffer surface)
  (setf (surface-pending-buffer surface) buffer)
  (when buffer
    ;; A client may destroy an attached buffer before committing it.
    (let ((listener (make-listener
                     (lambda (data)
                       (declare (ignore data))
                       (when (eq (surface-pending-buffer surface) buffer)
                         (setf (surface-pending-buffer surface) nil))
                       (free-listener (surface-pending-buffer-listener surface))
                       (setf (surface-pending-buffer-listener surface) nil)))))
      (setf (surface-pending-buffer-listener surface) listener)
      (%resource-add-destroy-listener (resource-pointer buffer) listener))))

(define-request (surface surface :damage) (x y width height)
  (declare (ignore x y width height)))

(define-request (surface surface :damage-buffer) (x y width height)
  (declare (ignore x y width height)))

(define-request (surface surface :frame) (id)
  (push (make-child-resource 'frame-callback surface "wl_callback" id)
        (surface-pending-callbacks surface)))

(define-request (surface surface :set-opaque-region) (region)
  (declare (ignore region)))

(define-request (surface surface :set-input-region) (region)
  (declare (ignore region)))

(define-request (surface surface :set-buffer-scale) (scale)
  (setf (surface-pending-scale surface) scale))

(define-request (surface surface :set-buffer-transform) (transform)
  (unless (zerop transform)
    (server-log "ignoring buffer transform ~D on ~A" transform surface)))

(define-request (surface surface :offset) (x y)
  (declare (ignore x y)))

(defgeneric surface-committed (role surface)
  (:documentation "Called after SURFACE applied its pending state, with its role.")
  (:method (role surface) (declare (ignore role surface)) nil))

(define-request (surface surface :commit) ()
  (let ((buffer (surface-pending-buffer surface)))
    (unless (eq buffer :unchanged)
      (forget-pending-buffer surface)
      (setf (surface-pending-buffer surface) :unchanged)
      (cond
        ((null buffer)
         (publish-snapshot surface nil))
        (t
         (let ((snapshot (copy-shm-buffer (resource-pointer buffer)
                                          (surface-snapshot-pool surface))))
           (if snapshot
               (publish-snapshot surface snapshot)
               (server-log "~A committed a buffer we cannot read: ~A" surface buffer))
           (release-buffer buffer))))))
  (when (surface-pending-scale surface)
    (setf (surface-scale surface) (shiftf (surface-pending-scale surface) nil)))
  (setf (surface-callbacks surface)
        (append (surface-callbacks surface)
                (shiftf (surface-pending-callbacks surface) '())))
  (surface-committed (surface-role surface) surface))

(defun send-frame-callbacks (&key (surfaces (server-surfaces *server*))
                                  (time-ms (get-internal-real-time)))
  "Tell SURFACES' committed frame callbacks that now is a good time to draw.
Call on the server thread, after a frame that showed those surfaces."
  (dolist (surface surfaces)
    (dolist (callback (shiftf (surface-callbacks surface) '()))
      (fire-callback callback (floor (* time-ms 1000) internal-time-units-per-second)))))

;;; wl_subcompositor.  Subsurfaces are accepted so clients can run; Luvland
;;; does not yet draw them.

(defclass subcompositor (resource) ())
(defclass subsurface (resource)
  ((surface :initarg :surface :reader subsurface-surface)
   (parent :initarg :parent :reader subsurface-parent)
   (position :initform '(0 0) :accessor subsurface-position)))

(define-globals subcompositor-global ()
  (add-global "wl_subcompositor" 1
              (lambda (client version id)
                (make-resource 'subcompositor client "wl_subcompositor" version id))))

(define-request (subcompositor subcompositor :get-subsurface) (id surface parent)
  (let ((subsurface (make-child-resource 'subsurface subcompositor "wl_subsurface" id
                                         :surface surface :parent parent)))
    (setf (surface-role surface) subsurface)))

(define-request (subsurface subsurface :set-position) (x y)
  (setf (subsurface-position subsurface) (list x y)))

(define-request (subsurface subsurface :place-above) (sibling)
  (declare (ignore sibling)))
(define-request (subsurface subsurface :place-below) (sibling)
  (declare (ignore sibling)))
(define-request (subsurface subsurface :set-sync) ())
(define-request (subsurface subsurface :set-desync) ())

;;; wl_output.  One output, as large as the host says.

(defclass output (resource) ())

(define-globals output-global ()
  (add-global "wl_output" 4
              (lambda (client version id)
                (let ((output (make-resource 'output client "wl_output" version id))
                      (size (server-output-size *server*)))
                  (post-event output :geometry 0 0 600 340 0 "luv" "Luvland" 0)
                  (post-event output :mode 3 (first size) (second size) 60000)
                  (post-event output :scale 1)
                  (post-event output :name "LUVLAND-1")
                  (post-event output :description "A Luvland world")
                  (post-event output :done)))))

;;; wl_data_device_manager.  The clipboard is not yet shared, but clients
;;; such as foot refuse to start without the global.

(defclass data-device-manager (resource) ())
(defclass data-device (resource) ())
(defclass data-source (resource) ())

(define-globals data-device-global ()
  (add-global "wl_data_device_manager" 3
              (lambda (client version id)
                (make-resource 'data-device-manager client
                               "wl_data_device_manager" version id))))

(define-request (manager data-device-manager :create-data-source) (id)
  (make-child-resource 'data-source manager "wl_data_source" id))

(define-request (manager data-device-manager :get-data-device) (id seat)
  (declare (ignore seat))
  (make-child-resource 'data-device manager "wl_data_device" id))

(define-request (source data-source :offer) (mime-type)
  (declare (ignore mime-type)))
(define-request (source data-source :set-actions) (actions)
  (declare (ignore actions)))
(define-request (device data-device :set-selection) (source serial)
  (declare (ignore source serial)))
