;;;; linux-dmabuf: buffers that live in GPU memory.
;;;;
;;;; The server never touches the GPU.  The host says which device it renders
;;;; on and which format and modifier pairs it can import (SERVER-DMABUF); the
;;;; server advertises them, collects each buffer's planes, holds a commit until
;;;; its dmabuf polls readable (implicit fences, as niri does), and publishes a
;;;; DMABUF-FRAME.  The host imports the buffer and must hand each frame it
;;;; claimed back with RELEASE-DMABUF-FRAME once its GPU work is done.

(in-package #:luv.wayland)

;;; DRM fourcc codes for the formats a compositor meets first.

(defun fourcc (code)
  (logior (char-code (char code 0))
          (ash (char-code (char code 1)) 8)
          (ash (char-code (char code 2)) 16)
          (ash (char-code (char code 3)) 24)))

(defparameter +drm-format-argb8888+ (fourcc "AR24"))
(defparameter +drm-format-xrgb8888+ (fourcc "XR24"))
(defparameter +drm-format-abgr8888+ (fourcc "AB24"))
(defparameter +drm-format-xbgr8888+ (fourcc "XB24"))

(defconstant +drm-format-mod-invalid+ #x00ffffffffffffff)

(defun make-dev-t (major minor)
  "Linux's dev_t encoding of MAJOR:MINOR, as gnu_dev_makedev computes it."
  (logior (ash (logand major #xfffff000) 32)
          (ash (logand major #x00000fff) 8)
          (ash (logand minor #xffffff00) 12)
          (logand minor #x000000ff)))

(defun uint64-octets (value)
  (let ((octets (make-array 8 :element-type '(unsigned-byte 8))))
    (dotimes (index 8 octets)
      (setf (aref octets index) (ldb (byte 8 (* 8 index)) value)))))

;;; Buffers.

(defclass dmabuf-buffer (resource)
  ((width :initarg :width :reader dmabuf-width)
   (height :initarg :height :reader dmabuf-height)
   (format :initarg :format :reader dmabuf-format)
   (modifier :initarg :modifier :reader dmabuf-modifier)
   (planes :initarg :planes :reader dmabuf-planes
           :documentation "One (fd offset stride) per plane, in plane order.")
   (host-data :initform nil :accessor dmabuf-host-data
              :documentation "Whatever the host keeps for this buffer, such as
its imported image.  The host owns it and must drop it when the buffer dies.")
   (destroy-hooks :initform '() :accessor dmabuf-destroy-hooks))
  (:documentation "A wl_buffer whose contents are one or more dmabuf planes."))

(defmethod print-object ((buffer dmabuf-buffer) stream)
  (print-unreadable-object (buffer stream :type t)
    (format stream "~Dx~D ~8,'0X/~16,'0X ~D plane~:P~:[ dead~;~]"
            (dmabuf-width buffer) (dmabuf-height buffer) (dmabuf-format buffer)
            (dmabuf-modifier buffer) (length (dmabuf-planes buffer))
            (resource-live-p buffer))))

(defmethod resource-destroyed ((buffer dmabuf-buffer))
  (dolist (hook (dmabuf-destroy-hooks buffer))
    (funcall hook buffer))
  (dolist (plane (dmabuf-planes buffer))
    (%close (first plane))))

(defun on-dmabuf-destroyed (buffer function)
  "Call FUNCTION with BUFFER on the server thread when its client destroys it."
  (push function (dmabuf-destroy-hooks buffer)))

;;; Frames.  A committed dmabuf is published as a frame which moves
;;;
;;;   :FRESH --host claims--> :HELD --host releases--> :RELEASED
;;;      `--server publishes a newer one first--> :RELEASED
;;;
;;; by compare-and-swap, so a buffer the host never sampled is released as
;;; soon as it is superseded and one it did sample only when the host says.

(defstruct (dmabuf-frame (:constructor %make-dmabuf-frame (buffer serial planes)))
  buffer
  serial
  ;; (fd offset stride) per plane, with fds this frame owns: the buffer's
  ;; own may close whenever its client destroys it.
  (planes '())
  (state :fresh))

(defun make-dmabuf-frame (buffer serial)
  (%make-dmabuf-frame buffer serial
                      (loop for (fd offset stride) in (dmabuf-planes buffer)
                            collect (list (duplicate-fd fd) offset stride))))

(defun duplicate-fd (fd)
  (let ((copy (cffi:foreign-funcall "fcntl" :int fd :int 1030 :int 0 :int))) ; F_DUPFD_CLOEXEC
    (when (minusp copy)
      (error "Could not duplicate fd ~D." fd))
    copy))

(defun release-dmabuf-frame (frame)
  "Let FRAME's client reuse its buffer.  Call on the server thread, once."
  (dolist (plane (shiftf (dmabuf-frame-planes frame) '()))
    (%close (first plane)))
  (let ((buffer (dmabuf-frame-buffer frame)))
    (when (resource-live-p buffer)
      (release-buffer buffer))))

(defun claim-dmabuf-frame (surface)
  "SURFACE's current dmabuf frame if this call claimed it, or NIL.  The
claimant must call RELEASE-DMABUF-FRAME on the server thread when the GPU no
longer reads the buffer.  Callable from any thread."
  (let ((frame (surface-dmabuf-frame surface)))
    (when (and frame
               (eq :fresh (sb-ext:compare-and-swap (dmabuf-frame-state frame)
                                                   :fresh :held)))
      frame)))

(defun retire-dmabuf-frame (frame)
  (when (eq :fresh (sb-ext:compare-and-swap (dmabuf-frame-state frame) :fresh :released))
    (release-dmabuf-frame frame)))

(defun publish-dmabuf-frame (surface frame)
  "Make FRAME SURFACE's contents, replacing any shm snapshot or older frame."
  (let ((old (surface-dmabuf-frame surface)))
    (sb-thread:barrier (:write))
    (setf (surface-dmabuf-frame surface) frame)
    (when old (retire-dmabuf-frame old)))
  (when (and frame (surface-snapshot surface))
    (publish-snapshot surface nil)))

(defun commit-dmabuf (surface buffer)
  "Publish BUFFER on SURFACE once its implicit fences have signalled."
  (let ((frame (make-dmabuf-frame buffer (incf *snapshot-serial*))))
    (setf (surface-pending-dmabuf-frame surface) frame)
    (when-readable (first (first (dmabuf-planes buffer)))
                   (lambda ()
                     ;; A later commit may have overtaken this one.
                     (when (and (eq frame (surface-pending-dmabuf-frame surface))
                                (resource-live-p surface)
                                (resource-live-p buffer))
                       (setf (surface-pending-dmabuf-frame surface) nil)
                       (publish-dmabuf-frame surface frame))
                     (unless (eq frame (surface-dmabuf-frame surface))
                       (release-dmabuf-frame frame))))))

;;; zwp_linux_dmabuf_v1.

(defclass dmabuf-manager (resource) ())
(defclass buffer-params (resource)
  ((planes :initform '() :accessor params-planes
           :documentation "(index fd offset stride modifier), newest first.")
   (used-p :initform nil :accessor params-used-p)))
(defclass dmabuf-feedback (resource) ())

(define-globals dmabuf-global ()
  (when (server-dmabuf *server*)
    (add-global "zwp_linux_dmabuf_v1" 4
                (lambda (client version id)
                  (make-resource 'dmabuf-manager client "zwp_linux_dmabuf_v1"
                                 version id)))))

(define-request (manager dmabuf-manager :create-params) (id)
  (make-child-resource 'buffer-params manager "zwp_linux_buffer_params_v1" id))

(defun format-table-fd ()
  "A memfd holding the host's (format, padding, modifier) table, and its size."
  (let* ((formats (getf (server-dmabuf *server*) :formats))
         (octets (make-array (* 16 (length formats)) :element-type '(unsigned-byte 8)))
         (fd (%memfd-create "luv-dmabuf-formats" +mfd-cloexec+)))
    (when (minusp fd)
      (error "memfd_create failed for the dmabuf format table."))
    (loop for (format . modifier) in formats
          for offset from 0 by 16
          do (replace octets (subseq (uint32-array (list format 0)) 0 8) :start1 offset)
             (replace octets (uint64-octets modifier) :start1 (+ offset 8)))
    (sb-sys:with-pinned-objects (octets)
      (%write fd (sb-sys:vector-sap octets) (length octets)))
    (values fd (length octets))))

(defun send-feedback (feedback)
  (let* ((description (server-dmabuf *server*))
         (device (uint64-octets (getf description :main-device)))
         (count (length (getf description :formats))))
    (multiple-value-bind (fd size) (format-table-fd)
      (unwind-protect (post-event feedback :format-table fd size)
        (%close fd)))
    (post-event feedback :main-device device)
    ;; One tranche: everything the host can sample, on its own device.
    (post-event feedback :tranche-target-device device)
    (post-event feedback :tranche-formats
                (let ((octets (make-array (* 2 count) :element-type '(unsigned-byte 8))))
                  (dotimes (index count octets)
                    (setf (aref octets (* 2 index)) (ldb (byte 8 0) index)
                          (aref octets (1+ (* 2 index))) (ldb (byte 8 8) index)))))
    (post-event feedback :tranche-flags 0)
    (post-event feedback :tranche-done)
    (post-event feedback :done)))

(define-request (manager dmabuf-manager :get-default-feedback) (id)
  (send-feedback (make-child-resource 'dmabuf-feedback manager
                                      "zwp_linux_dmabuf_feedback_v1" id)))

(define-request (manager dmabuf-manager :get-surface-feedback) (id surface)
  (declare (ignore surface))
  (send-feedback (make-child-resource 'dmabuf-feedback manager
                                      "zwp_linux_dmabuf_feedback_v1" id)))

;;; zwp_linux_buffer_params_v1.

(defun params-error (params code message)
  (post-error params (enum-value (resource-interface params) "error" code) message))

(defmethod resource-destroyed ((params buffer-params))
  ;; Planes never handed to a buffer are still ours to close.
  (unless (params-used-p params)
    (dolist (plane (params-planes params))
      (%close (second plane)))))

(define-request (params buffer-params :add) (fd index offset stride modifier-hi modifier-lo)
  (cond
    ((params-used-p params)
     (%close fd)
     (params-error params :already-used "These params already made a buffer."))
    ((find index (params-planes params) :key #'first)
     (%close fd)
     (params-error params :plane-set (format nil "Plane ~D is already set." index)))
    ((> index 3)
     (%close fd)
     (params-error params :plane-idx (format nil "There is no plane ~D." index)))
    (t
     (push (list index fd offset stride (logior (ash modifier-hi 32) modifier-lo))
           (params-planes params)))))

(defun validate-params (params width height format flags)
  "The buffer's planes as (fd offset stride) and its modifier, or NIL after
posting the protocol error that explains why not."
  (let* ((planes (sort (copy-list (params-planes params)) #'< :key #'first))
         (modifier (fifth (first planes)))
         (formats (getf (server-dmabuf *server*) :formats)))
    (cond
      ((params-used-p params)
       (params-error params :already-used "These params already made a buffer.") nil)
      ((or (null planes)
           (/= (length planes) (1+ (first (car (last planes))))))
       (params-error params :incomplete "The planes are missing or not contiguous.") nil)
      ((notevery (lambda (plane) (= (fifth plane) modifier)) planes)
       (params-error params :invalid-format "Planes disagree about the modifier.") nil)
      ((not (find (cons format modifier) formats :test #'equal))
       (params-error params :invalid-format
                     (format nil "Format ~8,'0X with modifier ~16,'0X is not offered."
                             format modifier))
       nil)
      ((or (<= width 0) (<= height 0))
       (params-error params :invalid-dimensions "Width and height must be positive.") nil)
      ((/= flags 0)
       (params-error params :invalid-format "Inverted or interlaced buffers are not supported.")
       nil)
      (t
       (setf (params-used-p params) t)
       (values (mapcar (lambda (plane) (subseq plane 1 4)) planes) modifier)))))

(defun make-dmabuf-buffer (params id width height format planes modifier)
  (make-resource 'dmabuf-buffer (resource-client params) "wl_buffer" 1 id
                 :width width :height height :format format
                 :modifier modifier :planes planes))

(define-request (params buffer-params :create) (width height format flags)
  (multiple-value-bind (planes modifier) (validate-params params width height format flags)
    (when planes
      ;; Id 0 lets libwayland allocate a server-side id for the new buffer.
      (post-event params :created
                  (make-dmabuf-buffer params 0 width height format planes modifier)))))

(define-request (params buffer-params :create-immed) (id width height format flags)
  (multiple-value-bind (planes modifier) (validate-params params width height format flags)
    (when planes
      (make-dmabuf-buffer params id width height format planes modifier))))
