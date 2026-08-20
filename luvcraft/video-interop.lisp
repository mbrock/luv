;;; Portable ownership boundary between decoded hardware frames and the HAL.
;;;
;;; A decoder configuration and a frame importer belong to the GPU backend.
;;; The resulting picture does not: it is one complete, explicitly owned
;;; cohort of plane textures and their views.  Video screens publish only
;;; complete pictures, so a failed chroma import cannot disturb the luma and
;;; chroma pair which the preceding frame is still displaying.

(in-package #:luvcraft)

(defgeneric video-decode-configuration (device hardware-policy)
  (:documentation
   "Return FFmpeg's hardware selector and configuration for DEVICE.

HARDWARE-POLICY is :AUTO or :REQUIRED.  Two values are returned: the selector
accepted by LIBAV:OPEN-VIDEO and its backend-private configuration.  A device
without a decode bridge returns two NIL values and therefore uses software."))

(defmethod video-decode-configuration (device hardware-policy)
  (declare (ignore device hardware-policy))
  (values nil nil))

(defclass video-frame-importer ()
  ((device :initarg :device :reader video-frame-importer-device)
   (native-owner-retainers
    :initform 0
    :accessor video-frame-importer-native-owner-retainers
    :documentation
    "Number of adopted native owners whose HAL retirement has not finished.")
   (release-state
    :initform :open
    :accessor video-frame-importer-release-state
    :documentation
    "One of :OPEN, :REQUESTED, :RETIRING, :QUEUED, or :RELEASED.")
   (native-retirement-teardown
    :initform nil
    :accessor video-frame-importer-native-retirement-teardown
    :documentation
    "Cached retryable closure transferred when persistent state retires.")
   (lifetime-lock
    :initform (sb-thread:make-mutex :name "video frame importer lifetime")
    :reader video-frame-importer-lifetime-lock))
  (:documentation
   "Backend-owned state for adopting decoded hardware frames into DEVICE.

Every native plane owner retains this object until the HAL has completed its
physical retirement.  Asking to release the importer closes admission at once,
but its backend state remains live until the last such owner callback runs."))

(defgeneric make-video-frame-importer (device)
  (:documentation
   "Return an importer for hardware frames decoded for DEVICE, or NIL."))

(defmethod make-video-frame-importer (device)
  (declare (ignore device))
  nil)

(defgeneric adopt-decoded-video-frame (importer frame width height)
  (:documentation
   "Build and return one complete DECODED-VIDEO-PICTURE.

FRAME remains borrowed from the decoder.  An implementation must retain every
native plane for the lifetime of the returned picture and must release every
part of an incomplete candidate before propagating an error."))

(defgeneric release-video-frame-importer (importer)
  (:documentation
   "Request release of IMPORTER's backend-owned persistent state.

The request is idempotent.  Native state is released only after every adopted
plane owner has completed physical retirement in the HAL."))

(defgeneric release-video-frame-importer-native-state (importer)
  (:documentation
   "Release IMPORTER's backend-native state after its last owner retires."))

(defmethod release-video-frame-importer-native-state
    ((importer video-frame-importer))
  (declare (ignore importer))
  (values))

(defun make-video-frame-importer-native-retirement-teardown (importer)
  "Return one persistent, idempotent native-state teardown for IMPORTER."
  (let ((native-state-released-p nil))
    (lambda ()
      (unless native-state-released-p
        (release-video-frame-importer-native-state importer)
        (setf native-state-released-p t))
      (sb-thread:with-mutex
          ((video-frame-importer-lifetime-lock importer))
        (setf (video-frame-importer-release-state importer) :released
              (video-frame-importer-native-retirement-teardown importer) nil))
      (values))))

(defun maybe-retire-video-frame-importer-native-state (importer)
  "Transfer a ready IMPORTER's persistent native state into HAL retirement."
  (let ((retire-p nil)
        (teardown nil))
    (sb-thread:with-mutex ((video-frame-importer-lifetime-lock importer))
      (when (and (eq :requested
                     (video-frame-importer-release-state importer))
                 (zerop
                  (video-frame-importer-native-owner-retainers importer)))
        (setf (video-frame-importer-release-state importer) :retiring
              teardown
              (or (video-frame-importer-native-retirement-teardown importer)
                  (setf
                   (video-frame-importer-native-retirement-teardown importer)
                   (make-video-frame-importer-native-retirement-teardown
                    importer)))
              retire-p t)))
    (when retire-p
      (let ((transferred-p nil))
        (unwind-protect
             (luv::retire-gpu-native-owner
              (video-frame-importer-device importer)
              importer
              teardown
              (lambda ()
                ;; Queue implementations call this only after their ledger
                ;; durably owns IMPORTER and both closures.
                (setf transferred-p t)
                (sb-thread:with-mutex
                    ((video-frame-importer-lifetime-lock importer))
                  (unless (eq :released
                              (video-frame-importer-release-state importer))
                    (setf (video-frame-importer-release-state importer)
                          :queued)))
                (values)))
          ;; The default/non-live method can fail before transfer.  Preserve a
          ;; caller-owned retry in that case; once invalidated, the HAL ledger
          ;; is the durable owner and :QUEUED must survive every later failure.
          (unless transferred-p
            (sb-thread:with-mutex
                ((video-frame-importer-lifetime-lock importer))
              (when (eq :retiring
                        (video-frame-importer-release-state importer))
                (setf (video-frame-importer-release-state importer)
                      :requested))))))))
  importer)

(defun retain-video-frame-importer-native-owner (importer)
  "Record one native plane owner admitted while IMPORTER is still open."
  (sb-thread:with-mutex ((video-frame-importer-lifetime-lock importer))
    (unless (eq :open (video-frame-importer-release-state importer))
      (error "Cannot adopt a native video plane after importer release was requested."))
    (incf (video-frame-importer-native-owner-retainers importer)))
  importer)

(defun release-video-frame-importer-native-owner (importer)
  "Drop one native owner retainer, without losing a pending release request."
  (sb-thread:with-mutex ((video-frame-importer-lifetime-lock importer))
    (unless (plusp
             (video-frame-importer-native-owner-retainers importer))
      (error "Video frame importer native-owner retainer underflow."))
    (decf (video-frame-importer-native-owner-retainers importer)))
  importer)

(defun make-video-frame-importer-owner-release (importer release-native-owner)
  "Retain IMPORTER and return an idempotent native-owner release callback.

The callback first runs RELEASE-NATIVE-OWNER.  Only its successful completion
drops the importer retainer.  If importer closure was already requested, the
same callback then performs (or retries) that closure.  This ordering lets a
HAL retirement ledger retry any failed native teardown without prematurely
freeing the importer state which minted it."
  (check-type release-native-owner function)
  (retain-video-frame-importer-native-owner importer)
  (let ((native-owner-released-p nil)
        (importer-retainer-released-p nil))
    (lambda ()
      (unless native-owner-released-p
        (funcall release-native-owner)
        (setf native-owner-released-p t))
      (unless importer-retainer-released-p
        (release-video-frame-importer-native-owner importer)
        (setf importer-retainer-released-p t))
      ;; Importer retirement is a separate ledger entry.  A persistent-state
      ;; failure therefore cannot make this plane callback run twice.
      (maybe-retire-video-frame-importer-native-state importer)
      (values))))

(defmethod release-video-frame-importer ((importer null))
  (values))

(defmethod release-video-frame-importer ((importer video-frame-importer))
  (sb-thread:with-mutex ((video-frame-importer-lifetime-lock importer))
    (when (eq :open (video-frame-importer-release-state importer))
      (setf (video-frame-importer-release-state importer) :requested)))
  (maybe-retire-video-frame-importer-native-state importer)
  (values))

(defclass decoded-video-picture ()
  ((textures
    :initarg :textures
    :accessor decoded-video-picture-textures
    :documentation "Plane textures in shader binding order.")
   (views
    :initarg :views
    :accessor decoded-video-picture-views
    :documentation "Views corresponding one-for-one with TEXTURES."))
  (:documentation
   "One atomically publishable and explicitly owned decoded picture."))

(defun decoded-video-picture-view (picture plane)
  "Return PICTURE's view for PLANE, or NIL when it has no such plane."
  (nth plane (decoded-video-picture-views picture)))

(defun decoded-video-picture-released-p (picture)
  "True when PICTURE no longer owns a view or texture."
  (or (null picture)
      (and (null (decoded-video-picture-views picture))
           (null (decoded-video-picture-textures picture)))))

(defun try-release-decoded-video-resource (name resource)
  "Destroy RESOURCE as release step NAME and report whether it succeeded."
  (let ((released-p nil))
    (releasing name
      (destroy resource)
      (setf released-p t))
    released-p))

(defun release-decoded-video-picture (picture)
  "Release PICTURE's views before any of its textures.  Idempotent.

Failed handles remain attached for a later release attempt.  Textures are not
retired at all until every view is gone, preserving the dependency order even
when one view's destruction reports a failure."
  (when picture
    (setf (decoded-video-picture-views picture)
          (delete-if
           (lambda (view)
             (try-release-decoded-video-resource
              :decoded-video-picture-view view))
           (decoded-video-picture-views picture)))
    (unless (decoded-video-picture-views picture)
      (setf (decoded-video-picture-textures picture)
            (delete-if
             (lambda (texture)
               (try-release-decoded-video-resource
                :decoded-video-picture-texture texture))
            (decoded-video-picture-textures picture)))))
  picture)

(defvar *decoded-video-picture-release-backlog* nil
  "Incomplete construction candidates whose logical release needs a retry.")

(defvar *decoded-video-picture-release-backlog-lock*
  (sb-thread:make-mutex :name "decoded video picture release backlog"))

(defun retain-decoded-video-picture-release-backlog (picture)
  "Process-root PICTURE after construction rollback could not release it."
  (sb-thread:with-mutex (*decoded-video-picture-release-backlog-lock*)
    (pushnew picture *decoded-video-picture-release-backlog* :test #'eq))
  picture)

(defun release-decoded-video-picture-or-retain (picture)
  "Release PICTURE, retaining it across an exceptional construction unwind."
  (unwind-protect
       (release-decoded-video-picture picture)
    (unless (decoded-video-picture-released-p picture)
      (retain-decoded-video-picture-release-backlog picture)))
  picture)

(defun retry-decoded-video-picture-release-backlog ()
  "Retry every picture retained by an earlier construction rollback."
  (let ((pictures
          (sb-thread:with-mutex
              (*decoded-video-picture-release-backlog-lock*)
            (prog1 *decoded-video-picture-release-backlog*
              (setf *decoded-video-picture-release-backlog* nil)))))
    (dolist (picture pictures)
      (with-release-warnings
        (release-decoded-video-picture-or-retain picture))))
  (values))

(defun make-decoded-video-picture-from-planes
    (device plane-count make-plane-texture)
  "Build a picture transactionally from PLANE-COUNT native plane textures.

MAKE-PLANE-TEXTURE is called with each zero-based plane index and returns an
owned HAL texture.  This function creates the corresponding views.  If either
step fails, all views built so far are destroyed before every owned texture."
  (check-type plane-count (integer 1 *))
  (retry-decoded-video-picture-release-backlog)
  (let ((textures nil)
        (views nil)
        (completed-p nil))
    (unwind-protect
         (let ((picture
                 (progn
                   (dotimes (plane plane-count)
                     (let ((texture (funcall make-plane-texture plane)))
                       (unless texture
                         (error "Decoded video plane ~D produced no texture."
                                plane))
                       ;; Record the texture before creating its view: a view
                       ;; failure must still retire this newly adopted plane.
                       (push texture textures)
                       (let ((view
                               (create
                                device
                                (make-texture-view-descriptor
                                 :texture texture))))
                         (unless view
                           (error "Decoded video plane ~D produced no view."
                                  plane))
                         (push view views))))
                   (make-instance
                    'decoded-video-picture
                    :textures (reverse textures)
                    :views (reverse views)))))
           (setf completed-p t)
           picture)
      (unless completed-p
        (with-release-warnings
          (let ((candidate
                  (make-instance 'decoded-video-picture
                                 :textures textures :views views)))
            (release-decoded-video-picture-or-retain candidate)))))))
