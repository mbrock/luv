;;; Durable descriptions and asynchronous checkpoints for the little world.
;;;
;;; A save contains the procedural source and sparse authored edits.  Resident
;;; chunks and their derived products are deliberately absent.  The render
;;; owner captures an immutable description; a dedicated latest-value worker
;;; performs printing and atomic replacement without file I/O in a frame.

(in-package #:luvcraft)

(defconstant +luvcraft-save-format-version+ 1)
(defconstant +little-world-source-version+ 1)

(define-condition invalid-luvcraft-save (error)
  ((reason :initarg :reason :reader invalid-luvcraft-save-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid luvcraft save: ~A"
                     (invalid-luvcraft-save-reason condition)))))

(defun invalid-luvcraft-save (control &rest arguments)
  (error 'invalid-luvcraft-save
         :reason (apply #'format nil control arguments)))

(defun validate-description-plist (plist context)
  (unless (listp plist)
    (invalid-luvcraft-save "~A must be a property list, not ~S."
                           context plist))
  (unless (evenp (length plist))
    (invalid-luvcraft-save "~A has an odd property list: ~S."
                           context plist))
  plist)

(defun description-value (plist key context &key optional default)
  (validate-description-plist plist context)
  (let ((missing (gensym "MISSING-")))
    (let ((value (getf plist key missing)))
      (if (eq value missing)
          (if optional
              default
              (invalid-luvcraft-save "~A has no ~S field." context key))
          value))))

(defun tagged-description-values (description context)
  (unless (and (consp description) (keywordp (first description)))
    (invalid-luvcraft-save "~A needs a keyword kind, not ~S."
                           context description))
  (values (first description)
          (validate-description-plist (rest description) context)))

(defgeneric block-save-description (block)
  (:documentation
   "Return the portable value description stored for BLOCK in a world save."))

(defgeneric restore-block-save-description (kind description)
  (:documentation
   "Resolve a portable block value with keyword KIND and property DESCRIPTION."))

(defmethod block-save-description ((block null))
  '(:air))

(defmethod block-save-description ((block block-kind))
  (list :block :name (block-kind-name block)))

(defmethod restore-block-save-description ((kind (eql :air)) description)
  (unless (null description)
    (invalid-luvcraft-save "An :AIR value has unexpected fields ~S."
                           description))
  nil)

(defmethod restore-block-save-description ((kind (eql :block)) description)
  (let ((name (description-value description :name "block value")))
    (unless (keywordp name)
      (invalid-luvcraft-save "A block name must be a keyword, not ~S." name))
    (or (block-kind-named name nil)
        (invalid-luvcraft-save "No block kind is named ~S." name))))

(defmethod restore-block-save-description ((kind t) description)
  (declare (ignore description))
  (invalid-luvcraft-save "No block value reader handles ~S." kind))

(defun restore-block-value (description)
  (multiple-value-bind (kind fields)
      (tagged-description-values description "block value")
    (restore-block-save-description kind fields)))

(defun coordinate-key< (left right)
  (or (< (first left) (first right))
      (and (= (first left) (first right))
           (or (< (second left) (second right))
               (and (= (second left) (second right))
                    (< (third left) (third right)))))))

(defun block-edit-overlay-save-descriptions (overlay)
  "Return OVERLAY's current values in deterministic world-coordinate order."
  (check-type overlay block-edit-overlay)
  (sort
   (loop for coordinate being the hash-keys
           of (block-edit-overlay-entries overlay)
         using (hash-value block)
         collect (list :at (copy-list coordinate)
                       :value (block-save-description block)))
   #'coordinate-key< :key (lambda (edit) (getf edit :at))))

(defun restore-world-edit-block-value (description)
  "Restore one authored edit, migrating the retired gnome block to air.

Gnomes became embodied agents rather than terrain after save format version 1
had already written them as block edits.  This compatibility belongs at the
world-edit boundary: a legacy gnome occupied a cell, so removing that obsolete
occupant means an explicit air edit.  Other uses of block descriptions remain
strict; in particular a selected or carried block cannot silently become air."
  (multiple-value-bind (kind fields)
      (tagged-description-values description "block value")
    (if (and (eq kind :block)
             (eq (description-value fields :name "block value") :gnome))
        nil
        (restore-block-save-description kind fields))))

(defun restore-block-edit-overlay (descriptions)
  (unless (listp descriptions)
    (invalid-luvcraft-save "World edits must be a list, not ~S." descriptions))
  (let ((overlay (make-block-edit-overlay)))
    (dolist (description descriptions overlay)
      (let* ((coordinate
               (description-value description :at "world edit"))
             (value
               (description-value description :value "world edit")))
        (unless (and (listp coordinate)
                     (= (length coordinate) 3)
                     (every #'integerp coordinate))
          (invalid-luvcraft-save
           "A world edit coordinate must contain three integers, not ~S."
           coordinate))
        (destructuring-bind (x y z) coordinate
          (record-block-edit overlay
                             (restore-world-edit-block-value value)
                             x y z))))))

(defgeneric world-source-save-description (source)
  (:documentation
   "Return SOURCE as portable semantic data, excluding resident chunks."))

(defgeneric restore-world-source-save-description (kind description)
  (:documentation
   "Restore a world source named by keyword KIND from DESCRIPTION."))

(defmethod world-source-save-description ((source little-world-source))
  (list :little-world
        :source-version +little-world-source-version+
        :seed (little-world-source-seed source)
        :relief (little-world-source-relief source)
        :edits (block-edit-overlay-save-descriptions
                (little-world-source-edits source))))

(defmethod restore-world-source-save-description
    ((kind (eql :little-world)) description)
  (let ((version
          (description-value description :source-version
                             "little-world source"))
        (seed (description-value description :seed "little-world source"))
        ;; Saves written before reliefs existed were meadows; reading them as
        ;; anything else would move the ground under their edits.
        (relief (description-value description :relief "little-world source"
                                   :optional t :default :meadow))
        (edits (description-value description :edits "little-world source")))
    (unless (eql version +little-world-source-version+)
      (invalid-luvcraft-save
       "Little-world source version ~S is unsupported; expected ~D."
       version +little-world-source-version+))
    (unless (integerp seed)
      (invalid-luvcraft-save "A little-world seed must be an integer, not ~S."
                             seed))
    (unless (member relief '(:meadow :alpine))
      (invalid-luvcraft-save
       "A little-world relief must be :MEADOW or :ALPINE, not ~S." relief))
    (make-instance 'little-world-source
                   :seed seed
                   :relief relief
                   :edits (restore-block-edit-overlay edits))))

(defmethod restore-world-source-save-description ((kind t) description)
  (declare (ignore description))
  (invalid-luvcraft-save "No world source reader handles ~S." kind))

(defgeneric world-save-description (world)
  (:documentation
   "Return WORLD as a portable description without resident materializations."))

(defgeneric restore-world-save-description (kind description)
  (:documentation
   "Restore a world named by keyword KIND from DESCRIPTION."))

(defmethod world-save-description ((world block-world))
  (let* ((space (block-world-space world))
         (shape (voxel-space-chunk-shape space)))
    (list :block-world
          :space
          (list :chunk-shape
                (list (chunk-shape-width shape)
                      (chunk-shape-height shape)
                      (chunk-shape-depth shape))
                :cell-extent (vec3-list (voxel-space-cell-extent space)))
          :source (world-source-save-description
                   (block-world-source world)))))

(defmethod restore-world-save-description
    ((kind (eql :block-world)) description)
  (let* ((space (description-value description :space "block world"))
         (shape (description-value space :chunk-shape "voxel space"))
         (extent (description-value space :cell-extent "voxel space"))
         (source-description
           (description-value description :source "block world")))
    (unless (and (listp shape) (= (length shape) 3)
                 (every (lambda (dimension)
                          (typep dimension '(integer 1)))
                        shape))
      (invalid-luvcraft-save
       "A chunk shape must contain three positive integers, not ~S." shape))
    (unless (and (listp extent) (= (length extent) 3)
                 (every (lambda (component)
                          (and (realp component) (plusp component)))
                        extent))
      (invalid-luvcraft-save
       "A cell extent must contain three positive reals, not ~S." extent))
    (multiple-value-bind (source-kind source-fields)
        (tagged-description-values source-description "world source")
      (let ((source
              (restore-world-source-save-description source-kind source-fields)))
        (destructuring-bind (width height depth) shape
          (make-block-world
           :id (list :saved-world source-kind)
           :chunk-width width :chunk-height height :chunk-depth depth
           :cell-extent extent
           :source source))))))

(defmethod restore-world-save-description ((kind t) description)
  (declare (ignore description))
  (invalid-luvcraft-save "No world reader handles ~S." kind))

(defun luvcraft-resume-save-description
    (camera player selected-block &optional carried)
  "CARRIED are the blocks the player holds that are worth writing down:
per-instance ones such as films, which no palette would give back."
  (when (and camera player)
    (list :player-position
          (vec3-list (player-position player))
          :look (list :yaw (camera-yaw camera) :pitch (camera-pitch camera))
          :selected-block (block-save-description selected-block)
          :carried (mapcar #'block-save-description carried))))

(defun make-luvcraft-save-description
    (world &key camera player selected-block carried)
  "Capture one immutable, printable checkpoint description.  See #TR2JNQ."
  (list :luvcraft-world
        :format-version +luvcraft-save-format-version+
        :world (world-save-description world)
        :resume (luvcraft-resume-save-description
                 camera player selected-block carried)))

(defun restore-luvcraft-resume-save-description (description)
  "Return CAMERA, PLAYER, selected block, and carried blocks restored from
DESCRIPTION."
  (if (null description)
      (values (make-instance 'fly-camera) nil *stone-block* nil)
      (let* ((position
               (description-value description :player-position
                                  "resume state"))
             (look (description-value description :look "resume state"))
             (yaw (description-value look :yaw "saved look"))
             (pitch (description-value look :pitch "saved look"))
             (selected
               (description-value description :selected-block
                                  "resume state"))
             (carried
               (description-value description :carried "resume state"
                                  :optional t :default nil)))
        (unless (listp carried)
          (invalid-luvcraft-save
           "Carried blocks must be a list, not ~S." carried))
        (unless (and (listp position) (= (length position) 3)
                     (every #'realp position))
          (invalid-luvcraft-save
           "A player position must contain three reals, not ~S." position))
        (unless (and (realp yaw) (realp pitch))
          (invalid-luvcraft-save
           "Saved yaw and pitch must be real, not ~S and ~S." yaw pitch))
        (destructuring-bind (x y z) position
          (values
           (make-instance 'fly-camera :yaw yaw :pitch pitch)
           (make-instance 'block-world-player
                          :position
                          (make-vec3 (coerce x 'double-float)
                                     (coerce y 'double-float)
                                     (coerce z 'double-float)))
           (let ((block (restore-block-value selected)))
             (unless (typep block 'block-kind)
               (invalid-luvcraft-save
                "The selected value must name a block kind, not ~S." selected))
             block)
           (mapcar #'restore-block-value carried))))))

(defun restore-luvcraft-save-description (description)
  "Return the world and resume description represented by DESCRIPTION."
  (multiple-value-bind (kind fields)
      (tagged-description-values description "luvcraft save")
    (unless (eq kind :luvcraft-world)
      (invalid-luvcraft-save "Expected :LUVCRAFT-WORLD, not ~S." kind))
    (let ((version
            (description-value fields :format-version "luvcraft save"))
          (world-description
            (description-value fields :world "luvcraft save"))
          (resume-description
            (description-value fields :resume "luvcraft save"
                               :optional t :default nil)))
      (unless (eql version +luvcraft-save-format-version+)
        (invalid-luvcraft-save
         "Format version ~S is unsupported; expected ~D."
         version +luvcraft-save-format-version+))
      (multiple-value-bind (world-kind world-fields)
          (tagged-description-values world-description "saved world")
        (values (restore-world-save-description world-kind world-fields)
                resume-description)))))

(defun read-luvcraft-save (pathname)
  "Read and validate one luvcraft save from PATHNAME.

Return the restored world and its still-portable resume description."
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((*read-eval* nil)
          (*readtable* (copy-readtable nil))
          (eof (gensym "EOF-")))
      (let ((description (read stream nil eof)))
        (when (eq description eof)
          (invalid-luvcraft-save "~A is empty." pathname))
        (unless (eq (read stream nil eof) eof)
          (invalid-luvcraft-save "~A contains more than one form." pathname))
        (restore-luvcraft-save-description description)))))

(defun checkpoint-temporary-pathname (pathname)
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (make-pathname :name (format nil "~A~@[.~A~]" name type)
                   :type "new"
                   :defaults pathname)))

(defun write-luvcraft-save-description (description pathname)
  "Atomically replace PATHNAME with the printable save DESCRIPTION."
  (let* ((target (pathname pathname))
         (temporary (checkpoint-temporary-pathname target)))
    (ensure-directories-exist target)
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (let ((*print-readably* t)
                   (*print-pretty* t)
                   (*print-circle* nil))
               (write description :stream stream :pretty t)
               (terpri stream)
               (finish-output stream)))
           (uiop:rename-file-overwriting-target temporary target))
      (when (probe-file temporary)
        (ignore-errors (delete-file temporary)))))
  pathname)

(defclass world-checkpoint-writer ()
  ((pathname :initarg :pathname :reader world-checkpoint-writer-pathname)
   (mailbox :reader world-checkpoint-writer-mailbox)
   (lock :reader world-checkpoint-writer-lock)
   (pending :initform nil :accessor world-checkpoint-writer-pending)
   (pending-p :initform nil :accessor world-checkpoint-writer-pending-p)
   (active-p :initform nil :accessor world-checkpoint-writer-active-p)
   (wake-p :initform nil :accessor world-checkpoint-writer-wake-p)
   (running-p :initform t :accessor world-checkpoint-writer-running-p)
   (next-ticket :initform 0 :accessor world-checkpoint-writer-next-ticket)
   (completed-ticket :initform 0
                     :accessor world-checkpoint-writer-completed-ticket)
   (condition :initform nil :accessor world-checkpoint-writer-condition)
   (thread :initform nil :accessor world-checkpoint-writer-thread)))

(defgeneric perform-world-checkpoint (writer description)
  (:documentation
   "Persist immutable DESCRIPTION on WRITER's worker thread."))

(defmethod perform-world-checkpoint
    ((writer world-checkpoint-writer) description)
  (write-luvcraft-save-description
   description (world-checkpoint-writer-pathname writer)))

(defun take-world-checkpoint (writer)
  (sb-thread:with-mutex ((world-checkpoint-writer-lock writer))
    (setf (world-checkpoint-writer-wake-p writer) nil)
    (when (world-checkpoint-writer-pending-p writer)
      (destructuring-bind (ticket description)
          (world-checkpoint-writer-pending writer)
        (setf (world-checkpoint-writer-pending writer) nil
              (world-checkpoint-writer-pending-p writer) nil
              (world-checkpoint-writer-active-p writer) t)
        (values ticket description t)))))

(defun finish-world-checkpoint (writer ticket condition)
  (sb-thread:with-mutex ((world-checkpoint-writer-lock writer))
    (setf (world-checkpoint-writer-active-p writer) nil
          (world-checkpoint-writer-completed-ticket writer) ticket
          ;; Retain an earlier failure until shutdown reports it.  A later
          ;; successful coalesced checkpoint must not make lost durability
          ;; look like an entirely successful writer lifetime.
          (world-checkpoint-writer-condition writer)
          (or condition (world-checkpoint-writer-condition writer)))))

(defun run-world-checkpoint-writer (writer)
  (loop
    (multiple-value-bind (message received-p)
        (sb-concurrency:receive-message
         (world-checkpoint-writer-mailbox writer))
      (declare (ignore received-p))
      (ecase message
        (:stop (return))
        (:work
         (loop
           (multiple-value-bind (ticket description present-p)
               (take-world-checkpoint writer)
             (unless present-p (return))
             (let ((condition nil))
               (handler-case
                   (perform-world-checkpoint writer description)
                 (error (caught)
                   (setf condition caught)
                   (format *error-output* "luvcraft save: ~A~%" caught)
                   (finish-output *error-output*)))
               (finish-world-checkpoint writer ticket condition)))))))))

(defun make-world-checkpoint-writer (pathname)
  "Make a sleeping latest-value worker for atomic world checkpoints."
  (let ((writer
          (make-instance 'world-checkpoint-writer
                         :pathname (pathname pathname))))
    (setf (slot-value writer 'mailbox)
          (sb-concurrency:make-mailbox :name "luvcraft checkpoint requests")
          (slot-value writer 'lock)
          (sb-thread:make-mutex :name "luvcraft checkpoint state")
          (world-checkpoint-writer-thread writer)
          (sb-thread:make-thread
           (lambda () (run-world-checkpoint-writer writer))
           :name "luvcraft checkpoint writer"))
    writer))

(defun request-world-checkpoint (writer description)
  "Submit immutable DESCRIPTION without waiting for filesystem I/O.

Only the latest checkpoint which has not begun is retained.  Return its
monotonic ticket."
  (check-type writer world-checkpoint-writer)
  (let ((wake-p nil)
        (ticket nil))
    (sb-thread:with-mutex ((world-checkpoint-writer-lock writer))
      (unless (world-checkpoint-writer-running-p writer)
        (error "Checkpoint writer for ~A has stopped."
               (world-checkpoint-writer-pathname writer)))
      (setf ticket (incf (world-checkpoint-writer-next-ticket writer))
            (world-checkpoint-writer-pending writer)
            (list ticket description)
            (world-checkpoint-writer-pending-p writer) t)
      (unless (or (world-checkpoint-writer-active-p writer)
                  (world-checkpoint-writer-wake-p writer))
        (setf (world-checkpoint-writer-wake-p writer) t
              wake-p t)))
    (when wake-p
      (sb-concurrency:send-message
       (world-checkpoint-writer-mailbox writer) :work))
    ticket))

(defun stop-world-checkpoint-writer (writer &key (timeout 10.0))
  "Flush the latest requested checkpoint, stop WRITER, and join its thread."
  (check-type writer world-checkpoint-writer)
  (let ((stop-p nil)
        (requested-ticket nil))
    (sb-thread:with-mutex ((world-checkpoint-writer-lock writer))
      (setf requested-ticket (world-checkpoint-writer-next-ticket writer))
      (when (world-checkpoint-writer-running-p writer)
        (setf (world-checkpoint-writer-running-p writer) nil
              stop-p t)))
    (when stop-p
      (sb-concurrency:send-message
       (world-checkpoint-writer-mailbox writer) :stop)
      (multiple-value-bind (value state)
          (sb-thread:join-thread (world-checkpoint-writer-thread writer)
                                 :timeout timeout :default :timeout)
        (declare (ignore value))
        (when (eq state :timeout)
          (error "Checkpoint writer for ~A did not stop within ~,2F seconds."
                 (world-checkpoint-writer-pathname writer) timeout))))
    (unless (= requested-ticket
               (world-checkpoint-writer-completed-ticket writer))
      (error "Checkpoint writer for ~A stopped before ticket ~D was written."
             (world-checkpoint-writer-pathname writer) requested-ticket))
    (let ((condition (world-checkpoint-writer-condition writer)))
      (when condition (error condition))))
  nil)
