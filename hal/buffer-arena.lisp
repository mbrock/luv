;;;; Portable GPU buffer arena suballocator built strictly on the LUV HAL.
;;;;
;;;; Manages one growing backing GPU-BUFFER (in :DEVICE-LOCAL or :SHARED
;;;; memory) with element-aligned free-list suballocation, immediate neighbor
;;;; coalescing, and fence-quarantined deferred slice recycling so freed spans
;;;; are never overwritten while still read by in-flight GPU submissions.

(in-package #:luv)

(defstruct (gpu-buffer-slice
             (:constructor %make-gpu-buffer-slice (arena offset count))
             (:copier nil))
  "One owned element range [OFFSET, OFFSET + COUNT) inside a GPU-BUFFER-ARENA."
  (arena nil :read-only t)
  (offset 0 :type (unsigned-byte 64) :read-only t)
  (count 0 :type (unsigned-byte 64) :read-only t)
  (freed-p nil :type boolean))

(defclass gpu-buffer-arena (gpu-object)
  ((device :initarg :device :reader gpu-buffer-arena-device)
   (element-size :initarg :element-size :reader gpu-buffer-arena-element-size)
   (usage :initarg :usage :reader gpu-buffer-arena-usage)
   (memory :initarg :memory :reader gpu-buffer-arena-memory)
   (buffer :initarg :buffer :accessor gpu-buffer-arena-buffer)
   (capacity :initarg :capacity :accessor gpu-buffer-arena-capacity)
   (used-count :initform 0 :accessor gpu-buffer-arena-used-count)
   (generation :initform 0 :accessor gpu-buffer-arena-generation)
   ;; Sorted list of (OFFSET . COUNT) free element spans, coalesced.
   (free-spans :initarg :free-spans :accessor gpu-buffer-arena-free-spans)
   ;; FIFO list of (READY-AFTER OFFSET COUNT) waiting for GPU completion.
   (quarantine :initform nil :accessor gpu-buffer-arena-quarantine)
   ;; Pending growth copies: chronological list of (SRC DST BYTE-SIZE).
   (pending-copies :initform nil :accessor gpu-buffer-arena-pending-copies)
   ;; Pending staged writes: chronological list of (BYTE-OFFSET DATA BYTE-SIZE).
   (pending-writes :initform nil :accessor gpu-buffer-arena-pending-writes)
   ;; Superseded backing buffers to destroy once pending copies are submitted.
   (retired-buffers :initform nil :accessor gpu-buffer-arena-retired-buffers)
   ;; Ephemeral staging/retired buffers encoded onto caller encoders, paired
   ;; with the queue's submitted frontier at encode time: (SUBMITTED . BUFFER).
   (ephemeral-buffers :initform nil :accessor gpu-buffer-arena-ephemeral-buffers)
   (progress-reader :initarg :progress-reader
                    :reader gpu-buffer-arena-progress-reader)
   (destroyed-p :initform nil :accessor gpu-buffer-arena-destroyed-p))
  (:documentation
   "Portable suballocator over one growing GPU-BUFFER."))

(defun gpu-buffer-slice-byte-offset (slice)
  (* (gpu-buffer-slice-offset slice)
     (gpu-buffer-arena-element-size (gpu-buffer-slice-arena slice))))

(defun gpu-buffer-slice-byte-size (slice)
  (* (gpu-buffer-slice-count slice)
     (gpu-buffer-arena-element-size (gpu-buffer-slice-arena slice))))

(defun %ensure-live-arena (arena operation)
  (when (gpu-buffer-arena-destroyed-p arena)
    (error 'gpu-object-destroyed-error :object arena :operation operation)))

(defun %arena-backing-usage (usage)
  (let ((list (if (keywordp usage) (list usage) (copy-list usage))))
    (remove-duplicates (append list '(:copy-src :copy-dst)) :test #'eq)))

(defun %make-arena-backing-buffer (device label capacity element-size usage memory)
  (create device
          (make-buffer-descriptor
           :label label
           :size (max 16 (* capacity element-size))
           :usage (%arena-backing-usage usage)
           :memory memory)))

(defun make-gpu-buffer-arena
    (device &key (label "luv gpu buffer arena")
                 (element-size 16)
                 (initial-capacity 1024)
                 (usage '(:storage))
                 (memory :device-local))
  "Create a portable suballocating arena on DEVICE with ELEMENT-SIZE byte stride."
  (unless (and (integerp element-size) (plusp element-size)
               (zerop (mod element-size 4)))
    (error 'gpu-request-error
           :operation :make-gpu-buffer-arena
           :descriptor element-size
           :reason :invalid-arena-element-size
           :details element-size))
  (unless (and (integerp initial-capacity) (plusp initial-capacity))
    (error 'gpu-request-error
           :operation :make-gpu-buffer-arena
           :descriptor initial-capacity
           :reason :invalid-arena-capacity
           :details initial-capacity))
  (let* ((backing-usage (%arena-backing-usage usage))
         (buffer (%make-arena-backing-buffer
                  device label initial-capacity element-size backing-usage memory))
         (reader (queue-progress-reader (device-queue device))))
    (make-instance 'gpu-buffer-arena
                   :label label
                   :device device
                   :element-size element-size
                   :usage backing-usage
                   :memory memory
                   :buffer buffer
                   :capacity initial-capacity
                   :free-spans (list (cons 0 initial-capacity))
                   :progress-reader reader)))

(defun %arena-insert-free-span (arena offset count)
  "Insert [OFFSET, OFFSET + COUNT) into ARENA's sorted free list, coalescing neighbors."
  (when (zerop count)
    (return-from %arena-insert-free-span))
  (let ((end (+ offset count))
        (before nil)
        (after (gpu-buffer-arena-free-spans arena)))
    ;; Advance past spans strictly before OFFSET.
    (loop while (and after (< (+ (caar after) (cdar after)) offset))
          do (push (pop after) before))
    ;; Coalesce with immediate predecessor if adjacent.
    (when (and after (= (+ (caar after) (cdar after)) offset))
      (let ((prev (pop after)))
        (setf offset (car prev)
              count (+ (cdr prev) count)
              end (+ offset count))))
    ;; Coalesce with immediate successor if adjacent.
    (when (and after (= (caar after) end))
      (let ((next (pop after)))
        (incf count (cdr next))))
    (setf (gpu-buffer-arena-free-spans arena)
          (nreconc before (cons (cons offset count) after)))))

(defun %arena-reap-ephemeral-buffers (arena reader completed)
  "Destroy staging and superseded buffers whose encoding submission has completed."
  (let ((ephemeral (gpu-buffer-arena-ephemeral-buffers arena)))
    (when ephemeral
      (let ((remaining nil))
        (dolist (entry ephemeral)
          (destructuring-bind (submitted-at-encode . buffer) entry
            (if (or (null reader) (> completed submitted-at-encode))
                (destroy buffer)
                (push entry remaining))))
        (setf (gpu-buffer-arena-ephemeral-buffers arena)
              (nreverse remaining))))))

(defun gpu-buffer-arena-reclaim (arena &key (completed nil completed-p))
  "Return GPU-completed quarantined slices to ARENA's coalesced free list.

When COMPLETED is supplied, uses that completion frontier instead of polling
the queue again, so parallel arenas can reclaim in exact lockstep."
  (%ensure-live-arena arena :reclaim)
  (let ((quarantine (gpu-buffer-arena-quarantine arena))
        (ephemeral (gpu-buffer-arena-ephemeral-buffers arena))
        (reclaimed 0))
    (when (or quarantine ephemeral)
      (let* ((reader (gpu-buffer-arena-progress-reader arena))
             (frontier (cond (completed-p completed)
                             (reader (nth-value 1 (funcall reader)))
                             (t nil))))
        (unless (or completed-p reader)
          ;; Backend without non-blocking progress query: fence once cleanly.
          (submitted-work-done (device-queue (gpu-buffer-arena-device arena))))
        (when quarantine
          (let ((remaining nil))
            (dolist (entry quarantine)
              (destructuring-bind (ready-after offset count) entry
                (if (or (null frontier) (<= ready-after frontier))
                    (progn
                      (%arena-insert-free-span arena offset count)
                      (incf reclaimed count))
                    (push entry remaining))))
            (setf (gpu-buffer-arena-quarantine arena) (nreverse remaining))))
        (%arena-reap-ephemeral-buffers arena (and frontier t) (or frontier 0))))
    reclaimed))

(defun %arena-find-and-take-span (arena count)
  "First-fit allocate COUNT elements from ARENA's free list, or return NIL."
  (let ((prev nil)
        (cursor (gpu-buffer-arena-free-spans arena)))
    (loop while cursor
          for span = (car cursor)
          for span-offset = (car span)
          for span-count = (cdr span)
          do (when (>= span-count count)
               (if (= span-count count)
                   (if prev
                       (setf (cdr prev) (cdr cursor))
                       (setf (gpu-buffer-arena-free-spans arena) (cdr cursor)))
                   (setf (car span) (+ span-offset count)
                         (cdr span) (- span-count count)))
               (incf (gpu-buffer-arena-used-count arena) count)
               (return-from %arena-find-and-take-span span-offset))
             (setf prev cursor
                   cursor (cdr cursor))))
  nil)

(defun %arena-grow (arena minimum-extra)
  "Grow ARENA's backing buffer geometrically and queue a GPU copy of live data."
  (let* ((old-capacity (gpu-buffer-arena-capacity arena))
         (new-capacity (max (* 2 old-capacity) (+ old-capacity minimum-extra)))
         (element-size (gpu-buffer-arena-element-size arena))
         (old-buffer (gpu-buffer-arena-buffer arena))
         (new-buffer (%make-arena-backing-buffer
                      (gpu-buffer-arena-device arena)
                      (gpu-object-label arena)
                      new-capacity element-size
                      (gpu-buffer-arena-usage arena)
                      (gpu-buffer-arena-memory arena))))
    ;; Preserve the previous buffer's bytes on GPU during the next flush.
    (when (plusp old-capacity)
      (setf (gpu-buffer-arena-pending-copies arena)
            (nconc (gpu-buffer-arena-pending-copies arena)
                   (list (list old-buffer new-buffer (* old-capacity element-size))))))
    (push old-buffer (gpu-buffer-arena-retired-buffers arena))
    (setf (gpu-buffer-arena-buffer arena) new-buffer
          (gpu-buffer-arena-capacity arena) new-capacity)
    (incf (gpu-buffer-arena-generation arena))
    (%arena-insert-free-span arena old-capacity (- new-capacity old-capacity))
    arena))

(defun gpu-buffer-arena-allocate (arena count &key (reclaim-p t))
  "Allocate an immutable GPU-BUFFER-SLICE of COUNT elements from ARENA."
  (%ensure-live-arena arena :allocate)
  (unless (and (integerp count) (not (minusp count)))
    (error 'gpu-request-error
           :operation :allocate :descriptor count
           :reason :invalid-slice-count :details count))
  (when (zerop count)
    (return-from gpu-buffer-arena-allocate (%make-gpu-buffer-slice arena 0 0)))
  (when reclaim-p
    (gpu-buffer-arena-reclaim arena))
  (let ((offset (or (%arena-find-and-take-span arena count)
                    (progn
                      (%arena-grow arena count)
                      (%arena-find-and-take-span arena count)))))
    (unless offset
      (error "Arena ~S failed to allocate ~D elements after growth." arena count))
    (%make-gpu-buffer-slice arena offset count)))

(defun gpu-buffer-arena-free (arena slice &key (ready-after nil ready-after-p))
  "Quarantine SLICE for reuse once in-flight GPU work finishes."
  (%ensure-live-arena arena :free)
  (unless (and (typep slice 'gpu-buffer-slice)
               (eq arena (gpu-buffer-slice-arena slice)))
    (error 'gpu-request-error
           :operation :free :descriptor slice
           :reason :foreign-buffer-slice :details arena))
  (when (gpu-buffer-slice-freed-p slice)
    (error 'gpu-invalid-state-error
           :object slice :operation :free
           :state :freed :expected-state :live))
  (setf (gpu-buffer-slice-freed-p slice) t)
  (let ((count (gpu-buffer-slice-count slice)))
    (when (plusp count)
      (decf (gpu-buffer-arena-used-count arena) count)
      (let* ((reader (gpu-buffer-arena-progress-reader arena))
             (frontier (cond (ready-after-p ready-after)
                             (reader (nth-value 0 (funcall reader)))
                             (t 0))))
        (setf (gpu-buffer-arena-quarantine arena)
              (nconc (gpu-buffer-arena-quarantine arena)
                     (list (list frontier
                                 (gpu-buffer-slice-offset slice)
                                 count)))))))
  (values))

(defun %copy-numeric-vector (data)
  "Snapshot one-dimensional numeric DATA so later caller mutations cannot race a staged flush."
  (let ((copy (make-array (length data) :element-type (array-element-type data))))
    (replace copy data)
    copy))

(defun gpu-buffer-arena-write (arena slice data &key (byte-offset 0))
  "Write numeric vector DATA into SLICE at BYTE-OFFSET.

On a :SHARED arena with no pending growth copies, writes straight into mapped
memory immediately.  On a :DEVICE-LOCAL arena (or while growth copies are
pending), stages the write until GPU-BUFFER-ARENA-FLUSH."
  (%ensure-live-arena arena :write)
  (unless (and (typep slice 'gpu-buffer-slice)
               (eq arena (gpu-buffer-slice-arena slice)))
    (error 'gpu-request-error
           :operation :write :descriptor slice
           :reason :foreign-buffer-slice :details arena))
  (when (gpu-buffer-slice-freed-p slice)
    (error 'gpu-invalid-state-error
           :object slice :operation :write
           :state :freed :expected-state :live))
  (multiple-value-bind (foreign-type element-size)
      (buffer-data-foreign-type data)
    (unless foreign-type
      (error 'gpu-request-error
             :operation :write :descriptor data
             :reason :unsupported-buffer-data :details data))
    (let ((byte-size (* element-size (length data))))
      (unless (and (typep byte-offset '(unsigned-byte 64))
                   (zerop (mod byte-offset element-size))
                   (<= (+ byte-offset byte-size) (gpu-buffer-slice-byte-size slice)))
        (error 'gpu-request-error
               :operation :write :descriptor slice
               :reason :buffer-write-out-of-bounds
               :details (list :byte-offset byte-offset :byte-size byte-size
                              :slice-bytes (gpu-buffer-slice-byte-size slice))))
      (when (zerop byte-size)
        (return-from gpu-buffer-arena-write slice))
      (let ((destination-offset (+ (gpu-buffer-slice-byte-offset slice) byte-offset)))
        (if (and (eq :shared (gpu-buffer-arena-memory arena))
                 (null (gpu-buffer-arena-pending-copies arena)))
            (write-buffer (gpu-buffer-arena-buffer arena) data
                          :offset destination-offset)
            (setf (gpu-buffer-arena-pending-writes arena)
                  (nconc (gpu-buffer-arena-pending-writes arena)
                         (list (list destination-offset
                                     (%copy-numeric-vector data)
                                     byte-size))))))))
  slice)

(defun %align-up-16 (value)
  (logandc2 (+ value 15) 15))

(defun %encode-arena-flush (arena encoder)
  "Record pending growth copies and batched staging uploads onto ENCODER.

Returns (VALUES FLUSHED-P TRANSIENT-BUFFERS).  TRANSIENT-BUFFERS must remain
live until ENCODER's command buffer has been submitted."
  (let ((copies (gpu-buffer-arena-pending-copies arena))
        (writes (gpu-buffer-arena-pending-writes arena))
        (retired (gpu-buffer-arena-retired-buffers arena))
        (staging nil)
        (completed-p nil))
    (when (and (null copies) (null writes) (null retired))
      (return-from %encode-arena-flush (values nil nil)))
    (unwind-protect
         (progn
           ;; Growth copies run first in chronological order so older live slices
           ;; reach the newest backing buffer before new slice uploads overwrite.
           (dolist (copy copies)
             (destructuring-bind (source destination byte-size) copy
               (copy-buffer-to-buffer encoder source destination byte-size)))
           (when writes
             (let ((total-bytes 0)
                   (plan nil))
               (dolist (entry writes)
                 (destructuring-bind (dest-offset data byte-size) entry
                   (let ((stage-offset (%align-up-16 total-bytes)))
                     (push (list stage-offset dest-offset data byte-size) plan)
                     (setf total-bytes (+ stage-offset byte-size)))))
               (setf plan (nreverse plan)
                     staging (create (gpu-buffer-arena-device arena)
                                     (make-buffer-descriptor
                                      :label (format nil "~A staging"
                                                     (or (gpu-object-label arena) "arena"))
                                      :size (max 16 total-bytes)
                                      :usage '(:copy-src)
                                      :memory :shared)))
               (dolist (step plan)
                 (destructuring-bind (stage-offset dest-offset data byte-size) step
                   (write-buffer staging data :offset stage-offset)
                   (copy-buffer-to-buffer
                    encoder staging (gpu-buffer-arena-buffer arena) byte-size
                    :source-offset stage-offset
                    :destination-offset dest-offset)))))
           (setf (gpu-buffer-arena-pending-copies arena) nil
                 (gpu-buffer-arena-pending-writes arena) nil
                 (gpu-buffer-arena-retired-buffers arena) nil
                 completed-p t)
           (values t (if staging (cons staging retired) retired)))
      (when (and staging (not completed-p))
        (destroy staging)))))

(defun gpu-buffer-arena-flush (arena &optional encoder)
  "Flush pending growth copies and staged slice writes for ARENA.

When ENCODER is provided, records copies directly onto ENCODER without an
extra submission; transient staging and superseded buffers remain live until
a later GPU-BUFFER-ARENA-RECLAIM (or DESTROY) observes completion.  When
ENCODER is NIL, records and submits a one-shot transfer command buffer and
releases transient buffers immediately after submission."
  (%ensure-live-arena arena :flush)
  (when (and (null (gpu-buffer-arena-pending-copies arena))
             (null (gpu-buffer-arena-pending-writes arena))
             (null (gpu-buffer-arena-retired-buffers arena)))
    (return-from gpu-buffer-arena-flush nil))
  (if encoder
      (multiple-value-bind (flushed-p transient)
          (%encode-arena-flush arena encoder)
        (when transient
          (let* ((reader (gpu-buffer-arena-progress-reader arena))
                 (submitted (if reader (nth-value 0 (funcall reader)) 0)))
            (dolist (buffer transient)
              (push (cons submitted buffer)
                    (gpu-buffer-arena-ephemeral-buffers arena)))))
        flushed-p)
      (let* ((device (gpu-buffer-arena-device arena))
             (queue (device-queue device))
             (local-encoder nil)
             (commands nil)
             (transient nil))
        (unwind-protect
             (progn
               (setf local-encoder (create device (make-command-encoder-descriptor)))
               (multiple-value-bind (flushed-p buffers)
                   (%encode-arena-flush arena local-encoder)
                 (declare (ignore flushed-p))
                 (setf transient buffers))
               (setf commands (finish local-encoder))
               (submit queue commands)
               t)
          (dolist (buffer transient)
            (when buffer (destroy buffer)))
          (when commands (destroy commands))
          (when local-encoder (destroy local-encoder))))))

(defun gpu-buffer-arena-stats (arena)
  "Return an inspectable plist of ARENA's capacity, usage, fragmentation, and quarantine."
  (let* ((free-spans (gpu-buffer-arena-free-spans arena))
         (free-count (reduce #'+ free-spans :key #'cdr :initial-value 0))
         (quarantine (gpu-buffer-arena-quarantine arena))
         (quarantine-count (reduce #'+ quarantine :key #'third :initial-value 0))
         (element-size (gpu-buffer-arena-element-size arena))
         (capacity (gpu-buffer-arena-capacity arena)))
    (list :capacity capacity
          :element-size element-size
          :bytes (* capacity element-size)
          :used (gpu-buffer-arena-used-count arena)
          :free free-count
          :free-span-count (length free-spans)
          :quarantined quarantine-count
          :quarantine-entry-count (length quarantine)
          :generation (gpu-buffer-arena-generation arena)
          :memory (gpu-buffer-arena-memory arena))))

(defmethod destroy ((arena gpu-buffer-arena))
  (setf (gpu-buffer-arena-destroyed-p arena) t
        (gpu-buffer-arena-pending-copies arena) nil
        (gpu-buffer-arena-pending-writes arena) nil
        (gpu-buffer-arena-free-spans arena) nil
        (gpu-buffer-arena-quarantine arena) nil)
  ;; Retain any buffer whose destruction failed so a later DESTROY retries
  ;; only the failed handle without double-destroying siblings.
  (with-release-report
    (let ((retained-retired nil))
      (dolist (old (gpu-buffer-arena-retired-buffers arena))
        (when old
          (let ((released-p nil))
            (releasing :arena-retired-buffer
              (destroy old)
              (setf released-p t))
            (unless released-p (push old retained-retired)))))
      (setf (gpu-buffer-arena-retired-buffers arena) (nreverse retained-retired)))
    (let ((retained-ephemeral nil))
      (dolist (entry (gpu-buffer-arena-ephemeral-buffers arena))
        (if (cdr entry)
            (let ((released-p nil))
              (releasing :arena-ephemeral-buffer
                (destroy (cdr entry))
                (setf released-p t))
              (unless released-p (push entry retained-ephemeral)))
            nil))
      (setf (gpu-buffer-arena-ephemeral-buffers arena) (nreverse retained-ephemeral)))
    (let ((buffer (gpu-buffer-arena-buffer arena)))
      (when buffer
        (releasing :arena-backing-buffer
          (destroy buffer)
          (setf (gpu-buffer-arena-buffer arena) nil)))))
  (values))
